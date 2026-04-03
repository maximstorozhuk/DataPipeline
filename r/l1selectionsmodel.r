# Feature set: see l1regularization_coefficients.csv — "lambda.min" (default),
# "lambda.path.min" (most predictors), "lambda.1se" (sparsest).
FEATURE_LAMBDA <- "lambda.min"
AUTO_LENIENT <- TRUE
MIN_FEATURES_PREFERRED <- 3L

STAKE <- 10
MIN_TRAIN_ROWS_BASE <- 80L
EDGE_THRESHOLDS <- seq(0, 0.15, by = 0.01)

csv_candidates <- c(
  file.path("csv", "nba_2008-2025_extended.csv"),
  file.path("..", "csv", "nba_2008-2025_extended.csv")
)
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) {
  stop("Cannot find nba_2008-2025_extended.csv (expected csv/ under the project root).")
}

repo_root <- dirname(dirname(normalizePath(csv_path)))
coef_path <- file.path(repo_root, "r", "output", "l1regularization", "l1regularization_coefficients.csv")
if (!file.exists(coef_path)) {
  stop(
    "Missing ", coef_path, " — run r/l1regularization.r first.",
    call. = FALSE
  )
}

coef_tab <- utils::read.csv(coef_path, stringsAsFactors = FALSE)

pick_nonzero <- function(tab, lab) {
  tab[
    tab$lambda == lab &
      tab$feature != "(Intercept)" &
      abs(tab$coefficient) > 1e-8,
  ]
}

sel <- pick_nonzero(coef_tab, FEATURE_LAMBDA)
used_lab <- FEATURE_LAMBDA

if (AUTO_LENIENT && nrow(sel) < MIN_FEATURES_PREFERRED) {
  sel_path <- pick_nonzero(coef_tab, "lambda.path.min")
  if (nrow(sel_path) > nrow(sel)) {
    sel <- sel_path
    used_lab <- "lambda.path.min"
    cat(
      "Note: primary feature set had < ", MIN_FEATURES_PREFERRED,
      " predictors; using lambda.path.min (n = ", nrow(sel), ").\n",
      sep = ""
    )
  }
}

if (nrow(sel) == 0L) {
  for (lab in c("lambda.path.min", "lambda.min", "lambda.1se")) {
    sel <- pick_nonzero(coef_tab, lab)
    if (nrow(sel) > 0L) {
      used_lab <- lab
      cat("Note: using ", lab, " features (fallback).\n", sep = "")
      break
    }
  }
}

features <- unique(sel$feature)
cat(
  "Loading CSV; features from ", used_lab, " (n = ", length(features), ")...\n",
  sep = ""
)
flush.console()

nba <- read.csv(
  csv_path,
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE
)

for (nm in names(nba)) {
  x <- nba[[nm]]
  if (!is.character(x)) next
  u <- unique(stats::na.omit(x))
  if (length(u) == 0) next
  if (all(tolower(u) %in% c("true", "false"))) {
    nba[[nm]] <- ifelse(is.na(x), NA, tolower(x) == "true")
  }
}

tie <- nba$score_home == nba$score_away
nba <- nba[!tie, , drop = FALSE]
nba$home_win <- as.integer(nba$score_home > nba$score_away)

nba$date_num <- suppressWarnings(as.numeric(as.Date(nba$date)))

features <- intersect(features, names(nba))
miss <- setdiff(unique(sel$feature), names(nba))
if (length(miss)) {
  cat("Dropping features not in data:", paste(miss, collapse = ", "), "\n")
}
if (length(features) < 1L) {
  stop("No L1-selected features found in dataset.", call. = FALSE)
}

implied_prob_ml <- function(ml) {
  if (is.na(ml)) {
    return(NA_real_)
  }
  if (ml < 0) {
    return(abs(ml) / (abs(ml) + 100))
  }
  100 / (ml + 100)
}

profit_bet <- function(odds, won) {
  if (is.na(odds)) {
    return(NA_real_)
  }
  if (won) {
    if (odds < 0) {
      return(STAKE * (100 / abs(odds)))
    }
    return(STAKE * (odds / 100))
  }
  -STAKE
}

safe_predict <- function(mod, newdata) {
  tryCatch(
    stats::predict(mod, newdata = newdata, type = "response"),
    error = function(e) rep(NA_real_, nrow(newdata))
  )
}

glm_formula <- function(feats) {
  rhs <- paste(
    paste0("`", gsub("`", "``", feats, fixed = TRUE), "`"),
    collapse = " + "
  )
  stats::as.formula(paste("home_win ~", rhs))
}

min_train <- max(MIN_TRAIN_ROWS_BASE, 10L * length(features))
seasons <- sort(unique(nba$season))
n_k <- length(EDGE_THRESHOLDS)
total_profit <- numeric(n_k)
n_bets <- integer(n_k)
names(total_profit) <- names(n_bets) <- sprintf("%.2f", EDGE_THRESHOLDS)

if (length(seasons) < 2L) {
  stop("Need at least two seasons.", call. = FALSE)
}

for (test_season in seasons[-1L]) {
  tr_idx <- nba$season < test_season
  te_idx <- nba$season == test_season
  train <- nba[tr_idx, , drop = FALSE]
  test <- nba[te_idx, , drop = FALSE]
  if (nrow(train) < min_train || nrow(test) < 1L) {
    next
  }

  d_tr <- train[, c("home_win", features), drop = FALSE]
  d_tr <- stats::na.omit(d_tr)
  if (nrow(d_tr) < min_train) {
    next
  }

  mod <- tryCatch(
    suppressWarnings(
      stats::glm(
        glm_formula(features),
        data = d_tr,
        family = stats::binomial(),
        control = stats::glm.control(maxit = 100, epsilon = 1e-6)
      )
    ),
    error = function(e) NULL
  )
  if (is.null(mod)) {
    next
  }

  nd <- test[, features, drop = FALSE]
  p_hat <- safe_predict(mod, nd)
  if (length(p_hat) != nrow(test)) {
    next
  }

  for (i in seq_len(nrow(test))) {
    ph <- p_hat[i]
    if (is.na(ph)) {
      next
    }
    mlh <- test$moneyline_home[i]
    mla <- test$moneyline_away[i]
    ih <- implied_prob_ml(mlh)
    ia <- implied_prob_ml(mla)
    edge_h <- ph - ih
    edge_a <- (1 - ph) - ia
    sa <- test$score_away[i]
    sh <- test$score_home[i]
    home_won <- sh > sa
    away_won <- sa > sh

    for (ki in seq_len(n_k)) {
      k <- EDGE_THRESHOLDS[ki]
      take_h <- edge_h > k && !is.na(mlh)
      take_a <- edge_a > k && !is.na(mla)
      if (take_h && take_a) {
        if (edge_h >= edge_a) {
          total_profit[ki] <- total_profit[ki] + profit_bet(mlh, home_won)
          n_bets[ki] <- n_bets[ki] + 1L
        } else {
          total_profit[ki] <- total_profit[ki] + profit_bet(mla, away_won)
          n_bets[ki] <- n_bets[ki] + 1L
        }
      } else if (take_h) {
        total_profit[ki] <- total_profit[ki] + profit_bet(mlh, home_won)
        n_bets[ki] <- n_bets[ki] + 1L
      } else if (take_a) {
        total_profit[ki] <- total_profit[ki] + profit_bet(mla, away_won)
        n_bets[ki] <- n_bets[ki] + 1L
      }
    }
  }
}

results <- data.frame(
  min_edge = EDGE_THRESHOLDS,
  rule = sprintf("P(side) > implied + %.2f", EDGE_THRESHOLDS),
  total_profit = round(total_profit, 2),
  n_bets = n_bets,
  stringsAsFactors = FALSE
)

out_dir <- file.path(repo_root, "r", "output", "l1selectionsmodel")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "l1selectionsmodel_thresholds.csv")
out_png <- file.path(out_dir, "l1selectionsmodel_thresholds.png")
feat_txt <- file.path(out_dir, "l1selectionsmodel_features.txt")

write.csv(results, out_csv, row.names = FALSE)
writeLines(
  c(
    paste0("Feature lambda used: ", used_lab),
    paste(features, collapse = ", ")
  ),
  feat_txt
)

if (requireNamespace("gridExtra", quietly = TRUE)) {
  grDevices::png(
    out_png,
    width = 11,
    height = min(14, max(5, nrow(results) * 0.35 + 2)),
    units = "in",
    res = 120
  )
  grid::grid.newpage()
  gridExtra::grid.table(
    results,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(base_size = 9)
  )
  grDevices::dev.off()
} else {
  warning("gridExtra not installed; PNG skipped.", call. = FALSE)
}

cat("\nFeatures used (", length(features), "): ", paste(features, collapse = ", "), "\n\n", sep = "")
print(results, row.names = FALSE)
cat("\nSaved:", normalizePath(out_csv), "\n")
cat("Saved:", normalizePath(feat_txt), "\n")
if (file.exists(out_png)) {
  cat("Saved:", normalizePath(out_png), "\n")
}

invisible(list(results = results, features = features))
