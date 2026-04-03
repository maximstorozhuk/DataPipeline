STAKE <- 10
MIN_TRAIN_ROWS <- 80L
MAX_DISTINCT_FOR_GLM <- 100L

csv_candidates <- c(
  file.path("csv", "nba_2008-2025_extended.csv"),
  file.path("..", "csv", "nba_2008-2025_extended.csv")
)
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) {
  stop("Cannot find nba_2008-2025_extended.csv (expected csv/ under the project root).")
}

cat("Loading CSV...\n")
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

leaky <- c(
  "score_away",
  "score_home",
  "home_win",
  grep("^q[1-4]_", names(nba), value = TRUE),
  grep("^ot_", names(nba), value = TRUE)
)

nba$date_num <- suppressWarnings(as.numeric(as.Date(nba$date)))

# Not pre-game / not at tipoff: in-game or post-open lines
# Not using market-implied columns as predictors (trivial vs the betting rule)
exclude_extra <- c(
  "date",
  "h2_spread",
  "id_spread",
  grep("^implied_odds", names(nba), value = TRUE),
  grep("^profit_", names(nba), value = TRUE)
)
pred_names <- setdiff(names(nba), c(leaky, exclude_extra))

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

safe_glm_predict <- function(mod, newdata) {
  tryCatch(
    stats::predict(mod, newdata = newdata, type = "response"),
    error = function(e) rep(NA_real_, nrow(newdata))
  )
}

fold_profit_for_predictor <- function(nm) {
  total <- 0
  n_bets <- 0L
  seasons <- sort(unique(nba$season))
  if (length(seasons) < 2L) {
    return(list(total = total, n_bets = n_bets))
  }

  for (test_season in seasons[-1L]) {
    tr_idx <- nba$season < test_season
    te_idx <- nba$season == test_season
    train <- nba[tr_idx, , drop = FALSE]
    test <- nba[te_idx, , drop = FALSE]
    if (nrow(train) < MIN_TRAIN_ROWS || nrow(test) < 1L) {
      next
    }

    y_tr <- train$home_win
    x_tr <- if (nm == "date_num") train$date_num else train[[nm]]
    d_tr <- data.frame(home_win = y_tr, v = x_tr, stringsAsFactors = FALSE)
    names(d_tr)[2L] <- nm
    d_tr <- stats::na.omit(d_tr)
    if (nrow(d_tr) < MIN_TRAIN_ROWS) {
      next
    }

    xv <- d_tr[[nm]]
    if (is.numeric(xv) && length(unique(xv)) < 2L) {
      next
    }
    is_cat <- is.character(xv) || is.factor(xv)
    n_dist <- if (is_cat) {
      length(unique(as.character(xv)))
    } else {
      length(unique(xv))
    }
    if (is_cat && n_dist > MAX_DISTINCT_FOR_GLM) {
      next
    }
    if (is_cat && n_dist > nrow(d_tr) * 0.35) {
      next
    }

    f <- stats::as.formula(paste("home_win ~", paste0("`", gsub("`", "``", nm, fixed = TRUE), "`")))
    mod <- tryCatch(
      suppressWarnings(
        stats::glm(
          f,
          data = d_tr,
          family = stats::binomial(),
          control = stats::glm.control(maxit = 50, epsilon = 1e-6)
        )
      ),
      error = function(e) NULL
    )
    if (is.null(mod)) {
      next
    }

    nd <- data.frame(v = if (nm == "date_num") test$date_num else test[[nm]], stringsAsFactors = FALSE)
    names(nd)[1L] <- nm
    p_hat <- safe_glm_predict(mod, nd)
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

      if (edge_h > 0 && edge_a > 0 && !is.na(mlh) && !is.na(mla)) {
        if (edge_h >= edge_a) {
          total <- total + profit_bet(mlh, home_won)
          n_bets <- n_bets + 1L
        } else {
          total <- total + profit_bet(mla, away_won)
          n_bets <- n_bets + 1L
        }
      } else if (edge_h > 0 && !is.na(mlh)) {
        total <- total + profit_bet(mlh, home_won)
        n_bets <- n_bets + 1L
      } else if (edge_a > 0 && !is.na(mla)) {
        total <- total + profit_bet(mla, away_won)
        n_bets <- n_bets + 1L
      }
    }
  }

  list(total = total, n_bets = n_bets)
}

cat("Evaluating", length(pred_names), "predictors (rolling train by season)...\n")
flush.console()
out <- vector("list", length(pred_names))
for (i in seq_along(pred_names)) {
  nm <- pred_names[i]
  cat("  [", i, "/", length(pred_names), "] ", nm, "\n", sep = "")
  flush.console()
  fp <- fold_profit_for_predictor(nm)
  out[[i]] <- data.frame(
    predictor = nm,
    total_profit = round(fp$total, 2),
    n_bets = fp$n_bets,
    stringsAsFactors = FALSE
  )
}

results <- do.call(rbind, out)
results <- results[order(-results$total_profit, results$predictor), ]
row.names(results) <- NULL

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "singlepredictormodels.csv")
out_png <- file.path(out_dir, "singlepredictormodels.png")

write.csv(results, out_csv, row.names = FALSE)

if (requireNamespace("gridExtra", quietly = TRUE)) {
  h <- min(22, max(6, nrow(results) * 0.25 + 2))
  png(out_png, width = 12, height = h, units = "in", res = 96)
  grid::grid.newpage()
  gridExtra::grid.table(
    results,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(base_size = 8)
  )
  dev.off()
} else {
  warning("gridExtra not installed; PNG skipped.", call. = FALSE)
}

print(results, row.names = FALSE)
cat("\nSaved:", normalizePath(out_csv), "\n")
if (file.exists(out_png)) {
  cat("Saved:", normalizePath(out_png), "\n")
}

invisible(results)
