# Shared rolling logistic + value-betting backtest (used by r/featureSelection/*_selectionsmodel.r).
# Not meant to be run alone — source() from other scripts.

load_nba_prepared <- function(csv_path) {
  nba <- utils::read.csv(
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
  nba
}

rolling_value_bet_backtest <- function(
    nba,
    features,
    stake = 10,
    min_train_base = 80L,
    edge_thresholds = seq(0, 0.15, by = 0.01)
) {
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
        return(stake * (100 / abs(odds)))
      }
      return(stake * (odds / 100))
    }
    -stake
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

  features <- intersect(features, names(nba))
  if (length(features) < 1L) {
    stop("No valid feature names in data.", call. = FALSE)
  }

  min_train <- max(min_train_base, 10L * length(features))
  seasons <- sort(unique(nba$season))
  n_k <- length(edge_thresholds)
  total_profit <- numeric(n_k)
  n_bets <- integer(n_k)

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
        k <- edge_thresholds[ki]
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

  data.frame(
    min_edge = edge_thresholds,
    rule = sprintf("P(side) > implied + %.2f", edge_thresholds),
    total_profit = round(total_profit, 2),
    n_bets = n_bets,
    stringsAsFactors = FALSE
  )
}

save_selection_outputs <- function(
    results,
    features,
    repo_root,
    out_subdir,
    file_prefix,
    method_label
) {
  out_dir <- file.path(repo_root, "r", "output", out_subdir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_csv <- file.path(out_dir, paste0(file_prefix, "_thresholds.csv"))
  out_png <- file.path(out_dir, paste0(file_prefix, "_thresholds.png"))
  feat_txt <- file.path(out_dir, paste0(file_prefix, "_features.txt"))

  utils::write.csv(results, out_csv, row.names = FALSE)
  writeLines(
    c(
      paste0("Feature selection: ", method_label),
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
  }

  list(csv = out_csv, png = out_png, txt = feat_txt, dir = out_dir)
}
