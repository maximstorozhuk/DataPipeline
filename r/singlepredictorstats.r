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
pred_names <- setdiff(names(nba), c(leaky, "date"))

# glm with high-cardinality factors (e.g. some W–L strings) can take forever; cap distinct levels
MAX_DISTINCT_FOR_GLM <- 100L

fit_row <- function(nm) {
  d <- data.frame(
    home_win = nba$home_win,
    x = if (nm == "date_num") nba$date_num else nba[[nm]],
    stringsAsFactors = FALSE
  )
  names(d)[2] <- nm
  d <- stats::na.omit(d)
  if (nrow(d) < 30) {
    return(NULL)
  }
  xv <- d[[nm]]
  if (is.numeric(xv) && length(unique(xv)) < 2) {
    return(NULL)
  }
  is_cat <- is.character(xv) || is.factor(xv)
  n_distinct <- if (is_cat) {
    length(unique(as.character(xv)))
  } else {
    length(unique(xv))
  }
  # Character / factor: too many levels → skip glm (still record in output)
  if (is_cat && n_distinct > MAX_DISTINCT_FOR_GLM) {
    return(data.frame(
      predictor = nm,
      n = nrow(d),
      n_distinct = as.integer(n_distinct),
      aic = NA_real_,
      pseudo_r2 = NA_real_,
      lrt_p_value = NA_real_,
      coef_first_term = NA_real_,
      note = paste0("skipped: ", n_distinct, " distinct strings (> ", MAX_DISTINCT_FOR_GLM, ")"),
      stringsAsFactors = FALSE
    ))
  }
  if (is_cat && n_distinct > nrow(d) * 0.35) {
    return(data.frame(
      predictor = nm,
      n = nrow(d),
      n_distinct = as.integer(n_distinct),
      aic = NA_real_,
      pseudo_r2 = NA_real_,
      lrt_p_value = NA_real_,
      coef_first_term = NA_real_,
      note = "skipped: nearly unique per row",
      stringsAsFactors = FALSE
    ))
  }
  f <- stats::as.formula(paste("home_win ~", paste0("`", gsub("`", "``", nm, fixed = TRUE), "`")))
  mod <- tryCatch(
    suppressWarnings(
      stats::glm(
        f,
        data = d,
        family = stats::binomial(),
        control = stats::glm.control(maxit = 50, epsilon = 1e-6)
      )
    ),
    error = function(e) NULL
  )
  if (is.null(mod)) {
    return(NULL)
  }
  # Nested LRT vs intercept-only: use deviances on the same data (no second glm, no anova —
  # anova(null, mod, test="Chisq") can be very slow or appear to hang on some fits).
  pseudo_r2 <- 1 - mod$deviance / mod$null.deviance
  df_lr <- mod$df.null - mod$df.residual
  lr_stat <- mod$null.deviance - mod$deviance
  lrt_p <- if (df_lr > 0 && is.finite(lr_stat) && lr_stat >= 0) {
    stats::pchisq(lr_stat, df = df_lr, lower.tail = FALSE)
  } else {
    NA_real_
  }

  coef_non_int <- stats::coef(mod)
  coef_non_int <- coef_non_int[names(coef_non_int) != "(Intercept)"]
  coef_summary <- if (length(coef_non_int) == 1) {
    as.numeric(coef_non_int)
  } else {
    NA_real_
  }

  data.frame(
    predictor = nm,
    n = nrow(d),
    n_distinct = as.integer(n_distinct),
    aic = stats::AIC(mod),
    pseudo_r2 = round(pseudo_r2, 5),
    lrt_p_value = round(lrt_p, 6),
    coef_first_term = round(coef_summary, 6),
    note = NA_character_,
    stringsAsFactors = FALSE
  )
}

cat("Fitting", length(pred_names), "univariate models...\n")
flush.console()
rows <- vector("list", length(pred_names))
for (i in seq_along(pred_names)) {
  cat("  [", i, "/", length(pred_names), "] ", pred_names[i], "\n", sep = "")
  flush.console()
  rows[[i]] <- fit_row(pred_names[i])
}
rows <- rows[!vapply(rows, is.null, logical(1))]
if (length(rows) == 0) {
  results <- NULL
} else {
  results <- do.call(rbind, rows)
  row.names(results) <- NULL
}

if (!is.null(results) && nrow(results) > 0) {
  ok <- !is.na(results$pseudo_r2)
  if (any(ok)) {
    top <- results[ok, , drop = FALSE]
    top <- top[order(-top$pseudo_r2, top$aic), , drop = FALSE]
    results <- rbind(top, results[!ok, , drop = FALSE])
  }
  row.names(results) <- NULL
}

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_png <- file.path(out_dir, "singlepredictorstats.png")

if (!is.null(results) && nrow(results) > 0) {
  write.csv(results, out_csv, row.names = FALSE)
} else {
  warning("No univariate models were fitted successfully.")
}

# PNG: full table at high res is slow on some systems; preview top rows + lower res
if (!is.null(results) && nrow(results) > 0 && requireNamespace("gridExtra", quietly = TRUE)) {
  png_preview_rows <- min(35L, nrow(results))
  cat("Writing PNG (preview of first", png_preview_rows, "rows)...\n")
  flush.console()
  tbl <- results[seq_len(png_preview_rows), , drop = FALSE]
  png(
    out_png,
    width = 12,
    height = min(18, max(5, png_preview_rows * 0.28 + 2)),
    units = "in",
    res = 96
  )
  grid::grid.newpage()
  gridExtra::grid.table(
    tbl,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(base_size = 8)
  )
  dev.off()
} else if (!is.null(results) && nrow(results) > 0) {
  warning(
    "Package gridExtra is not installed; skipped PNG. Install with: install.packages(\"gridExtra\")",
    call. = FALSE
  )
}

cat("Response: home_win = 1 if home score > away score (ties removed).\n")
cat("Predictors: one at a time, logistic regression. Excluded in-game score columns.\n")
cat("coef_first_term: slope for a single numeric term; NA if factor / multiple coefficients.\n\n")
if (!is.null(results) && nrow(results) > 0) {
  if (nrow(results) > 25) {
    print(head(results, 25), row.names = FALSE)
    cat("... (", nrow(results) - 25, " more rows; see CSV)\n", sep = "")
  } else {
    print(results, row.names = FALSE)
  }
  cat("\nSaved CSV:", normalizePath(out_csv), "\n")
  if (file.exists(out_png)) {
    prev_n <- min(35L, nrow(results))
    cat(
      "Saved image:", normalizePath(out_png),
      "(table preview:", prev_n, "of", nrow(results), "rows)\n"
    )
  }
}

invisible(results)
