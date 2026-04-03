# BIC stepwise (both directions) logistic regression.
# Output: r/output/featureSelection/bic_stepwise/selected_features.csv
# Can be slow with many p; scope is full numeric matrix.

if (file.exists("r/featureSelection/lib_featureselection_data.R")) {
  source("r/featureSelection/lib_featureselection_data.R")
} else if (file.exists("featureSelection/lib_featureselection_data.R")) {
  source("featureSelection/lib_featureselection_data.R")
} else {
  source("lib_featureselection_data.R")
}

csv_candidates <- c(
  file.path("csv", "nba_2008-2025_extended.csv"),
  file.path("..", "csv", "nba_2008-2025_extended.csv")
)
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) stop("Cannot find nba CSV.")

dat <- prepare_standard_nba_xy(csv_path)
X <- dat$X
y <- dat$y
d <- data.frame(home_win = y, X, check.names = FALSE)

null <- stats::glm(home_win ~ 1, data = d, family = stats::binomial())
full <- stats::glm(home_win ~ ., data = d, family = stats::binomial())

fit <- tryCatch(
  stats::step(
    null,
    scope = list(lower = null, upper = full),
    direction = "both",
    trace = 0L,
    k = log(nrow(d))
  ),
  error = function(e) {
    warning("BIC stepwise failed: ", conditionMessage(e), call. = FALSE)
    NULL
  }
)

if (is.null(fit)) {
  stop("BIC stepwise could not complete.")
}

tn <- attr(stats::terms(fit), "term.labels")
sel <- tn[tn %in% names(d)]
if (length(sel) < 1L) {
  stop(
    "BIC stepwise returned intercept-only. Try fewer predictors or check separation.",
    call. = FALSE
  )
}

out_dir <- fs_output_dir(dat$repo_root, "bic_stepwise")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "selected_features.csv")
utils::write.csv(
  data.frame(feature = sel, method = "bic_stepwise", stringsAsFactors = FALSE),
  out_csv,
  row.names = FALSE
)

cat("Selected n =", length(sel), "\n")
cat("Saved:", normalizePath(out_csv), "\n")
invisible(sel)
