# Rolling value-bet backtest using L1/elastic-net features from l1regularization.r.
# Uses FEATURE_LAMBDA (+ optional AUTO_LENIENT fallback); distinct from
# run_selectionsmodel.R l1regularization (which uses selected_features.csv = lambda.min only).

FEATURE_LAMBDA <- "lambda.min"
AUTO_LENIENT <- TRUE
MIN_FEATURES_PREFERRED <- 3L

if (file.exists(file.path("r", "lib_rolling_selectionsmodel.R"))) {
  source(file.path("r", "lib_rolling_selectionsmodel.R"))
} else {
  source("lib_rolling_selectionsmodel.R")
}

csv_candidates <- c(
  file.path("csv", "nba_2008-2025_extended.csv"),
  file.path("..", "csv", "nba_2008-2025_extended.csv")
)
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) {
  stop("Cannot find nba_2008-2025_extended.csv (expected csv/ under the project root).")
}

repo_root <- dirname(dirname(normalizePath(csv_path)))
coef_path <- file.path(
  repo_root,
  "r",
  "output",
  "featureSelection",
  "l1regularization",
  "l1regularization_coefficients.csv"
)
if (!file.exists(coef_path)) {
  stop(
    "Missing ", coef_path, " — run r/featureSelection/l1regularization.r first.",
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
  "Features from ", used_lab, " (n = ", length(features), ")...\n",
  sep = ""
)
flush.console()

nba <- load_nba_prepared(csv_path)
features <- intersect(features, names(nba))
miss <- setdiff(unique(sel$feature), names(nba))
if (length(miss)) {
  cat("Dropping features not in data:", paste(miss, collapse = ", "), "\n")
}
if (length(features) < 1L) {
  stop("No L1-selected features found in dataset.", call. = FALSE)
}

results <- rolling_value_bet_backtest(
  nba,
  features,
  stake = 10,
  min_train_base = 80L,
  edge_thresholds = seq(0, 0.15, by = 0.01)
)

outs <- save_selection_outputs(
  results,
  features,
  repo_root,
  file.path("featureSelection", "l1_selectionsmodel"),
  "l1_selectionsmodel",
  paste0("L1 glmnet; lambda: ", used_lab)
)

cat("\nFeatures used (", length(features), "): ", paste(features, collapse = ", "), "\n\n", sep = "")
print(results, row.names = FALSE)
cat("\nSaved:", normalizePath(outs$csv), "\n")
cat("Saved:", normalizePath(outs$txt), "\n")
if (file.exists(outs$png)) {
  cat("Saved:", normalizePath(outs$png), "\n")
}

invisible(list(results = results, features = features))
