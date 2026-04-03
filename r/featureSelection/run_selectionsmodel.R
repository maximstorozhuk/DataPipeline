# Rolling logistic value-betting backtest using selected_features.csv from
# r/output/featureSelection/<subdir>/.
#
# Usage (from DataPipeline repo root):
#   Rscript r/featureSelection/run_selectionsmodel.R <subdir>
# Examples:
#   Rscript r/featureSelection/run_selectionsmodel.R filter_univariate
#   Rscript r/featureSelection/run_selectionsmodel.R ridge_featureselection
#   Rscript r/featureSelection/run_selectionsmodel.R stability_lasso
#
# Backtest output: r/output/featureSelection/<name>_selectionsmodel/
#   (ridge_featureselection -> ridge_selectionsmodel)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop(
    "Usage: Rscript r/featureSelection/run_selectionsmodel.R <feature_output_subdir>\n",
    "Example: Rscript r/featureSelection/run_selectionsmodel.R filter_univariate",
    call. = FALSE
  )
}

subdir <- args[1L]

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
  stop("Cannot find nba_2008-2025_extended.csv", call. = FALSE)
}

repo_root <- dirname(dirname(normalizePath(csv_path)))
feat_path <- file.path(
  repo_root,
  "r",
  "output",
  "featureSelection",
  subdir,
  "selected_features.csv"
)
if (!file.exists(feat_path)) {
  stop("Missing ", feat_path, call. = FALSE)
}

feat_df <- utils::read.csv(feat_path, stringsAsFactors = FALSE)
if (!"feature" %in% names(feat_df)) {
  stop("selected_features.csv must have a column named feature", call. = FALSE)
}
features <- unique(feat_df$feature)

out_name <- if (grepl("_featureselection$", subdir)) {
  sub("_featureselection$", "_selectionsmodel", subdir)
} else {
  paste0(subdir, "_selectionsmodel")
}
out_subdir <- file.path("featureSelection", out_name)
file_prefix <- out_name

cat("Features (n = ", length(features), ") from ", feat_path, "\n", sep = "")
flush.console()

nba <- load_nba_prepared(csv_path)
features <- intersect(features, names(nba))
if (length(features) < 1L) {
  stop("No matching feature names in NBA data.", call. = FALSE)
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
  out_subdir,
  file_prefix,
  paste0("Rolling glm + value bets; features from ", subdir)
)

print(results, row.names = FALSE)
cat("\nSaved:", normalizePath(outs$csv), "\n")
if (file.exists(outs$png)) {
  cat("Saved:", normalizePath(outs$png), "\n")
}

invisible(list(results = results, features = features))
