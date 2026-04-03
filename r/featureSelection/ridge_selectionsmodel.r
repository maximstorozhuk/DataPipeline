# Rolling backtest using Ridge-selected features (see ridge_featureselection.r).

subdir <- "ridge_featureselection"

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
if (is.na(csv_path)) stop("Cannot find nba CSV.")

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
  stop("Run r/featureSelection/ridge_featureselection.r first.", call. = FALSE)
}

features <- unique(utils::read.csv(feat_path, stringsAsFactors = FALSE)$feature)
nba <- load_nba_prepared(csv_path)
features <- intersect(features, names(nba))

results <- rolling_value_bet_backtest(nba, features)
outs <- save_selection_outputs(
  results,
  features,
  repo_root,
  file.path("featureSelection", "ridge_selectionsmodel"),
  "ridge_selectionsmodel",
  "ridge_top_k (ridge_featureselection.r)"
)

print(results, row.names = FALSE)
cat("\nSaved:", normalizePath(outs$csv), "\n")
invisible(list(results = results, features = features))
