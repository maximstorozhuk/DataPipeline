# Random forest variable importance (MeanDecreaseAccuracy); top RF_TOP_K.
# Requires: randomForest
# Output: r/output/featureSelection/rf_importance/selected_features.csv

if (!requireNamespace("randomForest", quietly = TRUE)) {
  stop("Install randomForest: install.packages(\"randomForest\")", call. = FALSE)
}

if (file.exists("r/featureSelection/lib_featureselection_data.R")) {
  source("r/featureSelection/lib_featureselection_data.R")
} else if (file.exists("featureSelection/lib_featureselection_data.R")) {
  source("featureSelection/lib_featureselection_data.R")
} else {
  source("lib_featureselection_data.R")
}

RF_TOP_K <- 20L
set.seed(42L)

csv_candidates <- c(
  file.path("csv", "nba_2008-2025_extended.csv"),
  file.path("..", "csv", "nba_2008-2025_extended.csv")
)
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) stop("Cannot find nba CSV.")

dat <- prepare_standard_nba_xy(csv_path)
X <- dat$X
y <- dat$y
d <- data.frame(home_win = factor(y, levels = c(0, 1), labels = c("loss", "win")), X, check.names = FALSE)

rf <- randomForest::randomForest(
  home_win ~ .,
  data = d,
  importance = TRUE,
  ntree = 300L,
  na.action = stats::na.omit
)

imp <- randomForest::importance(rf)
mc <- ncol(imp)
score <- if (mc >= 1L) {
  rowMeans(abs(imp[, seq_len(min(2L, mc)), drop = FALSE]))
} else {
  rep(0, nrow(imp))
}
names(score) <- rownames(imp)
ord <- order(score, decreasing = TRUE)
k <- min(RF_TOP_K, length(ord))
sel <- names(score)[ord[seq_len(k)]]

out_dir <- fs_output_dir(dat$repo_root, "rf_importance")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "selected_features.csv")
utils::write.csv(
  data.frame(
    feature = sel,
    importance = score[sel],
    method = "random_forest",
    stringsAsFactors = FALSE
  ),
  out_csv,
  row.names = FALSE
)

cat("Saved:", normalizePath(out_csv), "\n")
invisible(sel)
