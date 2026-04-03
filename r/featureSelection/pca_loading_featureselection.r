# Rank original variables by sum of squared loadings on first PCA_TOP_PCS principal components
# (exploratory; selection uses full-sample PCA for ranking only).
# Output: r/output/featureSelection/pca_loading/selected_features.csv

if (file.exists("r/featureSelection/lib_featureselection_data.R")) {
  source("r/featureSelection/lib_featureselection_data.R")
} else if (file.exists("featureSelection/lib_featureselection_data.R")) {
  source("featureSelection/lib_featureselection_data.R")
} else {
  source("lib_featureselection_data.R")
}

PCA_TOP_PCS <- 5L
PCA_TOP_K <- 20L

csv_candidates <- c(
  file.path("csv", "nba_2008-2025_extended.csv"),
  file.path("..", "csv", "nba_2008-2025_extended.csv")
)
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) stop("Cannot find nba CSV.")

dat <- prepare_standard_nba_xy(csv_path)
X <- dat$X
fn <- colnames(X)

pr <- stats::prcomp(X, center = TRUE, scale. = TRUE)
L <- pr$rotation[, seq_len(min(PCA_TOP_PCS, ncol(pr$rotation))), drop = FALSE]
sc <- rowSums(L^2)
ord <- order(sc, decreasing = TRUE)
k <- min(PCA_TOP_K, length(ord))
sel <- fn[ord[seq_len(k)]]

out_dir <- fs_output_dir(dat$repo_root, "pca_loading")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "selected_features.csv")
utils::write.csv(
  data.frame(
    feature = sel,
    loading_score = sc[sel],
    method = "pca_loading_sq",
    stringsAsFactors = FALSE
  ),
  out_csv,
  row.names = FALSE
)

cat("Saved:", normalizePath(out_csv), "\n")
invisible(sel)
