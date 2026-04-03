# Stability selection: repeated subsamples + glmnet Lasso; keep features nonzero
# in at least STABILITY_PROP fraction of runs.
# Requires: glmnet
# Output: r/output/featureSelection/stability_lasso/selected_features.csv

if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop("Install glmnet: install.packages(\"glmnet\")", call. = FALSE)
}

if (file.exists("r/featureSelection/lib_featureselection_data.R")) {
  source("r/featureSelection/lib_featureselection_data.R")
} else if (file.exists("featureSelection/lib_featureselection_data.R")) {
  source("featureSelection/lib_featureselection_data.R")
} else {
  source("lib_featureselection_data.R")
}

STABILITY_B <- 35L
STABILITY_FRAC <- 0.65
SUBSAMPLE <- 0.72
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
fn <- colnames(X)
n <- nrow(X)
p <- ncol(X)

counts <- integer(p)
names(counts) <- fn

cat("Stability Lasso:", STABILITY_B, "subsamples...\n")
flush.console()

for (b in seq_len(STABILITY_B)) {
  idx <- sample.int(n, floor(SUBSAMPLE * n))
  cv <- tryCatch(
    glmnet::cv.glmnet(
      X[idx, , drop = FALSE],
      y[idx],
      family = "binomial",
      alpha = 1,
      nfolds = 5L,
      standardize = TRUE,
      type.measure = "class"
    ),
    error = function(e) NULL
  )
  if (is.null(cv)) next
  cm <- as.matrix(stats::coef(cv, s = "lambda.1se"))
  rn <- rownames(cm)[-1L]
  v <- as.numeric(cm[-1L, 1L])
  names(v) <- rn
  nz <- names(v)[abs(v) > 1e-8]
  counts[nz] <- counts[nz] + 1L
}

thr <- ceiling(STABILITY_FRAC * STABILITY_B)
sel <- names(counts)[counts >= thr]

if (length(sel) < 1L) {
  sel <- names(sort(counts, decreasing = TRUE))[seq_len(min(10L, p))]
  cat("Note: no feature met stability threshold; using top 10 counts.\n")
}

out_dir <- fs_output_dir(dat$repo_root, "stability_lasso")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "selected_features.csv")
utils::write.csv(
  data.frame(
    feature = sel,
    stability_count = counts[sel],
    method = "stability_lasso",
    stringsAsFactors = FALSE
  ),
  out_csv,
  row.names = FALSE
)

cat("Saved:", normalizePath(out_csv), "\n")
invisible(sel)
