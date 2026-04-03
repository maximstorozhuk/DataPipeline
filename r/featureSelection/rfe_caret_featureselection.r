# Recursive feature elimination with logistic regression via caret (if installed).
# Output: r/output/featureSelection/rfe_caret/selected_features.csv
# Requires: caret, lattice, ggplot2 (caret dependencies)

if (!requireNamespace("caret", quietly = TRUE)) {
  stop("Install caret: install.packages(c(\"caret\", \"lattice\", \"ggplot2\"))", call. = FALSE)
}

if (file.exists("r/featureSelection/lib_featureselection_data.R")) {
  source("r/featureSelection/lib_featureselection_data.R")
} else if (file.exists("featureSelection/lib_featureselection_data.R")) {
  source("featureSelection/lib_featureselection_data.R")
} else {
  source("lib_featureselection_data.R")
}

RFE_MAX_FEATURES <- 20L
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
d <- data.frame(home_win = factor(y, levels = c(0, 1)), X, check.names = FALSE)

ctrl <- caret::rfeControl(
  functions = caret::lrFuncs,
  method = "cv",
  number = 5L,
  verbose = FALSE
)

sizes <- c(
  min(RFE_MAX_FEATURES, ncol(X)),
  max(2L, min(5L, ncol(X))),
  as.integer(max(2L, ncol(X) / 2)),
  10L,
  15L
)
sizes <- sort(unique(sizes[sizes <= ncol(X) & sizes >= 2L]))
if (length(sizes) < 1L) sizes <- ncol(X)

rfe_fit <- caret::rfe(
  x = X,
  y = d$home_win,
  sizes = sizes,
  rfeControl = ctrl,
  metric = "Accuracy"
)

sel <- caret::predictors(rfe_fit)
if (length(sel) < 1L) {
  sel <- colnames(X)[seq_len(min(RFE_MAX_FEATURES, ncol(X)))]
}

out_dir <- fs_output_dir(dat$repo_root, "rfe_caret")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "selected_features.csv")
utils::write.csv(
  data.frame(feature = sel, method = "caret_rfe", stringsAsFactors = FALSE),
  out_csv,
  row.names = FALSE
)

cat("Saved:", normalizePath(out_csv), "\n")
invisible(sel)
