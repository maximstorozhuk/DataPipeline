# Univariate logistic p-values: keep FILTER_TOP_K smallest p (Wald test for slope).
# Output: r/output/featureSelection/filter_univariate/selected_features.csv
# Then: Rscript r/featureSelection/run_selectionsmodel.R filter_univariate

FILTER_TOP_K <- 20L

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
fn <- colnames(X)

pvals <- rep(NA_real_, ncol(X))
names(pvals) <- fn
for (j in seq_len(ncol(X))) {
  d <- data.frame(home_win = y, x = X[, j])
  m <- tryCatch(
    stats::glm(home_win ~ x, data = d, family = stats::binomial()),
    error = function(e) NULL
  )
  if (is.null(m)) next
  sm <- summary(m)$coefficients
  if (nrow(sm) >= 2L) {
    pvals[j] <- sm[2L, 4L]
  }
}

ok <- which(!is.na(pvals))
if (length(ok) < 1L) stop("No valid univariate models.")
ord <- ok[order(pvals[ok])]
k <- min(FILTER_TOP_K, length(ord))
sel <- fn[ord[seq_len(k)]]

out_dir <- fs_output_dir(dat$repo_root, "filter_univariate")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "selected_features.csv")
utils::write.csv(
  data.frame(
    feature = sel,
    p_value = pvals[sel],
    rank = seq_along(sel),
    method = "univariate_glm_pvalue",
    stringsAsFactors = FALSE
  ),
  out_csv,
  row.names = FALSE
)

cat("Saved:", normalizePath(out_csv), "\n")
invisible(sel)
