# Boruta all-relevant feature selection (RF-based).
# Requires: Boruta
# Output: r/output/featureSelection/boruta/selected_features.csv
# May take several minutes.

if (!requireNamespace("Boruta", quietly = TRUE)) {
  stop("Install Boruta: install.packages(\"Boruta\")", call. = FALSE)
}

if (file.exists("r/featureSelection/lib_featureselection_data.R")) {
  source("r/featureSelection/lib_featureselection_data.R")
} else if (file.exists("featureSelection/lib_featureselection_data.R")) {
  source("featureSelection/lib_featureselection_data.R")
} else {
  source("lib_featureselection_data.R")
}

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
d <- data.frame(home_win = y, X, check.names = FALSE)

b <- Boruta::Boruta(
  home_win ~ .,
  data = d,
  maxRuns = 30L,
  doTrace = 0L
)

dec <- Boruta::getSelectedAttributes(b, withTentative = FALSE)
if (length(dec) < 1L) {
  dec <- Boruta::getSelectedAttributes(b, withTentative = TRUE)
}
sel <- dec

out_dir <- fs_output_dir(dat$repo_root, "boruta")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "selected_features.csv")
utils::write.csv(
  data.frame(feature = sel, method = "boruta", stringsAsFactors = FALSE),
  out_csv,
  row.names = FALSE
)

cat("Saved:", normalizePath(out_csv), "\n")
invisible(sel)
