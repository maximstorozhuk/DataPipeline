csv_candidates <- c(
  file.path("csv", "nba_2008-2025_extended.csv"),
  file.path("..", "csv", "nba_2008-2025_extended.csv")
)
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) {
  stop("Cannot find nba_2008-2025_extended.csv (expected csv/ under the project root).")
}

nba <- read.csv(
  csv_path,
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE
)

n_rows <- nrow(nba)
n_cols <- ncol(nba)

missing_n <- vapply(nba, function(x) sum(is.na(x)), integer(1))
missing_pct <- 100 * missing_n / n_rows

stats <- data.frame(
  column = names(missing_n),
  n_missing = as.integer(missing_n),
  pct_missing = round(missing_pct, 2),
  n_complete = n_rows - as.integer(missing_n),
  row.names = NULL
)
stats <- stats[order(-stats$pct_missing, stats$column), ]

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_png <- file.path(out_dir, "missingness.png")
if (requireNamespace("gridExtra", quietly = TRUE)) {
  png(
    out_png,
    width = 12,
    height = max(6, nrow(stats) * 0.35 + 1.5),
    units = "in",
    res = 150
  )
  grid::grid.newpage()
  gridExtra::grid.table(stats, rows = NULL)
  dev.off()
} else {
  warning(
    "Package gridExtra is not installed; skipped PNG. Install with: install.packages(\"gridExtra\")",
    call. = FALSE
  )
}

cat("File:", normalizePath(csv_path), "\n")
cat("Rows:", n_rows, "  Columns:", n_cols, "\n\n")
cat("Per-column missingness (sorted by % missing, descending):\n\n")
print(stats, row.names = FALSE)
if (file.exists(out_png)) {
  cat("Saved image:", normalizePath(out_png), "\n")
}

invisible(stats)
