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

for (nm in names(nba)) {
  x <- nba[[nm]]
  if (!is.character(x)) next
  u <- unique(stats::na.omit(x))
  if (length(u) == 0) next
  if (all(tolower(u) %in% c("true", "false"))) {
    nba[[nm]] <- ifelse(is.na(x), NA, tolower(x) == "true")
  }
}

appropriate <- vapply(
  nba,
  function(x) is.numeric(x) || is.logical(x),
  logical(1)
)
col_names <- names(nba)[appropriate]

summary_row <- function(nm) {
  x <- nba[[nm]]
  if (is.logical(x)) x <- as.numeric(x)
  n_miss <- sum(is.na(x))
  ok <- !is.na(x)
  n_ok <- sum(ok)
  if (n_ok == 0) {
    return(data.frame(
      column = nm,
      n_valid = 0L,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      median = NA_real_,
      max = NA_real_,
      n_missing = as.integer(n_miss),
      stringsAsFactors = FALSE
    ))
  }
  xv <- x[ok]
  data.frame(
    column = nm,
    n_valid = as.integer(n_ok),
    mean = mean(xv),
    sd = stats::sd(xv),
    min = min(xv),
    median = stats::median(xv),
    max = max(xv),
    n_missing = as.integer(n_miss),
    stringsAsFactors = FALSE
  )
}

summary_tbl <- do.call(rbind, lapply(col_names, summary_row))
row.names(summary_tbl) <- NULL

summary_tbl$mean <- round(summary_tbl$mean, 4)
summary_tbl$sd <- round(summary_tbl$sd, 4)
summary_tbl$min <- round(summary_tbl$min, 4)
summary_tbl$median <- round(summary_tbl$median, 4)
summary_tbl$max <- round(summary_tbl$max, 4)

summary_tbl <- summary_tbl[order(summary_tbl$column), ]

n_rows <- nrow(nba)
n_cols <- ncol(nba)

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_png <- file.path(out_dir, "summarystats.png")
if (requireNamespace("gridExtra", quietly = TRUE)) {
  png(
    out_png,
    width = 14,
    height = max(6, nrow(summary_tbl) * 0.35 + 1.5),
    units = "in",
    res = 150
  )
  grid::grid.newpage()
  gridExtra::grid.table(
    summary_tbl,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(base_size = 8)
  )
  dev.off()
} else {
  warning(
    "Package gridExtra is not installed; skipped PNG. Install with: install.packages(\"gridExtra\")",
    call. = FALSE
  )
}

cat("File:", normalizePath(csv_path), "\n")
cat("Rows:", n_rows, "  Columns:", n_cols, "\n")
cat("Summarized columns (numeric / logical):", length(col_names), "\n\n")
print(summary_tbl, row.names = FALSE)
if (file.exists(out_png)) {
  cat("\nSaved image:", normalizePath(out_png), "\n")
}

invisible(summary_tbl)
