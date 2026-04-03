# Aggregate rolling backtest outputs from every *_selectionsmodel folder under
# r/output/featureSelection/, write one long-format CSV, and plot profit vs edge.
#
# x-axis: minimum edge x (bet when model prob exceeds implied by more than x), 0–0.15
# y-axis: total profit ($)
#
# Run after workflows complete, or standalone from repo root:
#   Rscript r/featureSelection/compare_all_selectionsmodels.R

csv_candidates <- c(
  file.path("csv", "nba_2008-2025_extended.csv"),
  file.path("..", "csv", "nba_2008-2025_extended.csv")
)
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) {
  stop("Cannot find nba CSV (run from DataPipeline repo root).", call. = FALSE)
}
repo_root <- dirname(dirname(normalizePath(csv_path)))

fs_out <- file.path(repo_root, "r", "output", "featureSelection")
if (!dir.exists(fs_out)) {
  stop("Missing ", fs_out, call. = FALSE)
}

technique_pretty <- function(folder_base) {
  key <- c(
    l1regularization_selectionsmodel = "L1 (lambda.min features + glm)",
    l1_selectionsmodel = "L1 (glmnet + lenient lambda)",
    ridge_selectionsmodel = "Ridge (top-K |coef|)",
    filter_univariate_selectionsmodel = "Univariate glm p-value",
    bic_stepwise_selectionsmodel = "BIC stepwise",
    stability_lasso_selectionsmodel = "Stability Lasso",
    rf_importance_selectionsmodel = "Random forest importance",
    boruta_selectionsmodel = "Boruta",
    pca_loading_selectionsmodel = "PCA loadings",
    rfe_caret_selectionsmodel = "Caret RFE"
  )
  if (folder_base %in% names(key)) {
    return(unname(key[folder_base]))
  }
  sub("_selectionsmodel$", "", folder_base)
}

subdirs <- list.dirs(fs_out, full.names = TRUE, recursive = FALSE)
subdirs <- subdirs[basename(subdirs) != "comparison"]

rows <- list()
for (d in subdirs) {
  csvs <- list.files(d, pattern = "_thresholds\\.csv$", full.names = TRUE)
  if (length(csvs) != 1L) {
    next
  }
  tab <- utils::read.csv(csvs[[1L]], stringsAsFactors = FALSE)
  if (!all(c("min_edge", "total_profit") %in% names(tab))) {
    warning("Skip (bad columns): ", csvs[[1L]], call. = FALSE)
    next
  }
  fb <- basename(d)
  tab$technique <- technique_pretty(fb)
  tab$output_folder <- fb
  rows[[length(rows) + 1L]] <- tab
}

if (length(rows) < 1L) {
  stop("No *_thresholds.csv files found under ", fs_out, call. = FALSE)
}

combined <- do.call(rbind, rows)
row.names(combined) <- NULL

cmp_dir <- file.path(fs_out, "comparison")
dir.create(cmp_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(cmp_dir, "all_techniques_profit_by_edge.csv")
utils::write.csv(combined, out_csv, row.names = FALSE)
cat("Saved:", normalizePath(out_csv), "\n")

out_png <- file.path(cmp_dir, "all_techniques_profit_vs_edge.png")

if (requireNamespace("ggplot2", quietly = TRUE)) {
  g <- ggplot2::ggplot(
    combined,
    ggplot2::aes(x = min_edge, y = total_profit, color = technique)
  ) +
    ggplot2::geom_line(ggplot2::aes(group = technique), linewidth = 0.35, alpha = 0.85) +
    ggplot2::geom_point(size = 2.2, alpha = 0.9) +
    ggplot2::scale_x_continuous(
      limits = c(0, 0.15),
      breaks = seq(0, 0.15, by = 0.02),
      expand = c(0.01, 0)
    ) +
    ggplot2::labs(
      title = "Rolling value bets: total profit vs minimum edge threshold",
      subtitle = "Edge x: bet only when model win prob exceeds implied prob by more than x",
      x = "Minimum edge x (0 to 0.15)",
      y = "Total profit ($)",
      color = "Feature selection"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave(
    out_png,
    g,
    width = 11,
    height = 6.5,
    dpi = 120,
    bg = "white"
  )
  cat("Saved:", normalizePath(out_png), "\n")
} else {
  grDevices::png(out_png, width = 11, height = 6.5, units = "in", res = 120)
  on.exit(grDevices::dev.off(), add = TRUE)
  techs <- unique(combined$technique)
  n <- length(techs)
  pal <- grDevices::hcl.colors(max(3L, n), "Dark 3")[seq_len(n)]
  yr <- range(combined$total_profit, na.rm = TRUE)
  if (!all(is.finite(yr))) {
    yr <- c(-1, 1)
  }
  graphics::plot(
    NA,
    xlim = c(0, 0.15),
    ylim = yr,
    xlab = "Minimum edge x (0 to 0.15)",
    ylab = "Total profit ($)",
    main = "Rolling value bets: profit vs edge by method"
  )
  graphics::grid()
  for (i in seq_len(n)) {
    sub <- combined[combined$technique == techs[i], , drop = FALSE]
    o <- order(sub$min_edge)
    graphics::lines(sub$min_edge[o], sub$total_profit[o], col = pal[i], lwd = 1.5)
    graphics::points(sub$min_edge, sub$total_profit, col = pal[i], pch = 16, cex = 0.9)
  }
  graphics::legend(
    "topright",
    legend = techs,
    col = pal,
    lwd = 1.5,
    pch = 16,
    cex = 0.65,
    ncol = 1L
  )
  cat("Saved (base graphics):", normalizePath(out_png), "\n")
}

invisible(list(combined = combined, csv = out_csv, png = out_png))
