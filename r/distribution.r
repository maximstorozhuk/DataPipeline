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

nba$game_total <- nba$score_away + nba$score_home
nba$margin <- nba$score_home - nba$score_away

plot_hist_density <- function(x, main, xlab = NULL, col = "grey85") {
  xlab <- if (is.null(xlab)) main else xlab
  x <- x[!is.na(x)]
  if (length(x) < 2) {
    plot.new()
    text(0.5, 0.5, paste("Insufficient data:\n", main), cex = 0.9)
    return()
  }
  br <- min(40, max(10, nclass.Sturges(x)))
  h <- hist(x, plot = FALSE, breaks = br)
  dens_y <- if (length(x) >= 2 && stats::sd(x) > 0) {
    stats::density(x)$y
  } else {
    0
  }
  ylim <- range(0, h$density, dens_y, na.rm = TRUE)
  hist(
    x,
    freq = FALSE,
    col = col,
    border = "white",
    main = main,
    xlab = xlab,
    ylim = ylim,
    breaks = br
  )
  if (length(x) >= 2 && stats::sd(x) > 0) {
    lines(stats::density(x), col = "steelblue4", lwd = 2)
  }
}

# Count histogram only (no density) — better for heavy-tailed / discrete-leaning odds
plot_hist_count <- function(x, main, xlab = NULL, col = "grey85", xlim = NULL) {
  xlab <- if (is.null(xlab)) main else xlab
  x <- x[!is.na(x)]
  if (!is.null(xlim)) {
    x <- x[x >= xlim[1] & x <= xlim[2]]
  }
  if (length(x) < 1) {
    plot.new()
    text(0.5, 0.5, paste("Insufficient data:\n", main), cex = 0.9)
    return()
  }
  br <- min(50, max(15, nclass.Sturges(x)))
  hist(
    x,
    freq = TRUE,
    col = col,
    border = "white",
    main = main,
    xlab = xlab,
    ylab = "count",
    breaks = br,
    xlim = xlim
  )
}

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "outputEDA")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_png <- file.path(out_dir, "distribution.png")

png(out_png, width = 12, height = 13, units = "in", res = 150)
par(mfrow = c(4, 2), mar = c(4, 4, 2.5, 1), oma = c(0, 0, 2, 0))

plot_hist_density(nba$spread, "Spread (closing line)", xlab = "points")
plot_hist_density(nba$total, "Game total (O/U line)", xlab = "points")
plot_hist_density(nba$game_total, "Total points scored (away + home)", xlab = "points")
plot_hist_density(nba$margin, "Margin (home score − away score)", xlab = "points")
plot_hist_count(
  nba$moneyline_away,
  "Moneyline (away)",
  xlab = "American odds (negative = favorite)",
  xlim = c(-1000, 1000)
)
plot_hist_count(
  nba$moneyline_home,
  "Moneyline (home)",
  xlab = "American odds (negative = favorite)",
  xlim = c(-1000, 1000)
)

boxplot(
  spread ~ regular,
  data = nba,
  main = "Spread by game type",
  xlab = "regular season (TRUE) vs playoffs (FALSE)",
  ylab = "spread",
  col = c("lightsteelblue", "lightsalmon"),
  border = "grey30"
)

xs <- nba$spread[!is.na(nba$spread)]
if (length(xs) >= 2 && stats::sd(xs) > 0) {
  stats::qqnorm(xs, main = "Spread: normal Q-Q plot", pch = 16, cex = 0.35, col = "grey35")
  stats::qqline(xs, col = "steelblue4", lwd = 2)
} else {
  plot.new()
  text(0.5, 0.5, "Insufficient spread data for Q-Q", cex = 0.9)
}

mtext(
  "NBA 2008–2025 extended: distribution overview",
  outer = TRUE,
  line = 0.3,
  cex = 1.1,
  font = 2
)

dev.off()

cat("Saved:", normalizePath(out_png), "\n")
