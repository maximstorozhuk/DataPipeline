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

required_cols <- c("season", "whos_favored", "score_home", "score_away")
missing_cols <- setdiff(required_cols, names(nba))
if (length(missing_cols) > 0) {
	stop(
		paste0(
			"Missing required columns: ",
			paste(missing_cols, collapse = ", ")
		)
	)
}

fav <- tolower(trimws(as.character(nba$whos_favored)))
fav[!(fav %in% c("home", "away"))] <- NA

winner <- ifelse(
	nba$score_home > nba$score_away,
	"home",
	ifelse(nba$score_away > nba$score_home, "away", NA)
)

analysis_df <- data.frame(
	season = nba$season,
	favorite = fav,
	winner = winner,
	stringsAsFactors = FALSE
)

analysis_df$favorite_correct <- analysis_df$favorite == analysis_df$winner
usable <- analysis_df[stats::complete.cases(analysis_df[, c("season", "favorite", "winner")]), ]

if (nrow(usable) == 0) {
	stop("No usable rows after filtering. Check whos_favored and score columns.")
}

overall <- data.frame(
	n_games = nrow(usable),
	n_correct = sum(usable$favorite_correct, na.rm = TRUE),
	accuracy = mean(usable$favorite_correct, na.rm = TRUE),
	stringsAsFactors = FALSE
)

seasons <- sort(unique(usable$season))
by_season <- do.call(
	rbind,
	lapply(seasons, function(s) {
		d <- usable[usable$season == s, ]
		data.frame(
			season = s,
			n_games = nrow(d),
			n_correct = sum(d$favorite_correct, na.rm = TRUE),
			accuracy = mean(d$favorite_correct, na.rm = TRUE),
			stringsAsFactors = FALSE
		)
	})
)

row.names(by_season) <- NULL
overall$accuracy <- round(overall$accuracy, 4)
by_season$accuracy <- round(by_season$accuracy, 4)

# Optional trend model so the report includes p-value and R-squared.
trend_df <- data.frame(
	season = by_season$season,
	accuracy = by_season$accuracy,
	stringsAsFactors = FALSE
)
trend_fit <- stats::lm(accuracy ~ season, data = trend_df)
trend_summary <- summary(trend_fit)

trend_stats <- data.frame(
	metric = c(
		"slope_estimate",
		"slope_p_value",
		"intercept",
		"r_squared",
		"adj_r_squared",
		"residual_se"
	),
	value = c(
		unname(stats::coef(trend_fit)["season"]),
		unname(trend_summary$coefficients["season", "Pr(>|t|)"]),
		unname(stats::coef(trend_fit)["(Intercept)"]),
		unname(trend_summary$r.squared),
		unname(trend_summary$adj.r.squared),
		unname(trend_summary$sigma)
	),
	stringsAsFactors = FALSE
)
trend_stats$value <- round(trend_stats$value, 6)

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "outputEDA")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_plot <- file.path(out_dir, "favorite_prediction_accuracy_by_season.png")
png(out_plot, width = 11, height = 5.5, units = "in", res = 150)
plot(
	by_season$season,
	by_season$accuracy,
	type = "b",
	pch = 16,
	col = "steelblue4",
	lwd = 2,
	ylim = c(0, 1),
	xlab = "Season",
	ylab = "Favorite prediction accuracy",
	main = "How often sportsbook favorite wins (by season)"
)
abline(h = 0.5, lty = 3, col = "grey50")
legend(
	"bottomleft",
	legend = c("Favorite wins", "50% baseline"),
	col = c("steelblue4", "grey50"),
	lty = c(1, 3),
	pch = c(16, NA),
	bty = "n"
)
dev.off()

out_report <- file.path(out_dir, "favorite_prediction_accuracy_report.png")
if (requireNamespace("gridExtra", quietly = TRUE)) {
	png(
		out_report,
		width = 14,
		height = max(9, nrow(by_season) * 0.28 + 5),
		units = "in",
		res = 150
	)

	title_block <- gridExtra::arrangeGrob(
		grid::textGrob(
			"Sportsbook Favorite Accuracy Report",
			x = 0,
			y = 1,
			just = c("left", "top"),
			gp = grid::gpar(fontsize = 16, fontface = "bold")
		),
		grid::textGrob(
			"Question: How often does the favored team actually win?",
			x = 0,
			y = 1,
			just = c("left", "top"),
			gp = grid::gpar(fontsize = 11)
		),
		ncol = 1,
		heights = c(0.6, 0.4)
	)

	overall_block <- gridExtra::arrangeGrob(
		grid::textGrob(
			"Overall accuracy",
			x = 0,
			y = 1,
			just = c("left", "top"),
			gp = grid::gpar(fontsize = 11, fontface = "bold")
		),
		gridExtra::tableGrob(
			overall,
			rows = NULL,
			theme = gridExtra::ttheme_minimal(base_size = 10)
		),
		ncol = 1,
		heights = c(0.22, 0.78)
	)

	by_season_block <- gridExtra::arrangeGrob(
		grid::textGrob(
			"By-season accuracy",
			x = 0,
			y = 1,
			just = c("left", "top"),
			gp = grid::gpar(fontsize = 11, fontface = "bold")
		),
		gridExtra::tableGrob(
			by_season,
			rows = NULL,
			theme = gridExtra::ttheme_minimal(base_size = 8)
		),
		ncol = 1,
		heights = c(0.12, 0.88)
	)

	trend_block <- gridExtra::arrangeGrob(
		grid::textGrob(
			"Trend model stats: accuracy ~ season (includes p-value and R-squared)",
			x = 0,
			y = 1,
			just = c("left", "top"),
			gp = grid::gpar(fontsize = 10, fontface = "bold")
		),
		gridExtra::tableGrob(
			trend_stats,
			rows = NULL,
			theme = gridExtra::ttheme_minimal(base_size = 8)
		),
		ncol = 1,
		heights = c(0.28, 0.72)
	)

	full_report <- gridExtra::arrangeGrob(
		title_block,
		overall_block,
		by_season_block,
		trend_block,
		ncol = 1,
		heights = c(1.0, 1.1, 3.6, 1.8)
	)

	grid::grid.newpage()
	grid::grid.draw(full_report)
	dev.off()
} else {
	warning(
		"Package gridExtra is not installed; skipped report PNG. Install with: install.packages(\"gridExtra\")",
		call. = FALSE
	)
}

if (interactive()) {
	plot(
		by_season$season,
		by_season$accuracy,
		type = "b",
		pch = 16,
		col = "steelblue4",
		lwd = 2,
		ylim = c(0, 1),
		xlab = "Season",
		ylab = "Favorite prediction accuracy",
		main = "How often sportsbook favorite wins (by season)"
	)
	abline(h = 0.5, lty = 3, col = "grey50")
	legend(
		"bottomleft",
		legend = c("Favorite wins", "50% baseline"),
		col = c("steelblue4", "grey50"),
		lty = c(1, 3),
		pch = c(16, NA),
		bty = "n"
	)
}

cat("Source:", normalizePath(csv_path), "\n")
cat("Overall favorite accuracy:\n")
print(overall, row.names = FALSE)
cat("\nBy-season favorite accuracy:\n")
print(by_season, row.names = FALSE)
cat("\nTrend model stats (accuracy ~ season):\n")
print(trend_stats, row.names = FALSE)
cat("\nSaved outputs:\n")
cat(" -", normalizePath(out_plot), "\n")
if (file.exists(out_report)) {
	cat(" -", normalizePath(out_report), "\n")
}

invisible(list(overall = overall, by_season = by_season, trend_stats = trend_stats))
