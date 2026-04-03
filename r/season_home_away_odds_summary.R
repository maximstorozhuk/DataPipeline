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

required_cols <- c(
	"season",
	"score_away", "score_home",
	"moneyline_away", "moneyline_home",
	"implied_odds_away", "implied_odds_home"
)
missing_cols <- setdiff(required_cols, names(nba))
if (length(missing_cols) > 0) {
	stop(
		paste0(
			"Missing required columns: ",
			paste(missing_cols, collapse = ", ")
		)
	)
}

american_to_prob <- function(odds) {
	ifelse(
		is.na(odds),
		NA_real_,
		ifelse(
			odds < 0,
			abs(odds) / (abs(odds) + 100),
			100 / (odds + 100)
		)
	)
}

# Win indicator by game.
nba$home_win <- nba$score_home > nba$score_away
nba$away_win <- nba$score_away > nba$score_home
nba$tie <- nba$score_away == nba$score_home

# Use implied probabilities when present, otherwise derive from American moneylines.
home_prob <- ifelse(
	is.na(nba$implied_odds_home),
	american_to_prob(nba$moneyline_home),
	nba$implied_odds_home
)
away_prob <- ifelse(
	is.na(nba$implied_odds_away),
	american_to_prob(nba$moneyline_away),
	nba$implied_odds_away
)

nba$home_prob <- home_prob
nba$away_prob <- away_prob

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "outputEDA")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

build_season_summary <- function(data, label) {
	seasons <- sort(unique(stats::na.omit(data$season)))
	summary_df <- do.call(
		rbind,
		lapply(seasons, function(season_value) {
			subset_data <- data[data$season == season_value & !data$tie, ]
			if (nrow(subset_data) == 0) {
				return(data.frame(
					version = label,
					season = season_value,
					games = 0L,
					home_wins = 0L,
					home_win_pct = NA_real_,
					away_win_pct = NA_real_,
					avg_home_moneyline = NA_real_,
					avg_away_moneyline = NA_real_,
					avg_home_prob = NA_real_,
					avg_away_prob = NA_real_,
					moneyline_gap = NA_real_,
					probability_gap = NA_real_,
					stringsAsFactors = FALSE
				))
			}

			games <- nrow(subset_data)
			home_wins <- sum(subset_data$home_win, na.rm = TRUE)
			away_wins <- sum(subset_data$away_win, na.rm = TRUE)

			avg_home_moneyline <- mean(subset_data$moneyline_home, na.rm = TRUE)
			avg_away_moneyline <- mean(subset_data$moneyline_away, na.rm = TRUE)
			avg_home_prob <- mean(subset_data$home_prob, na.rm = TRUE)
			avg_away_prob <- mean(subset_data$away_prob, na.rm = TRUE)

			data.frame(
				version = label,
				season = season_value,
				games = as.integer(games),
				home_wins = as.integer(home_wins),
				home_win_pct = home_wins / games,
				away_win_pct = away_wins / games,
				avg_home_moneyline = avg_home_moneyline,
				avg_away_moneyline = avg_away_moneyline,
				avg_home_prob = avg_home_prob,
				avg_away_prob = avg_away_prob,
				moneyline_gap = avg_home_moneyline - avg_away_moneyline,
				probability_gap = avg_home_prob - avg_away_prob,
				stringsAsFactors = FALSE
			)
		})
	)

	summary_df <- summary_df[order(summary_df$season), ]
	row.names(summary_df) <- NULL

	summary_df$home_win_pct <- round(summary_df$home_win_pct, 4)
	summary_df$away_win_pct <- round(summary_df$away_win_pct, 4)
	summary_df$avg_home_moneyline <- round(summary_df$avg_home_moneyline, 2)
	summary_df$avg_away_moneyline <- round(summary_df$avg_away_moneyline, 2)
	summary_df$avg_home_prob <- round(summary_df$avg_home_prob, 4)
	summary_df$avg_away_prob <- round(summary_df$avg_away_prob, 4)
	summary_df$moneyline_gap <- round(summary_df$moneyline_gap, 2)
	summary_df$probability_gap <- round(summary_df$probability_gap, 4)

	summary_df
}

safe_fit <- function(formula, data) {
	d <- data[stats::complete.cases(data[, all.vars(formula)]), ]
	if (nrow(d) < 3) {
		return(NULL)
	}
	stats::lm(formula, data = d)
}

extract_stats <- function(model, model_name) {
	if (is.null(model)) {
		return(data.frame(
			model = model_name,
			r_squared = NA_real_,
			adj_r_squared = NA_real_,
			residual_se = NA_real_,
			slope_estimate = NA_real_,
			slope_p_value = NA_real_,
			stringsAsFactors = FALSE
		))
	}
	s <- summary(model)
	coefs <- s$coefficients
	data.frame(
		model = model_name,
		r_squared = unname(s$r.squared),
		adj_r_squared = unname(s$adj.r.squared),
		residual_se = unname(s$sigma),
		slope_estimate = if (nrow(coefs) >= 2) unname(coefs[2, 1]) else NA_real_,
		slope_p_value = if (nrow(coefs) >= 2) unname(coefs[2, 4]) else NA_real_,
		stringsAsFactors = FALSE
	)
}

build_model_stats <- function(summary_df, label) {
	fit_home_win_pct <- safe_fit(home_win_pct ~ season, summary_df)
	fit_home_prob <- safe_fit(avg_home_prob ~ season, summary_df)
	fit_home_win_vs_home_prob <- safe_fit(home_win_pct ~ avg_home_prob, summary_df)

	stats_df <- rbind(
		extract_stats(fit_home_win_pct, "home_win_pct ~ season"),
		extract_stats(fit_home_prob, "avg_home_prob ~ season"),
		extract_stats(fit_home_win_vs_home_prob, "home_win_pct ~ avg_home_prob")
	)
	stats_df$version <- label
	stats_df <- stats_df[, c("version", "model", "r_squared", "adj_r_squared", "residual_se", "slope_estimate", "slope_p_value")]
	stats_df$r_squared <- round(stats_df$r_squared, 6)
	stats_df$adj_r_squared <- round(stats_df$adj_r_squared, 6)
	stats_df$residual_se <- round(stats_df$residual_se, 6)
	stats_df$slope_estimate <- round(stats_df$slope_estimate, 6)
	stats_df$slope_p_value <- round(stats_df$slope_p_value, 6)
	stats_df
}

core_complete_cols <- c("season", "score_away", "score_home", "moneyline_away", "moneyline_home", "home_prob", "away_prob")
nba_without_na <- nba[stats::complete.cases(nba[, core_complete_cols]), ]

season_summary_with_na <- build_season_summary(nba, "with_na")
season_summary_without_na <- build_season_summary(nba_without_na, "without_na")

model_stats_with_na <- build_model_stats(season_summary_with_na, "with_na")
model_stats_without_na <- build_model_stats(season_summary_without_na, "without_na")

open_png_device <- function(file_path, width, height) {
	tryCatch(
		{
			png(file_path, width = width, height = height, units = "in", res = 150)
			return(file_path)
		},
		error = function(e) {
			base <- tools::file_path_sans_ext(basename(file_path))
			ext <- tools::file_ext(file_path)
			timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
			fallback <- file.path(dirname(file_path), paste0(base, "_", timestamp, ".", ext))
			message(
				"Could not write to ", basename(file_path),
				" (likely open/locked). Writing to ", basename(fallback), " instead."
			)
			png(fallback, width = width, height = height, units = "in", res = 150)
			return(fallback)
		}
	)
}

save_png_safely <- function(file_path, width, height, draw_expr) {
	actual_path <- open_png_device(file_path, width, height)
	device_closed <- FALSE
	on.exit({
		if (!device_closed) {
			try(grDevices::dev.off(), silent = TRUE)
		}
	}, add = TRUE)

	ok <- FALSE
	tryCatch(
		{
			force(draw_expr)
			ok <- TRUE
		},
		error = function(e) {
			message("Failed writing PNG: ", basename(file_path), " | ", conditionMessage(e))
		}
	)

	try(grDevices::dev.off(), silent = TRUE)
	device_closed <- TRUE

	if (!ok) {
		if (file.exists(actual_path)) {
			try(file.remove(actual_path), silent = TRUE)
		}
		stop(paste0("Could not generate PNG: ", normalizePath(dirname(actual_path)), "/", basename(actual_path)))
	}

	actual_path
}

draw_summary_plot <- function(summary_df, label) {
	par(mfrow = c(2, 1), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

	plot(
		summary_df$season,
		summary_df$home_win_pct,
		type = "b",
		pch = 16,
		col = "firebrick",
		lwd = 2,
		ylim = c(0, 1),
		xlab = "Season",
		ylab = "Win percentage",
		main = "Home win percentage vs away win percentage by season"
	)
	lines(summary_df$season, summary_df$away_win_pct, type = "b", pch = 16, col = "steelblue4", lwd = 2)
	abline(h = 0.5, lty = 3, col = "grey50")
	legend(
		"topleft",
		legend = c("Home win %", "Away win %", "50% baseline"),
		col = c("firebrick", "steelblue4", "grey50"),
		lty = c(1, 1, 3),
		pch = c(16, 16, NA),
		bty = "n"
	)

	plot(
		summary_df$season,
		summary_df$avg_home_prob,
		type = "b",
		pch = 16,
		col = "darkgreen",
		lwd = 2,
		ylim = range(c(summary_df$avg_home_prob, summary_df$avg_away_prob), na.rm = TRUE),
		xlab = "Season",
		ylab = "Average implied probability",
		main = "Average home/away betting probability by season"
	)
	lines(summary_df$season, summary_df$avg_away_prob, type = "b", pch = 16, col = "purple4", lwd = 2)
	legend(
		"topleft",
		legend = c("Home implied prob", "Away implied prob"),
		col = c("darkgreen", "purple4"),
		lty = 1,
		pch = 16,
		bty = "n"
	)

	mtext(
		paste0("NBA 2008-2025 extended: season-level home advantage vs betting odds (", label, ")"),
		outer = TRUE,
		line = 0.5,
		font = 2
	)
}

out_plot_with_na <- file.path(out_dir, "season_home_away_odds_summary_with_na.png")
out_plot_without_na <- file.path(out_dir, "season_home_away_odds_summary_without_na.png")
out_plot_with_na_actual <- save_png_safely(out_plot_with_na, 13, 8, {
	draw_summary_plot(season_summary_with_na, "with NA rows")
})
out_plot_without_na_actual <- save_png_safely(out_plot_without_na, 13, 8, {
	draw_summary_plot(season_summary_without_na, "without NA rows")
})

build_report <- function(summary_df, stats_df, label) {
	title_block <- gridExtra::arrangeGrob(
		grid::textGrob(
			paste0("Season home vs away odds report (", label, ")"),
			x = 0,
			y = 1,
			just = c("left", "top"),
			gp = grid::gpar(fontsize = 15, fontface = "bold")
		)
	)

	season_block <- gridExtra::arrangeGrob(
		grid::textGrob(
			"Season summary table",
			x = 0,
			y = 1,
			just = c("left", "top"),
			gp = grid::gpar(fontsize = 10, fontface = "bold")
		),
		gridExtra::tableGrob(
			summary_df,
			rows = NULL,
			theme = gridExtra::ttheme_minimal(base_size = 8)
		),
		ncol = 1,
		heights = c(0.08, 0.92)
	)

	stats_block <- gridExtra::arrangeGrob(
		grid::textGrob(
			"Model stats (includes p-values and R-squared)",
			x = 0,
			y = 1,
			just = c("left", "top"),
			gp = grid::gpar(fontsize = 10, fontface = "bold")
		),
		gridExtra::tableGrob(
			stats_df,
			rows = NULL,
			theme = gridExtra::ttheme_minimal(base_size = 8)
		),
		ncol = 1,
		heights = c(0.22, 0.78)
	)

	grid::grid.newpage()
	grid::grid.draw(
		gridExtra::arrangeGrob(
			title_block,
			season_block,
			stats_block,
			ncol = 1,
			heights = c(0.7, 4.2, 1.9)
		)
	)
}

out_report_with_na <- file.path(out_dir, "season_home_away_odds_report_with_na.png")
out_report_without_na <- file.path(out_dir, "season_home_away_odds_report_without_na.png")

if (requireNamespace("gridExtra", quietly = TRUE)) {
	safe_height_with_na <- max(10, nrow(season_summary_with_na) * 0.42 + 4)
	safe_height_without_na <- max(10, nrow(season_summary_without_na) * 0.42 + 4)
	out_report_with_na_actual <- save_png_safely(out_report_with_na, 14, safe_height_with_na, {
		build_report(season_summary_with_na, model_stats_with_na, "with NA rows")
	})
	out_report_without_na_actual <- save_png_safely(out_report_without_na, 14, safe_height_without_na, {
		build_report(season_summary_without_na, model_stats_without_na, "without NA rows")
	})

} else {
	out_report_with_na_actual <- NA_character_
	out_report_without_na_actual <- NA_character_
	warning(
		"Package gridExtra is not installed; skipped report PNGs. Install with: install.packages(\"gridExtra\")",
		call. = FALSE
	)
}

cat("Source:", normalizePath(csv_path), "\n")
cat("Rows in full dataset:", nrow(nba), "\n")
cat("Rows after NA removal (core cols):", nrow(nba_without_na), "\n\n")

cat("Season summary (with NA rows):\n")
print(season_summary_with_na, row.names = FALSE)
cat("\nModel stats (with NA rows):\n")
print(model_stats_with_na, row.names = FALSE)

cat("\nSeason summary (without NA rows):\n")
print(season_summary_without_na, row.names = FALSE)
cat("\nModel stats (without NA rows):\n")
print(model_stats_without_na, row.names = FALSE)

cat("\nSaved outputs:\n")
cat(" -", normalizePath(out_plot_with_na_actual), "\n")
cat(" -", normalizePath(out_plot_without_na_actual), "\n")
if (!is.na(out_report_with_na_actual) && file.exists(out_report_with_na_actual)) {
	cat(" -", normalizePath(out_report_with_na_actual), "\n")
}
if (!is.na(out_report_without_na_actual) && file.exists(out_report_without_na_actual)) {
	cat(" -", normalizePath(out_report_without_na_actual), "\n")
}

if (interactive()) {
	par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
	plot(
		season_summary_with_na$season,
		season_summary_with_na$home_win_pct,
		type = "b",
		pch = 16,
		col = "firebrick",
		lwd = 2,
		ylim = c(0, 1),
		xlab = "Season",
		ylab = "Win %",
		main = "With NA rows: home win %"
	)
	lines(season_summary_with_na$season, season_summary_with_na$away_win_pct, type = "b", pch = 16, col = "steelblue4", lwd = 2)

	plot(
		season_summary_without_na$season,
		season_summary_without_na$home_win_pct,
		type = "b",
		pch = 16,
		col = "firebrick",
		lwd = 2,
		ylim = c(0, 1),
		xlab = "Season",
		ylab = "Win %",
		main = "Without NA rows: home win %"
	)
	lines(season_summary_without_na$season, season_summary_without_na$away_win_pct, type = "b", pch = 16, col = "steelblue4", lwd = 2)

	plot(
		season_summary_with_na$season,
		season_summary_with_na$avg_home_prob,
		type = "b",
		pch = 16,
		col = "darkgreen",
		lwd = 2,
		xlab = "Season",
		ylab = "Avg implied prob",
		main = "With NA rows: implied probs"
	)
	lines(season_summary_with_na$season, season_summary_with_na$avg_away_prob, type = "b", pch = 16, col = "purple4", lwd = 2)

	plot(
		season_summary_without_na$season,
		season_summary_without_na$avg_home_prob,
		type = "b",
		pch = 16,
		col = "darkgreen",
		lwd = 2,
		xlab = "Season",
		ylab = "Avg implied prob",
		main = "Without NA rows: implied probs"
	)
	lines(season_summary_without_na$season, season_summary_without_na$avg_away_prob, type = "b", pch = 16, col = "purple4", lwd = 2)
}

invisible(list(
	season_summary_with_na = season_summary_with_na,
	season_summary_without_na = season_summary_without_na,
	model_stats_with_na = model_stats_with_na,
	model_stats_without_na = model_stats_without_na
))
