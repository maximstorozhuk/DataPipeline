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
	"score_away", "score_home",
	"win_loss_away", "win_loss_home",
	"moneyline_away", "moneyline_home"
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

parse_record_diff <- function(record_text) {
	parts <- strsplit(as.character(record_text), "-", fixed = TRUE)
	wins <- vapply(parts, function(x) if (length(x) >= 1) suppressWarnings(as.numeric(x[1])) else NA_real_, numeric(1))
	losses <- vapply(parts, function(x) if (length(x) >= 2) suppressWarnings(as.numeric(x[2])) else NA_real_, numeric(1))
	wins - losses
}

# Team win-loss differential can be negative; keep the sign.
nba$win_loss_diff_away <- parse_record_diff(nba$win_loss_away)
nba$win_loss_diff_home <- parse_record_diff(nba$win_loss_home)

# Determine winning side from scores.
winner_side <- ifelse(
	nba$score_home > nba$score_away,
	"home",
	ifelse(nba$score_away > nba$score_home, "away", NA)
)

# Winner-only features.
nba$winner_win_loss_diff <- ifelse(
	winner_side == "home",
	nba$win_loss_diff_home,
	ifelse(winner_side == "away", nba$win_loss_diff_away, NA_real_)
)
nba$winner_moneyline <- ifelse(
	winner_side == "home",
	nba$moneyline_home,
	ifelse(winner_side == "away", nba$moneyline_away, NA_real_)
)

# Explicit NA removal before model.
model_df <- nba[stats::complete.cases(nba[, c("winner_win_loss_diff", "winner_moneyline")]), ]
if (nrow(model_df) < 10) {
	stop("Not enough complete winner rows to run regression (need at least 10).")
}
fit <- stats::lm(winner_win_loss_diff ~ winner_moneyline, data = model_df)
fit_summary <- summary(fit)

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "outputEDA")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_plot <- file.path(out_dir, "winner_win_loss_diff_vs_winner_odds.png")

draw_plot <- function() {
	plot(
		model_df$winner_moneyline,
		model_df$winner_win_loss_diff,
		pch = 16,
		cex = 0.45,
		col = rgb(0.2, 0.35, 0.65, 0.35),
		xlab = "Winner betting odds (American moneyline)",
		ylab = "Winner win-loss differential (wins - losses)",
		main = "Winning team's record differential vs winner betting odds"
	)
	abline(fit, col = "firebrick", lwd = 2)
	legend(
		"topleft",
		legend = c(
			paste0("n = ", nrow(model_df)),
			paste0("R^2 = ", round(fit_summary$r.squared, 4)),
			paste0("p(slope) = ", signif(fit_summary$coefficients["winner_moneyline", "Pr(>|t|)"], 4))
		),
		bty = "n"
	)
}

png(out_plot, width = 9, height = 6, units = "in", res = 150)
draw_plot()
dev.off()

if (interactive()) {
	draw_plot()
}

cat("Source:", normalizePath(csv_path), "\n")
cat("Rows:", nrow(nba), " Columns:", ncol(nba), "\n")
cat("Winner rows in model:", nrow(model_df), "\n\n")
cat("Regression model:\n")
print(fit_summary)
cat("\nSaved outputs:\n")
cat(" -", normalizePath(out_plot), "\n")

invisible(list(
	model = fit,
	model_data = model_df
))
