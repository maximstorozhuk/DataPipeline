STAKE <- 10

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

profit_one <- function(odds, won, tie) {
  if (tie) {
    return(0)
  }
  if (is.na(odds)) {
    return(NA_real_)
  }
  if (won) {
    if (odds < 0) {
      return(STAKE * (100 / abs(odds)))
    }
    return(STAKE * (odds / 100))
  }
  -STAKE
}

sa <- nba$score_away
sh <- nba$score_home
tie <- sa == sh
away_won <- sa > sh
home_won <- sh > sa

away_profit <- mapply(
  profit_one,
  nba$moneyline_away,
  away_won,
  tie,
  SIMPLIFY = TRUE
)
home_profit <- mapply(
  profit_one,
  nba$moneyline_home,
  home_won,
  tie,
  SIMPLIFY = TRUE
)

away_df <- data.frame(team = nba$away, profit = away_profit, stringsAsFactors = FALSE)
home_df <- data.frame(team = nba$home, profit = home_profit, stringsAsFactors = FALSE)
long_df <- rbind(away_df, home_df)
long_df <- long_df[!is.na(long_df$profit), ]

sums <- aggregate(profit ~ team, long_df, sum, na.rm = TRUE)
names(sums)[names(sums) == "profit"] <- "total_profit"
counts <- aggregate(profit ~ team, long_df, length)
names(counts)[names(counts) == "profit"] <- "n_games_with_line"
by_team <- merge(sums, counts, by = "team")
by_team$total_profit <- round(by_team$total_profit, 2)
by_team <- by_team[order(-by_team$total_profit, by_team$team), ]
row.names(by_team) <- NULL

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_png <- file.path(out_dir, "teambyteam.png")

if (requireNamespace("gridExtra", quietly = TRUE)) {
  png(
    out_png,
    width = 12,
    height = max(6, nrow(by_team) * 0.35 + 1.5),
    units = "in",
    res = 150
  )
  grid::grid.newpage()
  gridExtra::grid.table(
    by_team,
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

cat("Stake per game: $", STAKE, " (moneyline win only)\n", sep = "")
cat("Source:", normalizePath(csv_path), "\n")
cat("Games counted: rows with non-missing moneyline for that team in that game.\n\n")
print(by_team, row.names = FALSE)
if (file.exists(out_png)) {
  cat("\nSaved image:", normalizePath(out_png), "\n")
}

invisible(by_team)
