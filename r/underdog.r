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

ma <- nba$moneyline_away
mh <- nba$moneyline_home
wf <- trimws(tolower(as.character(nba$whos_favored)))

sa <- nba$score_away
sh <- nba$score_home
tie <- sa == sh
away_won <- sa > sh
home_won <- sh > sa

fav_is_away <- ifelse(
  is.na(ma) | is.na(mh),
  NA,
  ifelse(ma == mh, wf == "away", ma < mh)
)

fav_ml <- ifelse(fav_is_away, ma, mh)
dog_ml <- ifelse(fav_is_away, mh, ma)
fav_won <- ifelse(fav_is_away, away_won, home_won)
dog_won <- ifelse(fav_is_away, home_won, away_won)

profit_favorite <- mapply(profit_one, fav_ml, fav_won, tie)
profit_underdog <- mapply(profit_one, dog_ml, dog_won, tie)

n_bets <- sum(!is.na(profit_favorite))
total_profit_favorite <- sum(profit_favorite, na.rm = TRUE)
total_profit_underdog <- sum(profit_underdog, na.rm = TRUE)

summary_df <- data.frame(
  strategy = c("Bet $10 on favorite (ML) each game", "Bet $10 on underdog (ML) each game"),
  total_profit = round(c(total_profit_favorite, total_profit_underdog), 2),
  n_games = c(n_bets, n_bets),
  stringsAsFactors = FALSE
)

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_png <- file.path(out_dir, "underdog.png")

cat("Stake per bet: $", STAKE, "\n", sep = "")
cat("Games with both moneylines and a resolved favorite:", n_bets, "\n")
cat("(Pick'ems use whos_favored; games with missing ML excluded.)\n\n")
cat("Total profit — favorite strategy: $", round(total_profit_favorite, 2), "\n", sep = "")
cat("Total profit — underdog strategy: $", round(total_profit_underdog, 2), "\n\n", sep = "")
print(summary_df, row.names = FALSE)

if (requireNamespace("gridExtra", quietly = TRUE)) {
  png(
    out_png,
    width = 10,
    height = 4,
    units = "in",
    res = 150
  )
  grid::grid.newpage()
  gridExtra::grid.table(
    summary_df,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(base_size = 11)
  )
  dev.off()
} else {
  warning(
    "Package gridExtra is not installed; skipped PNG. Install with: install.packages(\"gridExtra\")",
    call. = FALSE
  )
}

if (file.exists(out_png)) {
  cat("Saved image:", normalizePath(out_png), "\n")
}

invisible(list(
  total_profit_favorite = total_profit_favorite,
  total_profit_underdog = total_profit_underdog,
  n_games = n_bets,
  summary = summary_df
))
