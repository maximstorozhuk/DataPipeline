# Shared data prep for feature-selection scripts (numeric matrix + home_win).
# Lives under r/featureSelection/; source from repo root or r/ (CSV via candidates).

prepare_standard_nba_xy <- function(csv_path) {
  nba <- utils::read.csv(csv_path, na.strings = c("", "NA"), stringsAsFactors = FALSE)
  for (nm in names(nba)) {
    x <- nba[[nm]]
    if (!is.character(x)) next
    u <- unique(stats::na.omit(x))
    if (length(u) == 0) next
    if (all(tolower(u) %in% c("true", "false"))) {
      nba[[nm]] <- ifelse(is.na(x), NA, tolower(x) == "true")
    }
  }
  tie <- nba$score_home == nba$score_away
  nba <- nba[!tie, , drop = FALSE]
  nba$home_win <- as.integer(nba$score_home > nba$score_away)
  nba$date_num <- suppressWarnings(as.numeric(as.Date(nba$date)))

  leaky <- c(
    "score_away",
    "score_home",
    "home_win",
    grep("^q[1-4]_", names(nba), value = TRUE),
    grep("^ot_", names(nba), value = TRUE)
  )
  exclude_extra <- c(
    "date",
    "h2_spread",
    "id_spread",
    grep("^implied_odds", names(nba), value = TRUE),
    grep("^profit_", names(nba), value = TRUE)
  )
  cand <- setdiff(names(nba), c(leaky, exclude_extra))
  use <- cand[vapply(cand, function(nm) {
    xx <- nba[[nm]]
    is.numeric(xx) || is.logical(xx) || is.integer(xx)
  }, logical(1))]

  x_list <- lapply(use, function(nm) {
    v <- nba[[nm]]
    if (is.logical(v)) as.numeric(v) else v
  })
  X <- do.call(cbind, x_list)
  colnames(X) <- use
  ok <- stats::complete.cases(X, nba$home_win)
  X <- X[ok, , drop = FALSE]
  y <- nba$home_win[ok]
  var_sd <- apply(X, 2L, stats::sd, na.rm = TRUE)
  keep <- !is.na(var_sd) & var_sd > 1e-8
  X <- X[, keep, drop = FALSE]

  repo_root <- dirname(dirname(normalizePath(csv_path)))
  list(nba = nba, X = X, y = y, feature_names = colnames(X), repo_root = repo_root)
}

# r/output/featureSelection/<method>/...
fs_output_dir <- function(repo_root, ...) {
  file.path(repo_root, "r", "output", "featureSelection", ...)
}
