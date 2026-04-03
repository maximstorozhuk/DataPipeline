if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop("Install glmnet: install.packages(\"glmnet\")", call. = FALSE)
}

RIDGE_TOP_K <- 20L

csv_candidates <- c(
  file.path("csv", "nba_2008-2025_extended.csv"),
  file.path("..", "csv", "nba_2008-2025_extended.csv")
)
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) {
  stop("Cannot find nba_2008-2025_extended.csv (expected csv/ under the project root).")
}

cat("Loading CSV (Ridge feature selection)...\n")
flush.console()
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

tie <- nba$score_home == nba$score_away
nba <- nba[!tie, , drop = FALSE]
nba$home_win <- as.integer(nba$score_home > nba$score_away)

leaky <- c(
  "score_away",
  "score_home",
  "home_win",
  grep("^q[1-4]_", names(nba), value = TRUE),
  grep("^ot_", names(nba), value = TRUE)
)

nba$date_num <- suppressWarnings(as.numeric(as.Date(nba$date)))

exclude_extra <- c(
  "date",
  "h2_spread",
  "id_spread",
  grep("^implied_odds", names(nba), value = TRUE),
  grep("^profit_", names(nba), value = TRUE)
)

cand <- setdiff(names(nba), c(leaky, exclude_extra))
use <- cand[vapply(cand, function(nm) {
  x <- nba[[nm]]
  is.numeric(x) || is.logical(x) || is.integer(x)
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
cat("n =", length(y), " p =", ncol(X), "\n")
flush.console()

if (ncol(X) < 2L || length(y) < 100L) {
  stop("Too few complete rows or predictors for glmnet.")
}

set.seed(42L)
cv <- glmnet::cv.glmnet(
  X,
  y,
  family = "binomial",
  alpha = 0,
  nfolds = 10L,
  standardize = TRUE,
  type.measure = "class"
)

coef_min <- stats::coef(cv, s = "lambda.min")
cm <- as.matrix(coef_min)
nm <- rownames(cm)
cf <- as.numeric(cm[, 1L])
names(cf) <- nm
cf_pred <- cf[names(cf) != "(Intercept)"]

ord <- order(-abs(cf_pred))
k_take <- min(RIDGE_TOP_K, as.integer(length(cf_pred)))
top_idx <- ord[seq_len(k_take)]
selected_names <- names(cf_pred)[top_idx]

selected_df <- data.frame(
  feature = selected_names,
  coefficient = cf_pred[selected_names],
  abs_coefficient = abs(cf_pred[selected_names]),
  rank = seq_len(k_take),
  method = "ridge_top_k_at_lambda.min",
  stringsAsFactors = FALSE
)

full_df <- data.frame(
  feature = names(cf_pred),
  coefficient = as.numeric(cf_pred),
  abs_coefficient = abs(as.numeric(cf_pred)),
  selected_for_model = names(cf_pred) %in% selected_names,
  stringsAsFactors = FALSE
)
full_df <- full_df[order(-full_df$abs_coefficient), ]
row.names(full_df) <- NULL

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "output", "featureSelection", "ridge_featureselection")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_sel <- file.path(out_dir, "ridge_selected_features.csv")
out_full <- file.path(out_dir, "ridge_coefficients_lambda.min.csv")
out_lam <- file.path(out_dir, "ridge_cv_summary.csv")
out_cv <- file.path(out_dir, "ridge_cv_plot.png")
out_path <- file.path(out_dir, "ridge_coef_path.png")

write.csv(selected_df, out_sel, row.names = FALSE)
write.csv(full_df, out_full, row.names = FALSE)
utils::write.csv(
  data.frame(feature = selected_df$feature),
  file.path(out_dir, "selected_features.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    method = "ridge",
    alpha = 0,
    lambda_min = as.numeric(cv$lambda.min),
    lambda_1se = as.numeric(cv$lambda.1se),
    n_predictors_in_model = k_take,
    ridge_top_k = RIDGE_TOP_K,
    stringsAsFactors = FALSE
  ),
  out_lam,
  row.names = FALSE
)

grDevices::png(out_cv, width = 9, height = 6, units = "in", res = 120)
graphics::plot(cv)
grDevices::dev.off()

grDevices::png(out_path, width = 9, height = 6, units = "in", res = 120)
graphics::plot(cv$glmnet.fit, xvar = "lambda", label = TRUE)
grDevices::dev.off()

cat("\nRidge (alpha=0): top", k_take, "predictors by |coef| at lambda.min\n")
cat("lambda.min:", cv$lambda.min, "\n")
cat("\nSaved:\n")
cat(" ", normalizePath(out_sel), "\n")
cat(" ", normalizePath(out_full), "\n")
cat(" ", normalizePath(out_lam), "\n")
invisible(list(cv = cv, selected = selected_df, full = full_df))
