if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop("Install glmnet: install.packages(\"glmnet\")", call. = FALSE)
}

# alpha = 1 is pure Lasso (very sparse). alpha in (0,1) is elastic net — often keeps
# more correlated predictors. Increase toward 1 for sparser fits.
GLMNET_ALPHA <- 0.88

csv_candidates <- c(
  file.path("csv", "nba_2008-2025_extended.csv"),
  file.path("..", "csv", "nba_2008-2025_extended.csv")
)
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) {
  stop("Cannot find nba_2008-2025_extended.csv (expected csv/ under the project root).")
}

cat("Loading CSV...\n")
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

# Drop near-constant columns; glmnet will standardize internally
x_list <- lapply(use, function(nm) {
  v <- nba[[nm]]
  if (is.logical(v)) {
    as.numeric(v)
  } else {
    v
  }
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
  alpha = GLMNET_ALPHA,
  nfolds = 10L,
  standardize = TRUE,
  type.measure = "class"
)

coef_min <- stats::coef(cv, s = "lambda.min")
coef_1se <- stats::coef(cv, s = "lambda.1se")
lambda_smallest <- min(cv$glmnet.fit$lambda)
coef_path_min <- stats::coef(cv, s = lambda_smallest)

to_df <- function(cm, label) {
  m <- as.matrix(cm)
  data.frame(
    lambda = label,
    feature = rownames(m),
    coefficient = as.numeric(m[, 1L]),
    stringsAsFactors = FALSE
  )
}

tab <- rbind(
  to_df(coef_min, "lambda.min"),
  to_df(coef_1se, "lambda.1se"),
  to_df(coef_path_min, "lambda.path.min")
)
tab <- tab[order(tab$lambda, -abs(tab$coefficient)), ]
row.names(tab) <- NULL

repo_root <- dirname(dirname(normalizePath(csv_path)))
out_dir <- file.path(repo_root, "r", "output", "l1regularization")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_coef <- file.path(out_dir, "l1regularization_coefficients.csv")
out_lambda <- file.path(out_dir, "l1regularization_lambda.csv")
out_cvplot <- file.path(out_dir, "l1regularization_cv.png")
out_path <- file.path(out_dir, "l1regularization_coef_path.png")

write.csv(tab, out_coef, row.names = FALSE)

cm_min <- as.matrix(coef_min)
cm_1se <- as.matrix(coef_1se)
cm_path <- as.matrix(coef_path_min)
lam_df <- data.frame(
  glmnet_alpha = GLMNET_ALPHA,
  lambda_min = as.numeric(cv$lambda.min),
  lambda_1se = as.numeric(cv$lambda.1se),
  lambda_path_min = as.numeric(lambda_smallest),
  n_nonzero_min = sum(abs(cm_min[-1L, 1L]) > 1e-8),
  n_nonzero_1se = sum(abs(cm_1se[-1L, 1L]) > 1e-8),
  n_nonzero_path_min = sum(abs(cm_path[-1L, 1L]) > 1e-8),
  stringsAsFactors = FALSE
)
write.csv(lam_df, out_lambda, row.names = FALSE)

grDevices::png(out_cvplot, width = 9, height = 6, units = "in", res = 120)
graphics::plot(cv)
grDevices::dev.off()

grDevices::png(out_path, width = 9, height = 6, units = "in", res = 120)
graphics::plot(cv$glmnet.fit, xvar = "lambda", label = TRUE)
grDevices::dev.off()

cat("\nglmnet alpha (1=Lasso, <1=elastic net):", GLMNET_ALPHA, "\n")
cat("lambda.min (CV misclassification):", cv$lambda.min, "\n")
cat("lambda.1se:", cv$lambda.1se, "\n")
cat("lambda.path.min (weakest penalty on path; most nonzero coefs):", lambda_smallest, "\n")
cat("\nSaved:\n")
cat(" ", normalizePath(out_coef), "\n")
cat(" ", normalizePath(out_lambda), "\n")
cat(" ", normalizePath(out_cvplot), "\n")
cat(" ", normalizePath(out_path), "\n")

invisible(list(cv = cv, coefficients = tab, lambda = lam_df))
