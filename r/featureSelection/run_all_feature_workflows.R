# Run all feature-selection scripts, then rolling backtests (requires packages).
# From repo root: Rscript r/featureSelection/run_all_feature_workflows.R
# Uses tryCatch so one failure does not stop the rest.

repo <- getwd()
if (!file.exists(file.path(repo, "csv", "nba_2008-2025_extended.csv"))) {
  stop("Run from DataPipeline repo root (csv/ should exist).", call. = FALSE)
}

fs_scripts <- c(
  "l1regularization.r",
  "filter_univariate_featureselection.r",
  "bic_stepwise_featureselection.r",
  "stability_featureselection.r",
  "ridge_featureselection.r",
  "rf_featureselection.r",
  "boruta_featureselection.r",
  "pca_loading_featureselection.r",
  "rfe_caret_featureselection.r"
)

subs <- c(
  "l1regularization",
  "filter_univariate",
  "bic_stepwise",
  "stability_lasso",
  "ridge_featureselection",
  "rf_importance",
  "boruta",
  "pca_loading",
  "rfe_caret"
)

message("=== Feature selection scripts ===")
for (f in fs_scripts) {
  fp <- file.path("r", "featureSelection", f)
  if (!file.exists(fp)) next
  message("\n>> ", f)
  tryCatch(
    source(fp, local = TRUE),
    error = function(e) warning(conditionMessage(e), call. = FALSE)
  )
}

rscript <- Sys.which("Rscript")
if (!nzchar(rscript)) {
  rh <- Sys.getenv("R_HOME")
  if (nzchar(rh)) {
    rscript <- file.path(rh, "bin", if (.Platform$OS.type == "windows") {
      "Rscript.exe"
    } else {
      "Rscript"
    })
  }
}
run_sm <- normalizePath(file.path(repo, "r", "featureSelection", "run_selectionsmodel.R"))

message("\n=== Rolling selection models (run_selectionsmodel.R) ===")
if (!nzchar(rscript) || !file.exists(rscript)) {
  warning(
    "Rscript not found; run manually: Rscript r/featureSelection/run_selectionsmodel.R <subdir>",
    call. = FALSE
  )
} else {
  for (s in subs) {
    message("\n>> ", s)
    tryCatch(
      {
        out <- system2(rscript, c(run_sm, s), stdout = TRUE, stderr = TRUE)
        if (length(out)) cat(paste(out, collapse = "\n"), "\n")
      },
      error = function(e) warning(conditionMessage(e), call. = FALSE)
    )
  }
}

l1_sm <- file.path("r", "featureSelection", "l1selectionsmodel.r")
if (file.exists(l1_sm)) {
  message("\n=== L1 selection model (FEATURE_LAMBDA / AUTO_LENIENT) ===")
  tryCatch(
    source(l1_sm, local = TRUE),
    error = function(e) warning(conditionMessage(e), call. = FALSE)
  )
}

cmp <- file.path("r", "featureSelection", "compare_all_selectionsmodels.R")
if (file.exists(cmp)) {
  message("\n=== Comparison: all techniques (CSV + scatter plot) ===")
  tryCatch(
    source(cmp, local = TRUE),
    error = function(e) warning(conditionMessage(e), call. = FALSE)
  )
}

message("\nDone.")
