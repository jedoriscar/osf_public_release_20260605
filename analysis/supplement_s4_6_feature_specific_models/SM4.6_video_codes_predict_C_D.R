# Optional video-code sensitivity analysis.
# This script aggregates any public video/content-code fields available in the
# analytic data and tests whether they predict video-level mean constructiveness
# and destructiveness. If no public video-code fields are present, the script
# records that boundary rather than estimating a model.
rm(list = ls())
library(tidyverse)

source("analysis/setup/load_data.R")

# Candidate video-level/content-code fields; comment-level fields are aggregated
# to video-level means before modeling.
content_code_candidates <- c(
  "diversity_positive_framing", "diversity_negative_framing",
  "multiracial_positive", "multiracial_negative",
  "political_implication", "commentary_fear", "commentary_toxicity",
  "commentary_marginalization", "commentary_inclusion",
  "statistics_reference", "fallacies", "changing_definition_race",
  "race_essentialism", "immigration", "population_change",
  "white_pop_decrease", "poc_pop_increase",
  "latin_mention", "asian_mention", "black_mention", "multiracial_mention", "poc_mention"
)

# Which exist in joined_data?
present_codes <- intersect(content_code_candidates, colnames(joined_data))
cat("=== VIDEO CODES PREDICTING C AND D ===\n\n")
cat("Content-code columns found in joined_data:", length(present_codes), "\n")
if (length(present_codes) > 0) cat(paste(present_codes, collapse = ", "), "\n\n")

# Optional external video-framing file if a reviewer receives it separately.
framing_file <- "supplemental_materials/04_extracted_data/video_framing_codes.csv"
video_framing <- NULL
if (file.exists(framing_file)) {
  video_framing <- read.csv(framing_file, stringsAsFactors = FALSE)
  if ("video_id" %in% colnames(video_framing)) {
    cat("Merged video_framing_codes.csv by video_id.\n")
  } else {
    video_framing <- NULL
    cat("video_framing_codes.csv found but no video_id column; using content codes only.\n")
  }
} else {
  cat("No separate video_framing_codes.csv found; using public data columns only.\n")
}

# Aggregate to video level: mean C, mean D, and mean of each content code
video_agg <- joined_data %>%
  group_by(video_id) %>%
  summarise(
    mean_C = mean(harmoniousness_raw, na.rm = TRUE),
    mean_D = mean(divisiveness_raw, na.rm = TRUE),
    n_comments = n(),
    across(any_of(present_codes), ~ mean(.x, na.rm = TRUE), .names = "avg_{.col}"),
    .groups = "drop"
  )
# Flatten names: avg_diversity_positive_framing etc. are now the predictor columns

# Merge external framing if available
if (!is.null(video_framing) && "video_id" %in% colnames(video_framing)) {
  video_agg <- video_agg %>%
    left_join(video_framing, by = "video_id")
}

# Predictor columns for regression (exclude identifiers and outcomes)
pred_cols <- setdiff(colnames(video_agg), c("video_id", "mean_C", "mean_D", "n_comments"))
pred_cols <- pred_cols[!grepl("^title|^url|^description|^channel", pred_cols, ignore.case = TRUE)]
# Only numeric
numeric_preds <- pred_cols[sapply(video_agg[, pred_cols, drop = FALSE], function(x) is.numeric(x) && !all(is.na(x)))]
if (length(numeric_preds) == 0) {
  cat("No numeric video-code predictors available. Skipping models.\n")
  cat("This optional sensitivity model is estimated only when public video/content-code predictors are available.\n")
} else {
  # Drop rows with any NA in predictors for clean lm
  video_lm <- video_agg %>%
    select(video_id, mean_C, mean_D, n_comments, all_of(numeric_preds)) %>%
    filter(complete.cases(select(., mean_C, mean_D, all_of(numeric_preds))))

  cat("Videos with complete data:", nrow(video_lm), "\n\n")

  f_C <- as.formula(paste("mean_C ~", paste(numeric_preds, collapse = " + ")))
  f_D <- as.formula(paste("mean_D ~", paste(numeric_preds, collapse = " + ")))

  mod_C <- lm(f_C, data = video_lm)
  mod_D <- lm(f_D, data = video_lm)

  cat("=== MODEL: Mean constructiveness (C) ~ video codes ===\n")
  print(summary(mod_C))
  cat("\n=== MODEL: Mean destructiveness (D) ~ video codes ===\n")
  print(summary(mod_D))

  # Summary table of significant predictors
  coef_C <- summary(mod_C)$coefficients
  coef_D <- summary(mod_D)$coefficients
  sig_C <- rownames(coef_C)[coef_C[, "Pr(>|t|)"] < 0.05]
  sig_D <- rownames(coef_D)[coef_D[, "Pr(>|t|)"] < 0.05]
  cat("\nPredictors significant for C (p < .05):", paste(sig_C[sig_C != "(Intercept)"], collapse = ", "), "\n")
  cat("Predictors significant for D (p < .05):", paste(sig_D[sig_D != "(Intercept)"], collapse = ", "), "\n")
}

# Save results for reporting
results <- list(
  n_videos = nrow(video_agg),
  n_videos_complete = if (exists("video_lm")) nrow(video_lm) else NA,
  present_codes = present_codes,
  numeric_predictors = if (exists("numeric_preds")) numeric_preds else character(0),
  model_C = if (exists("mod_C")) mod_C else NULL,
  model_D = if (exists("mod_D")) mod_D else NULL
)
saveRDS(results, "analysis/supplement_s4_6_feature_specific_models/SM4.6_video_codes_predict_C_D_results.rds")
cat("\n=== RESULTS SAVED ===\n")
