# Purpose
# Binary prevalence: proportion of comments with ≥1 constructive or destructive
# feature (Perspective API prob ≥ 0.6). Uses either individual prob_* columns or
# the label-based index (index > 0) from load_data.R.
#
# Reference: Main text line 56
# Reports: 52.8% ≥1 constructive, 20.0% ≥1 destructive, ~2.6-fold
# Data: Canonical joined_data; index is label-based when prob_* exist.

# Setup
rm(list = ls())

# Load data
# Canonical load; harmoniousness_raw/divisiveness_raw = mean of binary labels (0.6 threshold).
source("analysis/setup/load_data.R")

# Calculate binary prevalence
# Manuscript: "≥1 constructive feature" = at least one of the 5 constructive
# features has Perspective API probability ≥ 0.6 (same for destructive).
# Use individual prob_* columns when present; else index > 0 (and warn).
cat("=== BINARY PREVALENCE RATES ===\n\n")

constructive_cols <- c("prob_compassion", "prob_curiosity", "prob_nuance", "prob_personal_story", "prob_reasoning")
destructive_cols <- c("prob_toxic", "prob_identity_attack", "prob_threat", "prob_attack_on_author", "prob_attack_on_commenter")
has_features <- all(constructive_cols %in% colnames(joined_data)) && all(destructive_cols %in% colnames(joined_data))

if (has_features) {
  # Definition that matches manuscript: ≥1 feature with prob ≥ 0.6
  has_any_C <- rowSums(joined_data[, constructive_cols] >= 0.6, na.rm = TRUE) >= 1
  has_any_D <- rowSums(joined_data[, destructive_cols] >= 0.6, na.rm = TRUE) >= 1
  prop_C <- mean(has_any_C, na.rm = TRUE) * 100
  prop_D <- mean(has_any_D, na.rm = TRUE) * 100
  cat("(Using feature-level 0.6 threshold: prob_compassion, ..., prob_toxic, ...)\n")
} else {
  # Fallback: index > 0 (can give 100%/100% if index is continuous mean probability)
  prop_C <- mean(joined_data$harmoniousness_raw > 0, na.rm = TRUE) * 100
  prop_D <- mean(joined_data$divisiveness_raw > 0, na.rm = TRUE) * 100
  warning("Individual feature columns (prob_compassion, etc.) not found. Using index > 0. Manuscript 52.8%% / 20.0%% use feature-level 0.6 threshold; if you see 100%%/100%%, add those columns to canonical data.")
}

cat(sprintf("Proportion with ≥1 constructive feature: %.1f%%\n", prop_C))
cat(sprintf("Proportion with ≥1 destructive feature: %.1f%%\n", prop_D))
cat(sprintf("Ratio: %.1fx more comments have constructive features\n\n", prop_C / max(prop_D, 0.1)))

# Additional breakdowns
cat("=== ADDITIONAL BREAKDOWNS ===\n")

# Mean number of features (≥0.6) per comment
if (has_features) {
  n_C_per_comment <- rowSums(joined_data[, constructive_cols] >= 0.6, na.rm = TRUE)
  n_D_per_comment <- rowSums(joined_data[, destructive_cols] >= 0.6, na.rm = TRUE)
  mean_C_features <- mean(n_C_per_comment, na.rm = TRUE)
  mean_D_features <- mean(n_D_per_comment, na.rm = TRUE)
} else {
  mean_C_features <- mean(joined_data$harmoniousness_raw * 5, na.rm = TRUE)
  mean_D_features <- mean(joined_data$divisiveness_raw * 5, na.rm = TRUE)
}

cat(sprintf("Mean constructive features per comment: %.2f out of 5\n", mean_C_features))
cat(sprintf("Mean destructive features per comment: %.2f out of 5\n", mean_D_features))

# Save results
results <- list(
  prop_with_C = prop_C,
  prop_with_D = prop_D,
  ratio = prop_C / max(prop_D, 0.1),
  mean_C_features = mean_C_features,
  mean_D_features = mean_D_features,
  n = nrow(joined_data),
  used_feature_columns = has_features
)

saveRDS(results, "analysis/prevalence/RQ1_binary_prevalence_rates_results.rds")

cat("\n=== RESULTS SAVED ===\n")
