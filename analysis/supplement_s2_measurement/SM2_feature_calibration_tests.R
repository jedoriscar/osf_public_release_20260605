# Purpose
# Test whether Perspective API feature calibration differs between
# constructive and destructive features. Addresses potential calibration artifacts.
#
# Reference: SI Appendix Section 2.2, Main text lines 267-268

# Setup
rm(list = ls())

# Load data
source("analysis/setup/load_data.R")

# Compare feature score distributions
cat("=== FEATURE CALIBRATION TESTS ===\n\n")

# Constructive features
constructive_features <- c("prob_compassion", "prob_curiosity", "prob_nuance", 
                          "prob_personal_story", "prob_reasoning")

# Destructive features
destructive_features <- c("prob_toxic", "prob_identity_attack", "prob_threat",
                        "prob_attack_on_author", "prob_attack_on_commenter")

# Calculate mean probabilities
mean_constructive <- mean(sapply(constructive_features, function(f) {
  if (f %in% colnames(joined_data)) {
    mean(joined_data[[f]], na.rm = TRUE)
  } else {
    NA
  }
}), na.rm = TRUE)

mean_destructive <- mean(sapply(destructive_features, function(f) {
  if (f %in% colnames(joined_data)) {
    mean(joined_data[[f]], na.rm = TRUE)
  } else {
    NA
  }
}), na.rm = TRUE)

cat("Mean probability scores:\n")
cat(sprintf("  Constructive features: %.3f\n", mean_constructive))
cat(sprintf("  Destructive features: %.3f\n", mean_destructive))
cat(sprintf("  Difference: %.3f\n", mean_constructive - mean_destructive))

# Save results
results <- list(
  mean_constructive = mean_constructive,
  mean_destructive = mean_destructive,
  difference = mean_constructive - mean_destructive
)

saveRDS(results, "analysis/supplement_s2_measurement/SM2_feature_calibration_tests_results.rds")

cat("\n=== RESULTS SAVED ===\n")
