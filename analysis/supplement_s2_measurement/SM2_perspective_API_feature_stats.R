# Purpose
# Calculate descriptive statistics for Perspective API discourse features.
# Reports prevalence of each feature, distribution statistics.
#
# Reference: SI Appendix Section 2.2

# Setup
rm(list = ls())

# Load data
source("analysis/setup/load_data.R")

# Calculate feature-level statistics
cat("=== PERSPECTIVE API FEATURE STATISTICS ===\n\n")

# Constructive features
constructive_features <- c("prob_compassion", "prob_curiosity", "prob_nuance", 
                          "prob_personal_story", "prob_reasoning")

# Destructive features
destructive_features <- c("prob_toxic", "prob_identity_attack", "prob_threat",
                        "prob_attack_on_author", "prob_attack_on_commenter")

all_features <- c(constructive_features, destructive_features)

feature_stats <- data.frame(
  feature = character(),
  mean_prob = numeric(),
  median_prob = numeric(),
  sd_prob = numeric(),
  prevalence_binary = numeric(),
  stringsAsFactors = FALSE
)

for (feat in all_features) {
  if (feat %in% colnames(joined_data)) {
    mean_prob <- mean(joined_data[[feat]], na.rm = TRUE)
    median_prob <- median(joined_data[[feat]], na.rm = TRUE)
    sd_prob <- sd(joined_data[[feat]], na.rm = TRUE)
    prevalence_binary <- mean(joined_data[[feat]] >= 0.6, na.rm = TRUE) * 100
    
    feature_stats <- rbind(feature_stats, data.frame(
      feature = feat,
      mean_prob = mean_prob,
      median_prob = median_prob,
      sd_prob = sd_prob,
      prevalence_binary = prevalence_binary
    ))
    
    cat(sprintf("%s: Mean = %.3f, Prevalence (≥0.6) = %.1f%%\n",
                feat, mean_prob, prevalence_binary))
  }
}

# Save results
results <- list(
  feature_statistics = feature_stats,
  n = nrow(joined_data)
)

saveRDS(results, "analysis/supplement_s2_measurement/SM2_perspective_API_feature_stats_results.rds")

cat("\n=== RESULTS SAVED ===\n")
