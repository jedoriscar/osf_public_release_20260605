# Purpose
# Replicate RQ1 (prevalence) analyses in climate change dataset.
# Tests whether constructiveness exceeds destructiveness in climate discourse.
#
# Reference: Main text lines 184-185
# Expected: C = 29.0%, D = 12.0%, d = 0.79

# Setup
rm(list = ls())

# Load climate data
climate_data_path <- "data/analysis_objects/climate_comments.rds"

if (!file.exists(climate_data_path)) {
  stop("Climate data file not found. Expected at: ", climate_data_path)
}

joined_data <- readRDS(climate_data_path)

# Data checks (align with racial RQ1)
cat("=== CLIMATE REPLICATION: RQ1 PREVALENCE ===\n")
cat("N comments:", nrow(joined_data), "\n")
mean_C <- mean(joined_data$harmoniousness_raw, na.rm = TRUE)
mean_D <- mean(joined_data$divisiveness_raw, na.rm = TRUE)
cat("Mean constructiveness:", round(mean_C, 4), "\n")
cat("Mean destructiveness:", round(mean_D, 4), "\n\n")

# Paired t-test
ttest_result <- t.test(
  joined_data$harmoniousness_raw,
  joined_data$divisiveness_raw,
  paired = TRUE
)

mean_diff <- mean_C - mean_D
sd_diff <- sd(joined_data$harmoniousness_raw - joined_data$divisiveness_raw, na.rm = TRUE)
cohens_d <- mean_diff / sd_diff

cat("=== PAIRED T-TEST: CONSTRUCTIVENESS VS. DESTRUCTIVENESS ===\n\n")
cat("Paired t-test results:\n")
cat(sprintf("  t(%d) = %.2f, p < .001\n", ttest_result$parameter, ttest_result$statistic))
cat(sprintf("  Mean difference = %.3f\n", mean_diff))
cat(sprintf("  Cohen's d = %.2f\n", cohens_d))
cat(sprintf("  95%% CI: [%.3f, %.3f]\n", ttest_result$conf.int[1], ttest_result$conf.int[2]))

# Descriptive statistics (align with racial RQ1)
sd_C <- sd(joined_data$harmoniousness_raw, na.rm = TRUE)
sd_D <- sd(joined_data$divisiveness_raw, na.rm = TRUE)
cat("\n=== DESCRIPTIVE STATISTICS ===\n")
cat(sprintf("Constructiveness: M = %.3f, SD = %.3f\n", mean_C, sd_C))
cat(sprintf("Destructiveness: M = %.3f, SD = %.3f\n", mean_D, sd_D))

# Binary prevalence
prop_C <- mean(joined_data$harmoniousness_raw > 0, na.rm = TRUE) * 100
prop_D <- mean(joined_data$divisiveness_raw > 0, na.rm = TRUE) * 100
cat(sprintf("\nProportion with ≥1 constructive feature: %.1f%%\n", prop_C))
cat(sprintf("Proportion with ≥1 destructive feature: %.1f%%\n", prop_D))

# Save results
results <- list(
  t_test = ttest_result,
  mean_C = mean_C,
  mean_D = mean_D,
  sd_C = sd_C,
  sd_D = sd_D,
  cohens_d = cohens_d,
  prop_C = prop_C,
  prop_D = prop_D,
  n = nrow(joined_data)
)

saveRDS(results, "analysis/climate_replication/CLIMATE_RQ1_prevalence_results.rds")

cat("\n=== RESULTS SAVED ===\n")
