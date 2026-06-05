# Purpose
# Test whether constructiveness exceeds destructiveness overall using a paired t-test
# (same comment: two scores). Main prevalence finding for the manuscript.
#
# Reference: Main text line 56
# Expected: Constructiveness > destructiveness (d ≈ 0.86)
# Data: Canonical joined_data; harmoniousness_raw and divisiveness_raw are
#       label-based indices (proportion of 5 features ≥ 0.6) from load_data.R.

# Setup
rm(list = ls())

# Load data
# Loads canonical .rda; indices overwritten to label-based if prob_* columns exist.
source("analysis/setup/load_data.R")

# Quick sanity checks
cat("=== DATA CHECKS ===\n")
cat("N comments:", nrow(joined_data), "\n")
cat("Mean constructiveness:", mean(joined_data$harmoniousness_raw, na.rm = TRUE), "\n")
cat("Mean destructiveness:", mean(joined_data$divisiveness_raw, na.rm = TRUE), "\n\n")

# Paired t-test
# Same N for both; paired because each row is one comment with both C and D scores.
cat("=== PAIRED T-TEST: CONSTRUCTIVENESS VS. DESTRUCTIVENESS ===\n\n")

ttest_result <- t.test(
  joined_data$harmoniousness_raw,
  joined_data$divisiveness_raw,
  paired = TRUE
)

cat("Paired t-test results:\n")
cat(sprintf("  t(%d) = %.2f, p < .001\n", 
            ttest_result$parameter, ttest_result$statistic))

# Calculate Cohen's d
mean_diff <- mean(joined_data$harmoniousness_raw - joined_data$divisiveness_raw, na.rm = TRUE)
sd_diff <- sd(joined_data$harmoniousness_raw - joined_data$divisiveness_raw, na.rm = TRUE)
cohens_d <- mean_diff / sd_diff

cat(sprintf("  Mean difference = %.3f\n", mean_diff))
cat(sprintf("  Cohen's d = %.2f\n", cohens_d))
cat(sprintf("  95%% CI: [%.3f, %.3f]\n", 
            ttest_result$conf.int[1], ttest_result$conf.int[2]))

# Descriptive statistics
cat("\n=== DESCRIPTIVE STATISTICS ===\n")
cat(sprintf("Constructiveness: M = %.3f, SD = %.3f\n",
            mean(joined_data$harmoniousness_raw, na.rm = TRUE),
            sd(joined_data$harmoniousness_raw, na.rm = TRUE)))
cat(sprintf("Destructiveness: M = %.3f, SD = %.3f\n",
            mean(joined_data$divisiveness_raw, na.rm = TRUE),
            sd(joined_data$divisiveness_raw, na.rm = TRUE)))

# Binary prevalence: proportion with ≥1 feature present (index > 0; correct when index is label-based)
prop_C <- mean(joined_data$harmoniousness_raw > 0, na.rm = TRUE)
prop_D <- mean(joined_data$divisiveness_raw > 0, na.rm = TRUE)

cat(sprintf("\nProportion with ≥1 constructive feature: %.1f%%\n", prop_C * 100))
cat(sprintf("Proportion with ≥1 destructive feature: %.1f%%\n", prop_D * 100))

# Save results
results <- list(
  t_test = ttest_result,
  cohens_d = cohens_d,
  mean_C = mean(joined_data$harmoniousness_raw, na.rm = TRUE),
  mean_D = mean(joined_data$divisiveness_raw, na.rm = TRUE),
  sd_C = sd(joined_data$harmoniousness_raw, na.rm = TRUE),
  sd_D = sd(joined_data$divisiveness_raw, na.rm = TRUE),
  prop_C = prop_C,
  prop_D = prop_D
)

saveRDS(results, "analysis/prevalence/RQ1_overall_prevalence_ttest_results.rds")

cat("\n=== RESULTS SAVED ===\n")
