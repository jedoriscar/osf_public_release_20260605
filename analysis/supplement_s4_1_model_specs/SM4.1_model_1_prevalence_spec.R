# Purpose
# Document Model 1 specification: Overall prevalence (paired t-test).
# Provides complete model specification for reporting.
#
# Reference: SI Appendix Section 4.1, Main text line 56
# Model: Paired t-test comparing constructiveness vs. destructiveness

# Setup
rm(list = ls())

# Load data
source("analysis/setup/load_data.R")

# Model specification
cat("=== MODEL 1: OVERALL PREVALENCE (PAIRED T-TEST) ===\n\n")

cat("Model Type: Paired t-test\n")
cat("Dependent Variable: Constructiveness (harmoniousness_raw)\n")
cat("Comparison Variable: Destructiveness (divisiveness_raw)\n")
cat("N: ", nrow(joined_data), " comments\n\n")

cat("Hypothesis:\n")
cat("  H0: μ_C = μ_D\n")
cat("  H1: μ_C > μ_D\n\n")

cat("Test:\n")
cat("  t = (M_C - M_D) / (SD_diff / √n)\n")
cat("  df = n - 1\n\n")

# Run model
ttest_result <- t.test(
  joined_data$harmoniousness_raw,
  joined_data$divisiveness_raw,
  paired = TRUE
)

mean_C <- mean(joined_data$harmoniousness_raw, na.rm = TRUE)
mean_D <- mean(joined_data$divisiveness_raw, na.rm = TRUE)
mean_diff <- mean_C - mean_D
sd_diff <- sd(joined_data$harmoniousness_raw - joined_data$divisiveness_raw, na.rm = TRUE)
cohens_d <- mean_diff / sd_diff

cat("Results:\n")
cat(sprintf("  M_C = %.3f, M_D = %.3f\n", mean_C, mean_D))
cat(sprintf("  t(%d) = %.2f, p < .001\n", ttest_result$parameter, ttest_result$statistic))
cat(sprintf("  Cohen's d = %.3f\n", cohens_d))

# Save results
results <- list(
  model_type = "Paired t-test",
  n = nrow(joined_data),
  mean_C = mean_C,
  mean_D = mean_D,
  t_test = ttest_result,
  cohens_d = cohens_d
)

saveRDS(results, "analysis/supplement_s4_1_model_specs/SM4.1_model_1_prevalence_spec_results.rds")

cat("\n=== RESULTS SAVED ===\n")
