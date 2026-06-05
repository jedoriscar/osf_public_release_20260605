# Purpose
# Correlation between constructiveness and destructiveness (discriminant validity).
# Demonstrates C and D are separate dimensions, not opposite ends of one continuum.
#
# Reference: Main text line 52
# Scale note: Pearson r is scale-invariant (linear rescaling does not change r).
# Here both C and D are 0-1 (label-based proportion); correlation is comparable
# to RQ0 correlations with sentiment/politeness (which may be on other scales).

# Setup
rm(list = ls())

# Load data
source("analysis/setup/load_data.R")

n_use <- sum(complete.cases(joined_data[, c("harmoniousness_raw", "divisiveness_raw")]))

# Calculate correlation
cat("=== CONSTRUCTIVENESS × DESTRUCTIVENESS CORRELATION (RQ0) ===\n\n")

test_C_D <- cor.test(joined_data$harmoniousness_raw, joined_data$divisiveness_raw, use = "complete.obs")
cor_C_D <- test_C_D$estimate

cat("N (complete cases):", n_use, "\n\n")
cat("Constructiveness × Destructiveness:\n")
cat(sprintf("  r = %.3f, p = %.4f\n", cor_C_D, test_C_D$p.value))
if (!is.null(test_C_D$conf.int)) {
  cat(sprintf("  95%% CI: [%.3f, %.3f]\n", test_C_D$conf.int[1], test_C_D$conf.int[2]))
}
cat("\nInterpretation: Weak correlation supports treating C and D as\n")
cat("  separate dimensions rather than opposite ends of one continuum.\n")

# Save results
results <- list(
  correlation = cor_C_D,
  p_value = test_C_D$p.value,
  ci = if (!is.null(test_C_D$conf.int)) test_C_D$conf.int else NULL,
  n = n_use
)

saveRDS(results, "analysis/discriminant_validity/RQ0_C_D_correlation_results.rds")

cat("\n=== RESULTS SAVED ===\n")
