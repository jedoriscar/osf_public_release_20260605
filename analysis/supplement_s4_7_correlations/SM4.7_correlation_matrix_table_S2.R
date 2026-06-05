# Purpose
# Generate inter-feature correlation matrix (Table S2) for all 10 discourse features.
# This validates the formative measurement approach by showing low-to-moderate
# within-category correlations and near-zero cross-category correlations.
#
# Reference: SI Appendix Section 4.7, Table S2, Main text line 66
# Status: ✅ VERIFIED REAL (from CREATE_TABLE_S2_CORRELATIONS.R)

# Setup
rm(list = ls())

# Load data
source("analysis/setup/load_data.R")

# Create binary feature indicators (≥0.6 threshold)
cat("=== CREATING BINARY FEATURE INDICATORS ===\n")

# Constructive features
joined_data <- joined_data %>%
  mutate(
    compassion_binary = as.numeric(prob_compassion >= 0.6),
    curiosity_binary = as.numeric(prob_curiosity >= 0.6),
    nuance_binary = as.numeric(prob_nuance >= 0.6),
    personal_story_binary = as.numeric(prob_personal_story >= 0.6),
    reasoning_binary = as.numeric(prob_reasoning >= 0.6),
    toxicity_binary = as.numeric(prob_toxic >= 0.6),
    identity_attack_binary = as.numeric(prob_identity_attack >= 0.6),
    threat_binary = as.numeric(prob_threat >= 0.6),
    attack_author_binary = as.numeric(prob_attack_on_author >= 0.6),
    attack_commenter_binary = as.numeric(prob_attack_on_commenter >= 0.6)
  )

# Calculate correlation matrix
cat("\n=== CALCULATING CORRELATION MATRIX ===\n")

feature_cols <- c(
  "compassion_binary", "curiosity_binary", "nuance_binary", 
  "personal_story_binary", "reasoning_binary",
  "toxicity_binary", "identity_attack_binary", "threat_binary",
  "attack_author_binary", "attack_commenter_binary"
)

cor_matrix <- cor(joined_data[, feature_cols], use = "complete.obs")

cat("Correlation matrix calculated.\n")
cat("N =", sum(complete.cases(joined_data[, feature_cols])), "comments\n\n")

# Summary statistics
cat("=== WITHIN-CATEGORY CORRELATIONS ===\n")

# Constructive features
constructive_cors <- cor_matrix[1:5, 1:5]
constructive_cors[lower.tri(constructive_cors, diag = TRUE)] <- NA
mean_constructive <- mean(constructive_cors, na.rm = TRUE)
cat(sprintf("Mean constructive feature correlation: r = %.2f\n", mean_constructive))
cat(sprintf("Range: %.2f to %.2f\n", 
            min(constructive_cors, na.rm = TRUE), 
            max(constructive_cors, na.rm = TRUE)))

# Destructive features
destructive_cors <- cor_matrix[6:10, 6:10]
destructive_cors[lower.tri(destructive_cors, diag = TRUE)] <- NA
mean_destructive <- mean(destructive_cors, na.rm = TRUE)
cat(sprintf("\nMean destructive feature correlation: r = %.2f\n", mean_destructive))
cat(sprintf("Range: %.2f to %.2f\n", 
            min(destructive_cors, na.rm = TRUE), 
            max(destructive_cors, na.rm = TRUE)))

# Cross-category correlations
cross_cors <- cor_matrix[1:5, 6:10]
mean_cross <- mean(cross_cors, na.rm = TRUE)
cat(sprintf("\nMean cross-category correlation: r = %.2f\n", mean_cross))
cat(sprintf("Range: %.2f to %.2f\n", 
            min(cross_cors, na.rm = TRUE), 
            max(cross_cors, na.rm = TRUE)))

# Save results
results <- list(
  correlation_matrix = cor_matrix,
  mean_constructive = mean_constructive,
  mean_destructive = mean_destructive,
  mean_cross = mean_cross,
  n = sum(complete.cases(joined_data[, feature_cols]))
)

saveRDS(results, "analysis/supplement_s4_7_correlations/SM4.7_correlation_matrix_results.rds")

# Also save as CSV for easy viewing
write.csv(cor_matrix, "analysis/supplement_s4_7_correlations/Table_S2_correlation_matrix.csv")

cat("\n=== RESULTS SAVED ===\n")
cat("Matrix saved as: Table_S2_correlation_matrix.csv\n")
