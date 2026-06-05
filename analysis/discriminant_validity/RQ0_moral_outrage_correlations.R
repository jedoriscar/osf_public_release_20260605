# Purpose
# Correlations between constructiveness/destructiveness and moral outrage for
# discriminant validity (D expected to correlate more with moral outrage than C).
#
# Reference: Main text line 52
# Data: Canonical joined_data; moral outrage should be in .rda as prob_moral_outrage or moral_outrage.

# Setup
rm(list = ls())

# Load data
# Canonical load; indices are label-based (load_data.R).
source("analysis/setup/load_data.R")

# Moral outrage: must exist in canonical data (Perspective API or pipeline)
if (!"moral_outrage" %in% colnames(joined_data) && 
    !"prob_moral_outrage" %in% colnames(joined_data)) {
  stop("Moral outrage column not found. Check column names.")
}

# Use whichever column name the canonical data uses
moral_outrage_col <- ifelse("moral_outrage" %in% colnames(joined_data), 
                           "moral_outrage", "prob_moral_outrage")

# For correlation we use binary (≥0.6) so scale matches our index (0/1 features)
if (moral_outrage_col == "prob_moral_outrage") {
  joined_data$moral_outrage_binary <- as.numeric(joined_data[[moral_outrage_col]] >= 0.6)
  moral_outrage_var <- "moral_outrage_binary"
} else {
  moral_outrage_var <- moral_outrage_col
}

# Calculate correlations
cat("=== DISCRIMINANT VALIDITY: MORAL OUTRAGE CORRELATIONS ===\n\n")

cor_C_mo <- cor(joined_data$harmoniousness_raw, joined_data[[moral_outrage_var]], use = "complete.obs")
test_C_mo <- cor.test(joined_data$harmoniousness_raw, joined_data[[moral_outrage_var]], use = "complete.obs")

cat("Constructiveness × Moral Outrage:\n")
cat(sprintf("  r = %.3f, p = %.3f\n", cor_C_mo, test_C_mo$p.value))

cor_D_mo <- cor(joined_data$divisiveness_raw, joined_data[[moral_outrage_var]], use = "complete.obs")
test_D_mo <- cor.test(joined_data$divisiveness_raw, joined_data[[moral_outrage_var]], use = "complete.obs")

cat("\nDestructiveness × Moral Outrage:\n")
cat(sprintf("  r = %.3f, p = %.3f\n", cor_D_mo, test_D_mo$p.value))

# Save results
results <- list(
  C_moral_outrage = list(r = cor_C_mo, p = test_C_mo$p.value),
  D_moral_outrage = list(r = cor_D_mo, p = test_D_mo$p.value)
)

saveRDS(results, "analysis/discriminant_validity/RQ0_moral_outrage_correlations_results.rds")

cat("\n=== RESULTS SAVED ===\n")
