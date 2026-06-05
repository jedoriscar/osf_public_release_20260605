# Purpose
# Calculate correlations between constructiveness/destructiveness and sentiment
# to demonstrate discriminant validity (constructiveness is not just positivity).
#
# Reference: Main text lines 50-52
# Expected: Near-zero correlations with positive/negative sentiment
#
# Data: Canonical joined_data via load_data.R (label-based harmoniousness_raw,
# divisiveness_raw). Sentiment must be in canonical .rda as vader_positive / vader_negative.

# Setup
rm(list = ls())

# Load data
# Loads canonical .rda and applies RQ2 overwrites; indices are label-based (0.6 threshold).
source("analysis/setup/load_data.R")

# Require VADER in canonical data (add via your data pipeline if missing)
if (!"vader_positive" %in% colnames(joined_data) || 
    !"vader_negative" %in% colnames(joined_data)) {
  stop("VADER sentiment columns not found in dataset.\n",
       "Please run: python3 analysis/setup/add_vader_sentiment.py\n",
       "to add VADER scores to the canonical dataset.")
}

# Point to canonical sentiment columns (no derivation here)
joined_data$sentiment_positive <- joined_data$vader_positive
joined_data$sentiment_negative <- joined_data$vader_negative

# Calculate correlations
# Pearson r, pairwise complete observations. We use the same index as all other analyses.
cat("=== DISCRIMINANT VALIDITY: SENTIMENT CORRELATIONS ===\n\n")

# Constructiveness (harmoniousness_raw) with positive sentiment
cor_C_pos <- cor(joined_data$harmoniousness_raw, joined_data$sentiment_positive, use = "complete.obs")
test_C_pos <- cor.test(joined_data$harmoniousness_raw, joined_data$sentiment_positive, use = "complete.obs")

cat("Constructiveness × Positive Sentiment:\n")
cat(sprintf("  r = %.3f, p = %.3f\n", cor_C_pos, test_C_pos$p.value))

# Constructiveness with negative sentiment
cor_C_neg <- cor(joined_data$harmoniousness_raw, joined_data$sentiment_negative, use = "complete.obs")
test_C_neg <- cor.test(joined_data$harmoniousness_raw, joined_data$sentiment_negative, use = "complete.obs")

cat("\nConstructiveness × Negative Sentiment:\n")
cat(sprintf("  r = %.3f, p = %.3f\n", cor_C_neg, test_C_neg$p.value))

# Destructiveness with positive sentiment
cor_D_pos <- cor(joined_data$divisiveness_raw, joined_data$sentiment_positive, use = "complete.obs")
test_D_pos <- cor.test(joined_data$divisiveness_raw, joined_data$sentiment_positive, use = "complete.obs")

cat("\nDestructiveness × Positive Sentiment:\n")
cat(sprintf("  r = %.3f, p = %.3f\n", cor_D_pos, test_D_pos$p.value))

# Destructiveness with negative sentiment
cor_D_neg <- cor(joined_data$divisiveness_raw, joined_data$sentiment_negative, use = "complete.obs")
test_D_neg <- cor.test(joined_data$divisiveness_raw, joined_data$sentiment_negative, use = "complete.obs")

cat("\nDestructiveness × Negative Sentiment:\n")
cat(sprintf("  r = %.3f, p = %.3f\n", cor_D_neg, test_D_neg$p.value))

# Save results
results <- list(
  C_positive = list(r = cor_C_pos, p = test_C_pos$p.value),
  C_negative = list(r = cor_C_neg, p = test_C_neg$p.value),
  D_positive = list(r = cor_D_pos, p = test_D_pos$p.value),
  D_negative = list(r = cor_D_neg, p = test_D_neg$p.value)
)

saveRDS(results, "analysis/discriminant_validity/RQ0_sentiment_correlations_results.rds")

cat("\n=== RESULTS SAVED ===\n")
