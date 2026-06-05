# Purpose
# Replicate discriminant validity tests in climate change dataset.
# Tests correlations with sentiment, politeness, moral outrage.
#
# Reference: Main text lines 184

# Setup
rm(list = ls())

# Load climate data
climate_data_path <- "data/analysis_objects/climate_comments.rds"

if (!file.exists(climate_data_path)) {
  stop("Climate data file not found.")
}

joined_data <- readRDS(climate_data_path)

cat("=== CLIMATE REPLICATION: DISCRIMINANT VALIDITY ===\n\n")

# Sentiment: use VADER (vader_positive, vader_negative) if sentiment_positive not present
pos_col <- if ("sentiment_positive" %in% colnames(joined_data)) "sentiment_positive" else if ("vader_positive" %in% colnames(joined_data)) "vader_positive" else NULL
neg_col <- if ("sentiment_negative" %in% colnames(joined_data)) "sentiment_negative" else if ("vader_negative" %in% colnames(joined_data)) "vader_negative" else NULL
if (!is.null(pos_col) && !is.null(neg_col)) {
  cor_C_pos <- cor(joined_data$harmoniousness_raw, joined_data[[pos_col]], use = "complete.obs")
  cor_C_neg <- cor(joined_data$harmoniousness_raw, joined_data[[neg_col]], use = "complete.obs")
  cor_D_pos <- cor(joined_data$divisiveness_raw, joined_data[[pos_col]], use = "complete.obs")
  cor_D_neg <- cor(joined_data$divisiveness_raw, joined_data[[neg_col]], use = "complete.obs")
  cat("Sentiment correlations (", pos_col, " / ", neg_col, "):\n", sep = "")
  cat(sprintf("  C × Positive: r = %.3f\n", cor_C_pos))
  cat(sprintf("  C × Negative: r = %.3f\n", cor_C_neg))
  cat(sprintf("  D × Positive: r = %.3f\n", cor_D_pos))
  cat(sprintf("  D × Negative: r = %.3f\n", cor_D_neg))
} else {
  cat("Sentiment columns not found (need sentiment_positive/negative or vader_positive/negative).\n")
}

# Politeness
if ("politeness" %in% colnames(joined_data)) {
  cor_C_pol <- cor(joined_data$harmoniousness_raw, joined_data$politeness, use = "complete.obs")
  cor_D_pol <- cor(joined_data$divisiveness_raw, joined_data$politeness, use = "complete.obs")
  
  cat("\nPoliteness correlations:\n")
  cat(sprintf("  C × Politeness: r = %.3f\n", cor_C_pol))
  cat(sprintf("  D × Politeness: r = %.3f\n", cor_D_pol))
}

# C × D correlation (align with racial RQ0_C_D_correlation.R; discriminant validity)
ct_CD <- cor.test(joined_data$harmoniousness_raw, joined_data$divisiveness_raw, use = "complete.obs", exact = FALSE)
r_CD <- ct_CD$estimate
ci_CD <- ct_CD$conf.int
cat("\nConstructiveness × Destructiveness (C × D):\n")
cat(sprintf("  r = %.3f, 95%% CI [%.3f, %.3f], p %s\n",
            r_CD, ci_CD[1], ci_CD[2], if (ct_CD$p.value < .001) "< .001" else sprintf("= %.3f", ct_CD$p.value)))

# Save results
results <- list(
  n = nrow(joined_data),
  r_C_D = as.numeric(r_CD),
  r_C_D_ci = as.numeric(ci_CD),
  r_C_D_p = ct_CD$p.value
)

saveRDS(results, "analysis/climate_replication/CLIMATE_discriminant_validity_results.rds")

cat("\n=== RESULTS SAVED ===\n")
