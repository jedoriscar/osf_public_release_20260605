# Purpose
# Calculate spam removal statistics using Perspective API spam classifier.
# Reports number and percentage of comments removed as spam (threshold > 0.7).
#
# Reference: SI Appendix Section 1.4

# Setup
rm(list = ls())

# Load data
source("analysis/setup/load_data.R")

# Check for spam probability column
if (!"prob_spam" %in% colnames(joined_data) && 
    !"spam_probability" %in% colnames(joined_data)) {
  stop("Required spam probability column not found in the public analytic data.")
}

spam_col <- ifelse("prob_spam" %in% colnames(joined_data), "prob_spam", "spam_probability")

# Calculate spam removal statistics
cat("=== SPAM REMOVAL STATISTICS ===\n\n")

# Count comments that would be removed (spam > 0.7)
spam_removed <- sum(joined_data[[spam_col]] > 0.7, na.rm = TRUE)
total_before_spam <- nrow(joined_data) + spam_removed  # Approximate
spam_rate <- (spam_removed / total_before_spam) * 100

cat("Comments removed as spam (threshold > 0.7):", spam_removed, "\n")
cat("Estimated total before spam removal:", total_before_spam, "\n")
cat("Spam removal rate:", round(spam_rate, 2), "%\n")

# Distribution of spam scores
cat("\nSpam score distribution:\n")
cat(sprintf("  Mean: %.3f\n", mean(joined_data[[spam_col]], na.rm = TRUE)))
cat(sprintf("  Median: %.3f\n", median(joined_data[[spam_col]], na.rm = TRUE)))
cat(sprintf("  Range: %.3f to %.3f\n", 
            min(joined_data[[spam_col]], na.rm = TRUE),
            max(joined_data[[spam_col]], na.rm = TRUE)))

# Save results
results <- list(
  n_spam_removed = spam_removed,
  spam_removal_rate = spam_rate,
  spam_score_mean = mean(joined_data[[spam_col]], na.rm = TRUE),
  spam_score_median = median(joined_data[[spam_col]], na.rm = TRUE)
)

saveRDS(results, "analysis/supplement_s1_data_collection/SM1_spam_removal_stats_results.rds")

cat("\n=== RESULTS SAVED ===\n")
