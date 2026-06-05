# Purpose
# Calculate incoherent comment removal statistics using Perspective API.
# Reports number and percentage of comments removed as incoherent (threshold > 0.8).
#
# Reference: SI Appendix Section 1.4

# Setup
rm(list = ls())

# Load data
source("analysis/setup/load_data.R")

# Check for incoherent probability column
if (!"prob_incoherent" %in% colnames(joined_data) && 
    !"incoherent_probability" %in% colnames(joined_data)) {
  stop("Incoherent probability column not found. Check column names.")
}

incoherent_col <- ifelse("prob_incoherent" %in% colnames(joined_data), 
                        "prob_incoherent", "incoherent_probability")

# Calculate incoherent removal statistics
cat("=== INCOHERENT REMOVAL STATISTICS ===\n\n")

# Count comments that would be removed (incoherent > 0.8)
incoherent_removed <- sum(joined_data[[incoherent_col]] > 0.8, na.rm = TRUE)
total_before_incoherent <- nrow(joined_data) + incoherent_removed  # Approximate
incoherent_rate <- (incoherent_removed / total_before_incoherent) * 100

cat("Comments removed as incoherent (threshold > 0.8):", incoherent_removed, "\n")
cat("Estimated total before incoherent removal:", total_before_incoherent, "\n")
cat("Incoherent removal rate:", round(incoherent_rate, 2), "%\n")

# Distribution of incoherent scores
cat("\nIncoherent score distribution:\n")
cat(sprintf("  Mean: %.3f\n", mean(joined_data[[incoherent_col]], na.rm = TRUE)))
cat(sprintf("  Median: %.3f\n", median(joined_data[[incoherent_col]], na.rm = TRUE)))
cat(sprintf("  Range: %.3f to %.3f\n", 
            min(joined_data[[incoherent_col]], na.rm = TRUE),
            max(joined_data[[incoherent_col]], na.rm = TRUE)))

# Save results
results <- list(
  n_incoherent_removed = incoherent_removed,
  incoherent_removal_rate = incoherent_rate,
  incoherent_score_mean = mean(joined_data[[incoherent_col]], na.rm = TRUE),
  incoherent_score_median = median(joined_data[[incoherent_col]], na.rm = TRUE)
)

saveRDS(results, "analysis/supplement_s1_data_collection/SM1_incoherent_removal_stats_results.rds")

cat("\n=== RESULTS SAVED ===\n")
