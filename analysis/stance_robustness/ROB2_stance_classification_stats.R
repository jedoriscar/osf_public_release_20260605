# Purpose
# Calculate descriptive statistics for stance classifications.
# Shows distribution of stance categories (pro-diversity, anti-diversity, neutral).
#
# Reference: Main text lines 146-147
# Reports after high-probability Topic 0 filtering: Neutral/Unclear 51.1%,
# Anti-Diversity 40.8%, Pro-Diversity 8.2%

# Setup
rm(list = ls())
library(tidyverse)   # dplyr (%, filter, group_by, summarise)

# Load data
source("analysis/setup/load_data.R")

# Load stance classification data
stance_file <- "data/model_labels/racial_stance_labels.csv"
if (!file.exists(stance_file)) {
  stop("Stance classification file not found. Expected at: ", stance_file)
}

stance_data <- read.csv(stance_file)

# Filter to Topic 0 (Diversity and Immigration) with high probability (≥0.6) if column exists
# Stance CSV uses topic_0_diversity_and_immigration_in_north_america (not topic_0_prob)
topic_col <- NULL
if ("topic_0_prob" %in% colnames(stance_data)) {
  topic_col <- "topic_0_prob"
} else if ("topic_0_diversity_and_immigration_in_north_america" %in% colnames(stance_data)) {
  topic_col <- "topic_0_diversity_and_immigration_in_north_america"
}
if (!is.null(topic_col)) {
  analysis_data <- stance_data %>%
    filter(.data[[topic_col]] >= 0.6)
} else {
  analysis_data <- stance_data
}

cat("=== STANCE CLASSIFICATION STATISTICS ===\n")
cat("N comments with stance classifications:", nrow(analysis_data), "\n\n")

# Calculate distribution
stance_dist <- table(analysis_data$stance_label, useNA = "ifany")
stance_prop <- prop.table(stance_dist) * 100

cat("Stance Category Distribution:\n")
for (i in 1:length(stance_dist)) {
  cat(sprintf("  %s: n = %d (%.1f%%)\n", 
              names(stance_dist)[i], 
              stance_dist[i], 
              stance_prop[i]))
}

# Save results
results <- list(
  distribution = stance_dist,
  proportions = stance_prop,
  n = nrow(analysis_data)
)

saveRDS(results, "analysis/stance_robustness/ROB2_stance_classification_stats_results.rds")

cat("\n=== RESULTS SAVED ===\n")
