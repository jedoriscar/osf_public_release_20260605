# Purpose
# Replicate stance analysis in climate change dataset.
# Tests whether constructiveness operates independently of climate stance (believer vs skeptic).
#
# Reference: Main text lines 193

# Setup
rm(list = ls())

# Load climate data
climate_data_path <- "data/analysis_objects/climate_comments.rds"

if (!file.exists(climate_data_path)) {
  stop("Climate data file not found.")
}

joined_data <- readRDS(climate_data_path)

# Load climate stance data
stance_file <- "data/model_labels/climate_stance_labels.csv"
if (!file.exists(stance_file)) {
  stop("Climate stance data not found.")
}

library(tidyverse)
library(lme4)
library(lmerTest)

stance_data <- read.csv(stance_file)
stance_data <- stance_data %>%
  filter(!is.na(comment_id), comment_id != "", !is.na(stance_label), stance_label != "") %>%
  distinct(comment_id, .keep_all = TRUE)

# Merge with main data
analysis_data <- stance_data %>%
  left_join(
    joined_data %>%
      select(comment_id, harmoniousness_raw, divisiveness_raw),
    by = "comment_id"
  ) %>%
  filter(!is.na(harmoniousness_raw), !is.na(divisiveness_raw))

# Filter to largest topic with high probability if column exists
if ("topic_0_prob" %in% colnames(analysis_data)) {
  analysis_data <- analysis_data %>%
    filter(topic_0_prob >= 0.6)
}

# Test within each stance category
cat("=== CLIMATE REPLICATION: STANCE ANALYSIS ===\n\n")

stance_categories <- unique(analysis_data$stance_label)
results_list <- list()

for (category in stance_categories) {
  cat_data <- analysis_data %>%
    filter(stance_label == category)
  
  if (nrow(cat_data) > 0) {
    ttest <- t.test(cat_data$harmoniousness_raw, cat_data$divisiveness_raw, paired = TRUE)
    mean_C <- mean(cat_data$harmoniousness_raw, na.rm = TRUE)
    mean_D <- mean(cat_data$divisiveness_raw, na.rm = TRUE)
    
    cat(sprintf("%s: C = %.1f%%, D = %.1f%%, t = %.2f, p = %.3f\n",
                category, mean_C * 100, mean_D * 100, ttest$statistic, ttest$p.value))
    
    results_list[[category]] <- list(
      n = nrow(cat_data),
      mean_C = mean_C,
      mean_D = mean_D,
      t_test = ttest
    )
  }
}

# Save results
results <- list(
  by_category = results_list,
  n_total = nrow(analysis_data)
)

saveRDS(results, "analysis/climate_replication/CLIMATE_stance_analysis_results.rds")

cat("\n=== RESULTS SAVED ===\n")
