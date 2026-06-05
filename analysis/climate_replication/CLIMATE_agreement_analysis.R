# Purpose
# Replicate agreement analysis in climate change dataset.
# Tests whether people can constructively disagree in climate discourse.
#
# Reference: Main text lines 191-192

# Setup
rm(list = ls())

# Load climate data
climate_data_path <- "data/analysis_objects/climate_comments.rds"

if (!file.exists(climate_data_path)) {
  stop("Climate data file not found.")
}

joined_data <- readRDS(climate_data_path)

# Load climate agreement data
agreement_file <- "data/model_labels/climate_agreement_labels.csv"
if (!file.exists(agreement_file)) {
  stop("Climate agreement data not found.")
}

library(tidyverse)
library(lme4)
library(lmerTest)

agreement_data <- read.csv(agreement_file)

# Merge with main data
analysis_data <- agreement_data %>%
  left_join(
    joined_data %>%
      select(comment_id, harmoniousness_raw, divisiveness_raw),
    by = "comment_id"
  ) %>%
  filter(!is.na(harmoniousness_raw), !is.na(divisiveness_raw))

# Test within each agreement category
cat("=== CLIMATE REPLICATION: AGREEMENT ANALYSIS ===\n\n")

agreement_categories <- unique(analysis_data$agreement_label)
results_list <- list()

for (category in agreement_categories) {
  cat_data <- analysis_data %>%
    filter(agreement_label == category)
  
  if (nrow(cat_data) > 0) {
    ttest <- t.test(cat_data$harmoniousness_raw, cat_data$divisiveness_raw, paired = TRUE)
    mean_C <- mean(cat_data$harmoniousness_raw, na.rm = TRUE)
    mean_D <- mean(cat_data$divisiveness_raw, na.rm = TRUE)
    
    cat(sprintf("%s: C = %.1f%%, D = %.1f%%, t = %.2f, p < .001\n",
                category, mean_C * 100, mean_D * 100, ttest$statistic))
    
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

saveRDS(results, "analysis/climate_replication/CLIMATE_agreement_analysis_results.rds")

cat("\n=== RESULTS SAVED ===\n")
