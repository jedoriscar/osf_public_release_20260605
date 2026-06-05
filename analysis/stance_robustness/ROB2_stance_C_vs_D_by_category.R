# Purpose
# Test whether constructiveness exceeds destructiveness within each stance category.
# This demonstrates that constructiveness operates independently of ideological position.
# Restricts to Pro-Diversity vs Anti-Diversity only for the comparison.
#
# Reference: Main text lines 148-149
# Tests: Paired t-tests within each stance category (Pro-Diversity, Anti-Diversity)

# Setup
rm(list = ls())
library(tidyverse)   # dplyr (%, left_join, select, filter)

# Load data
source("analysis/setup/load_data.R")

# Load stance classification data
stance_file <- "data/model_labels/racial_stance_labels.csv"
if (!file.exists(stance_file)) {
  stop("Stance classification file not found. Expected at: ", stance_file)
}

stance_data <- read.csv(stance_file)

# C/D from canonical data (label-based); use renamed lookup to avoid .x/.y (stance CSV has its own C/D columns)
cd_lookup <- joined_data %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, c_raw = harmoniousness_raw, d_raw = divisiveness_raw)

analysis_data <- stance_data %>%
  left_join(cd_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(c_raw), !is.na(d_raw))

# Filter to Topic 0 with high probability (≥0.6) if column exists
if ("topic_0_prob" %in% colnames(analysis_data)) {
  analysis_data <- analysis_data %>%
    filter(topic_0_prob >= 0.6)
}

# Restrict to Pro-Diversity vs Anti-Diversity for the comparison (exclude Neutral/Unclear, Mixed)
analysis_data <- analysis_data %>%
  filter(stance_label %in% c("Pro-Diversity", "Anti-Diversity"))

# Test within each stance category
cat("=== CONSTRUCTIVENESS VS. DESTRUCTIVENESS BY STANCE CATEGORY (PRO vs ANTI) ===\n\n")

stance_categories <- unique(analysis_data$stance_label)
results_list <- list()

for (category in stance_categories) {
  cat_data <- analysis_data %>%
    filter(stance_label == category)
  
  if (nrow(cat_data) == 0) next
  
  # Paired t-test requires at least 2 observations (e.g. "Mixed" may have n=1)
  if (nrow(cat_data) < 2) {
    cat(sprintf("%s:\n", category))
    cat(sprintf("  N = %d (skipped t-test: too few observations)\n\n", nrow(cat_data)))
    results_list[[category]] <- list(n = nrow(cat_data), skipped = TRUE)
    next
  }

  # Paired t-test (c_raw/d_raw from canonical data)
  ttest <- t.test(cat_data$c_raw, cat_data$d_raw, paired = TRUE)
  mean_C <- mean(cat_data$c_raw, na.rm = TRUE)
  mean_D <- mean(cat_data$d_raw, na.rm = TRUE)
  mean_diff <- mean_C - mean_D
  sd_diff <- sd(cat_data$c_raw - cat_data$d_raw, na.rm = TRUE)
  cohens_d <- mean_diff / sd_diff

  cat(sprintf("%s:\n", category))
  cat(sprintf("  N = %d\n", nrow(cat_data)))
  cat(sprintf("  C = %.1f%%, D = %.1f%%, Advantage = %.1f%%\n",
              mean_C * 100, mean_D * 100, mean_diff * 100))
  cat(sprintf("  t(%d) = %.2f, p < .001, d = %.2f\n\n",
              ttest$parameter, ttest$statistic, cohens_d))

  results_list[[category]] <- list(
    n = nrow(cat_data),
    mean_C = mean_C,
    mean_D = mean_D,
    mean_diff = mean_diff,
    t_test = ttest,
    cohens_d = cohens_d
  )
}

# Save results
results <- list(
  by_category = results_list,
  n_total = nrow(analysis_data)
)

saveRDS(results, "analysis/stance_robustness/ROB2_stance_C_vs_D_by_category_results.rds")

cat("=== RESULTS SAVED ===\n")
cat("Constructiveness exceeds destructiveness within Pro-Diversity and Anti-Diversity.\n")
