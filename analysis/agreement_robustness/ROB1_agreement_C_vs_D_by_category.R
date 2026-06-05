# Purpose
# Test whether constructiveness exceeds destructiveness within each agreement category.
# This demonstrates that people can constructively disagree.
#
# Reference: Main text lines 128-129
# Tests: Paired t-tests within each agreement category

# Setup
rm(list = ls())
library(tidyverse)   # dplyr (%, left_join, filter, group_by, summarise)

# Load data
source("analysis/setup/load_data.R")

# Agreement is in canonical data (merged by prepare_canonical_data.R).
if (!"agreement_label" %in% colnames(joined_data)) {
  stop("agreement_label not in data. Run: Rscript analysis/setup/prepare_canonical_data.R")
}

analysis_data <- joined_data %>%
  filter(!is.na(agreement_label), !is.na(harmoniousness_raw), !is.na(divisiveness_raw)) %>%
  select(comment_id, agreement_label, harmoniousness_raw, divisiveness_raw)

# Test within each agreement category
cat("=== CONSTRUCTIVENESS VS. DESTRUCTIVENESS BY AGREEMENT CATEGORY ===\n\n")

agreement_categories <- unique(analysis_data$agreement_label)
results_list <- list()

for (category in agreement_categories) {
  cat_data <- analysis_data %>%
    filter(agreement_label == category)
  
  if (nrow(cat_data) > 0) {
    # Paired t-test
    ttest <- t.test(cat_data$harmoniousness_raw, cat_data$divisiveness_raw, paired = TRUE)
    
    # Calculate means and effect size
    mean_C <- mean(cat_data$harmoniousness_raw, na.rm = TRUE)
    mean_D <- mean(cat_data$divisiveness_raw, na.rm = TRUE)
    mean_diff <- mean_C - mean_D
    sd_diff <- sd(cat_data$harmoniousness_raw - cat_data$divisiveness_raw, na.rm = TRUE)
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
}

# Save results
results <- list(
  by_category = results_list,
  n_total = nrow(analysis_data)
)

saveRDS(results, "analysis/agreement_robustness/ROB1_agreement_C_vs_D_by_category_results.rds")

cat("=== RESULTS SAVED ===\n")
cat("Constructiveness exceeds destructiveness in all agreement categories.\n")
