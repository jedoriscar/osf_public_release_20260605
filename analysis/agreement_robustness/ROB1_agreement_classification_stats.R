# Purpose
# Calculate descriptive statistics for agreement classifications.
# Shows distribution of agreement categories (agree, disagree, mixed, neither).
#
# Reference: Main text lines 126-127
# Reports: Neither 45.5%, Disagreement 31.1%, Mixed 13.1%, Agreement 10.3%

# Setup
rm(list = ls())
library(tidyverse)

# Load data
source("analysis/setup/load_data.R")

# Agreement is in canonical data (merged by prepare_canonical_data.R from
# agreement_disagreement_test/outputs/agreement_test_output.csv).
if (!"agreement_label" %in% colnames(joined_data)) {
  stop("agreement_label not in data. Run from the public release folder root: Rscript analysis/setup/prepare_canonical_data.R")
}

agreement_data <- joined_data %>% filter(!is.na(agreement_label))

cat("=== AGREEMENT CLASSIFICATION STATISTICS ===\n")
cat("N comments with agreement classifications:", nrow(agreement_data), "\n\n")

# Calculate distribution
agreement_dist <- table(agreement_data$agreement_label, useNA = "ifany")
agreement_prop <- prop.table(agreement_dist) * 100

cat("Agreement Category Distribution:\n")
for (i in 1:length(agreement_dist)) {
  cat(sprintf("  %s: n = %d (%.1f%%)\n", 
              names(agreement_dist)[i], 
              agreement_dist[i], 
              agreement_prop[i]))
}

# Save results
results <- list(
  distribution = agreement_dist,
  proportions = agreement_prop,
  n = nrow(agreement_data)
)

saveRDS(results, "analysis/agreement_robustness/ROB1_agreement_classification_stats_results.rds")

cat("\n=== RESULTS SAVED ===\n")
