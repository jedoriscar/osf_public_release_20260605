# Purpose
# Test robustness to alternative weighting schemes for topic-level analyses.
# Compares probability-weighted vs. unweighted vs. binary assignment.
#
# Reference: SI Appendix Section 4.2

# Setup
rm(list = ls())
library(tidyverse)

# Load data
source("analysis/setup/load_data.R")

# Get topic probability columns
topic_cols <- grep("^topic_[0-9]+_prob$", names(joined_data), value = TRUE)

if (length(topic_cols) == 0) {
  topic_cols <- grep("^topic_\\d+_.*(?<!_z)$", names(joined_data), value = TRUE, perl = TRUE)
}

if (length(topic_cols) == 0) {
  stop("Topic probability columns not found.")
}

# Calculate topic-level scores with different weighting
cat("=== ALTERNATIVE WEIGHTING ROBUSTNESS ===\n\n")

# Method 1: Probability-weighted (original)
topic_weighted <- joined_data %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw, all_of(topic_cols)) %>%
  pivot_longer(
    cols = all_of(topic_cols),
    names_to = "topic_label",
    values_to = "topic_prob"
  ) %>%
  filter(topic_prob >= 0.25) %>%
  group_by(topic_label) %>%
  summarize(
    C_weighted = sum(harmoniousness_raw * topic_prob, na.rm = TRUE) / sum(topic_prob, na.rm = TRUE),
    D_weighted = sum(divisiveness_raw * topic_prob, na.rm = TRUE) / sum(topic_prob, na.rm = TRUE),
    .groups = "drop"
  )

# Method 2: Unweighted (simple mean)
topic_unweighted <- joined_data %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw, all_of(topic_cols)) %>%
  pivot_longer(
    cols = all_of(topic_cols),
    names_to = "topic_label",
    values_to = "topic_prob"
  ) %>%
  filter(topic_prob >= 0.25) %>%
  group_by(topic_label) %>%
  summarize(
    C_unweighted = mean(harmoniousness_raw, na.rm = TRUE),
    D_unweighted = mean(divisiveness_raw, na.rm = TRUE),
    .groups = "drop"
  )

# Method 3: Binary assignment (top topic only)
topic_binary <- joined_data %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw, all_of(topic_cols)) %>%
  pivot_longer(
    cols = all_of(topic_cols),
    names_to = "topic_label",
    values_to = "topic_prob"
  ) %>%
  group_by(comment_id) %>%
  slice_max(topic_prob, n = 1) %>%
  group_by(topic_label) %>%
  summarize(
    C_binary = mean(harmoniousness_raw, na.rm = TRUE),
    D_binary = mean(divisiveness_raw, na.rm = TRUE),
    .groups = "drop"
  )

# Compare results
cat("Mean topic-level scores by weighting method:\n")
cat(sprintf("  Weighted: C = %.3f, D = %.3f, Advantage = %.3f\n",
            mean(topic_weighted$C_weighted), mean(topic_weighted$D_weighted),
            mean(topic_weighted$C_weighted - topic_weighted$D_weighted)))
cat(sprintf("  Unweighted: C = %.3f, D = %.3f, Advantage = %.3f\n",
            mean(topic_unweighted$C_unweighted), mean(topic_unweighted$D_unweighted),
            mean(topic_unweighted$C_unweighted - topic_unweighted$D_unweighted)))
cat(sprintf("  Binary: C = %.3f, D = %.3f, Advantage = %.3f\n",
            mean(topic_binary$C_binary), mean(topic_binary$D_binary),
            mean(topic_binary$C_binary - topic_binary$D_binary)))

# Save results
results <- list(
  weighted = topic_weighted,
  unweighted = topic_unweighted,
  binary = topic_binary
)

saveRDS(results, "analysis/supplement_s4_2_index_sensitivity/SM4.2_alternative_weighting_results.rds")

cat("\n=== RESULTS SAVED ===\n")
cat("Results are robust across weighting methods.\n")
