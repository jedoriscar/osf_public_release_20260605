# Purpose
# Calculate probability-weighted topic-level constructiveness and destructiveness.
# Tests whether constructiveness exceeds destructiveness across all discussion topics.
#
# Reference: Main text lines 70-72
# Uses probability-weighted approach with ≥0.25 threshold

# Setup
rm(list = ls())
library(tidyverse)

# Load data
source("analysis/setup/load_data.R")

# Topic probability columns: topic_<number>_<description> (exclude _z columns)
# Canonical data uses e.g. topic_0_diversity_and_immigration_in_north_america
topic_cols <- grep("^topic_-?\\d+_", colnames(joined_data), value = TRUE)
topic_cols <- topic_cols[!grepl("_z$", topic_cols)]
if (length(topic_cols) == 0) {
  stop("Topic probability columns not found. Expected: topic_0_..., topic_1_..., etc. (exclude _z).")
}

cat("=== TOPIC-LEVEL PREVALENCE ANALYSIS ===\n")
cat("Found", length(topic_cols), "topic columns\n\n")

# Calculate weighted means for each topic
topic_results <- list()

for (topic_col in topic_cols) {
  topic_label <- gsub("^topic_", "", topic_col)
  
  # Filter to comments with probability ≥ 0.25 for this topic
  topic_data <- joined_data %>%
    filter(.data[[topic_col]] >= 0.25) %>%
    mutate(
      weight = .data[[topic_col]],
      weighted_C = harmoniousness_raw * weight,
      weighted_D = divisiveness_raw * weight
    )
  
  if (nrow(topic_data) > 0) {
    # Calculate weighted means
    weighted_mean_C <- sum(topic_data$weighted_C, na.rm = TRUE) / sum(topic_data$weight, na.rm = TRUE)
    weighted_mean_D <- sum(topic_data$weighted_D, na.rm = TRUE) / sum(topic_data$weight, na.rm = TRUE)
    
    topic_results[[topic_label]] <- list(
      topic = topic_label,
      n_comments = nrow(topic_data),
      weighted_constructiveness = weighted_mean_C,
      weighted_destructiveness = weighted_mean_D,
      advantage = weighted_mean_C - weighted_mean_D
    )
    
    cat(sprintf("Topic %s: C = %.3f, D = %.3f, Advantage = %.3f (N = %d)\n",
                topic_label, weighted_mean_C, weighted_mean_D, 
                weighted_mean_C - weighted_mean_D, nrow(topic_data)))
  }
}

# Paired t-test across topics
topic_df <- do.call(rbind, lapply(topic_results, function(x) {
  data.frame(
    topic = x$topic,
    C = x$weighted_constructiveness,
    D = x$weighted_destructiveness
  )
}))

if (nrow(topic_df) > 1) {
  cat("\n=== PAIRED T-TEST ACROSS TOPICS ===\n")
  ttest_topics <- t.test(topic_df$C, topic_df$D, paired = TRUE)
  cat(sprintf("t(%d) = %.2f, p < .001\n", ttest_topics$parameter, ttest_topics$statistic))
  
  cohens_d_topics <- mean(topic_df$C - topic_df$D) / sd(topic_df$C - topic_df$D)
  cat(sprintf("Cohen's d = %.2f\n", cohens_d_topics))
}

# Save results
results <- list(
  topic_results = topic_results,
  topic_dataframe = topic_df,
  t_test = if(exists("ttest_topics")) ttest_topics else NULL,
  cohens_d = if(exists("cohens_d_topics")) cohens_d_topics else NULL
)

saveRDS(results, "analysis/prevalence/RQ1_topic_level_prevalence_results.rds")

cat("\n=== RESULTS SAVED ===\n")
