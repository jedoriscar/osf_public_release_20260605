# Purpose
# Calculate BERTopic topic modeling statistics.
# Reports topic counts, document coverage, topic-level descriptives.
#
# Reference: SI Appendix Section 2.1, Table S12
# Reports: 18 topics, 78,123 documents, topic-level constructiveness/destructiveness

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

cat("=== BERTOPIC TOPIC STATISTICS ===\n")
cat("Found", length(topic_cols), "topics\n\n")

# Calculate topic-level statistics
topic_stats_list <- list()

for (topic_col in topic_cols) {
  topic_num <- gsub("topic_|_prob", "", topic_col)
  
  # Filter to comments with probability ≥ 0.25
  topic_data <- joined_data %>%
    filter(.data[[topic_col]] >= 0.25) %>%
    mutate(
      weight = .data[[topic_col]],
      weighted_C = harmoniousness_raw * weight,
      weighted_D = divisiveness_raw * weight
    )
  
  if (nrow(topic_data) > 0) {
    weighted_mean_C <- sum(topic_data$weighted_C, na.rm = TRUE) / sum(topic_data$weight, na.rm = TRUE)
    weighted_mean_D <- sum(topic_data$weighted_D, na.rm = TRUE) / sum(topic_data$weight, na.rm = TRUE)
    
    topic_stats_list[[topic_num]] <- list(
      topic = topic_num,
      n_documents = nrow(topic_data),
      weighted_constructiveness = weighted_mean_C,
      weighted_destructiveness = weighted_mean_D,
      advantage = weighted_mean_C - weighted_mean_D
    )
  }
}

# Convert to dataframe
topic_stats <- do.call(rbind, lapply(topic_stats_list, function(x) {
  data.frame(
    topic = x$topic,
    n_documents = x$n_documents,
    weighted_C = x$weighted_constructiveness,
    weighted_D = x$weighted_destructiveness,
    advantage = x$advantage
  )
}))

# Calculate percentages
total_docs <- sum(topic_stats$n_documents)
topic_stats <- topic_stats %>%
  mutate(
    percent_of_corpus = (n_documents / total_docs) * 100
  ) %>%
  arrange(desc(n_documents))

cat("Topic Statistics:\n")
print(head(topic_stats, 10))

cat("\nTotal documents across all topics:", total_docs, "\n")
cat("Largest topic:", topic_stats$topic[1], "(", round(topic_stats$percent_of_corpus[1], 1), "%)\n")

# Save results
results <- list(
  n_topics = length(topic_stats_list),
  topic_statistics = topic_stats,
  total_documents = total_docs
)

saveRDS(results, "analysis/supplement_s2_measurement/SM2_bertopic_topic_statistics_results.rds")
write.csv(topic_stats, "analysis/supplement_s2_measurement/Table_S12_topic_statistics.csv", row.names = FALSE)

cat("\n=== RESULTS SAVED ===\n")
