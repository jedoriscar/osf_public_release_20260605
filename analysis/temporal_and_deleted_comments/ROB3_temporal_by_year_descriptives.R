# Purpose
# Calculate mean constructiveness and destructiveness by year for temporal trends figure.
# Provides year-by-year descriptives for visualization.
#
# Reference: Main text Figure 7, lines 166-167

# Setup
rm(list = ls())
library(tidyverse)
library(lubridate)

# Load data
source("analysis/setup/load_data.R")

# Extract year from COMMENT date (when comment was posted), not video upload date
if ("comment_published_at" %in% colnames(joined_data)) {
  joined_data$year <- year(joined_data$comment_published_at)
} else if ("comment_date" %in% colnames(joined_data)) {
  joined_data$year <- year(as.Date(joined_data$comment_date))
} else {
  stop("Comment date column not found. Use comment_published_at for temporal analyses.")
}

analysis_data <- joined_data %>%
  filter(!is.na(year), !is.na(harmoniousness_raw), !is.na(divisiveness_raw))

# Calculate year-by-year means
cat("=== TEMPORAL DESCRIPTIVES BY YEAR ===\n\n")

year_stats <- analysis_data %>%
  group_by(year) %>%
  summarize(
    n = n(),
    mean_C = mean(harmoniousness_raw, na.rm = TRUE),
    sd_C = sd(harmoniousness_raw, na.rm = TRUE),
    mean_D = mean(divisiveness_raw, na.rm = TRUE),
    sd_D = sd(divisiveness_raw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

cat("Year-by-Year Statistics:\n")
print(year_stats)

# Save results
results <- list(
  year_stats = year_stats,
  n_total = nrow(analysis_data),
  year_range = range(analysis_data$year, na.rm = TRUE)
)

saveRDS(results, "analysis/temporal_and_deleted_comments/ROB3_temporal_by_year_descriptives_results.rds")
write.csv(year_stats, "analysis/temporal_and_deleted_comments/ROB3_temporal_by_year_stats.csv", row.names = FALSE)

cat("\n=== RESULTS SAVED ===\n")
cat("CSV saved for figure generation.\n")
