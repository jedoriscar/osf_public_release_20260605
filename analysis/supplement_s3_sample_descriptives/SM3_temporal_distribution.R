# Purpose
# Calculate temporal distribution of videos and comments.
# Reports year-by-year video uploads and comment counts.
#
# Reference: SI Appendix Section 3.3

# Setup
rm(list = ls())
library(tidyverse)
library(lubridate)

# Load data
source("analysis/setup/load_data.R")

# Extract years: comment_year for when comments were posted, video_year for when videos were uploaded
if ("comment_published_at" %in% colnames(joined_data)) {
  joined_data$comment_year <- year(joined_data$comment_published_at)
} else {
  joined_data$comment_year <- NA
}
if ("video_published_at" %in% colnames(joined_data)) {
  joined_data$video_year <- year(as.POSIXct(joined_data$video_published_at))
} else if ("upload_date" %in% colnames(joined_data)) {
  joined_data$video_year <- year(as.Date(joined_data$upload_date))
} else {
  joined_data$video_year <- NA
}

# Calculate temporal distribution
cat("=== TEMPORAL DISTRIBUTION ===\n\n")

# Videos by year (video upload date)
video_years <- joined_data %>%
  filter(!is.na(video_year)) %>%
  group_by(video_id) %>%
  summarize(year = first(video_year), .groups = "drop") %>%
  group_by(year) %>%
  summarize(n_videos = n(), .groups = "drop") %>%
  arrange(year)

cat("Videos by year:\n")
print(video_years)

# Comments by year (comment post date)
comment_years <- joined_data %>%
  filter(!is.na(comment_year)) %>%
  group_by(year = comment_year) %>%
  summarize(n_comments = n(), .groups = "drop") %>%
  arrange(year)

cat("\nComments by year:\n")
print(comment_years)

# Save results
results <- list(
  videos_by_year = video_years,
  comments_by_year = comment_years,
  video_year_range = range(joined_data$video_year, na.rm = TRUE),
  comment_year_range = range(joined_data$comment_year, na.rm = TRUE)
)

saveRDS(results, "analysis/supplement_s3_sample_descriptives/SM3_temporal_distribution_results.rds")
write.csv(video_years, "analysis/supplement_s3_sample_descriptives/videos_by_year.csv", row.names = FALSE)
write.csv(comment_years, "analysis/supplement_s3_sample_descriptives/comments_by_year.csv", row.names = FALSE)

cat("\n=== RESULTS SAVED ===\n")
