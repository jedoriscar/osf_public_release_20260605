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

# Extract years: comment_year for when comments were posted, video_year for when
# videos were uploaded. Some platforms use video_published_at and others use
# upload_date, so parse both and fall back row-by-row.
parse_year <- function(x) {
  sx <- as.character(x)
  sx[is.na(x)] <- NA_character_
  out <- suppressWarnings(year(ymd_hms(sx, quiet = TRUE, tz = "UTC")))
  miss <- is.na(out)
  out[miss] <- suppressWarnings(year(ymd(sx[miss], quiet = TRUE)))
  miss <- is.na(out)
  out[miss] <- suppressWarnings(year(mdy(sx[miss], quiet = TRUE)))
  miss <- is.na(out)
  out[miss] <- suppressWarnings(as.numeric(substr(sx[miss], 1, 4)))
  out[!is.finite(out)] <- NA_real_
  out
}

joined_data$comment_year <- if ("comment_published_at" %in% colnames(joined_data)) {
  parse_year(joined_data$comment_published_at)
} else {
  NA_real_
}

video_year_primary <- if ("video_upload_year" %in% colnames(joined_data)) {
  as.numeric(joined_data$video_upload_year)
} else if ("video_published_at" %in% colnames(joined_data)) {
  parse_year(joined_data$video_published_at)
} else {
  rep(NA_real_, nrow(joined_data))
}
video_year_fallback <- if ("upload_date" %in% colnames(joined_data)) {
  parse_year(joined_data$upload_date)
} else {
  rep(NA_real_, nrow(joined_data))
}
joined_data$video_year <- ifelse(!is.na(video_year_primary), video_year_primary, video_year_fallback)

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
