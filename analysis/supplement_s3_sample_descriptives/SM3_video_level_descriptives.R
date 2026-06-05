# Purpose
# Calculate video-level descriptive statistics.
# Reports views, likes, comments, subscribers by video.
#
# Reference: SI Appendix Section 3.2
# Reports: Mean views, likes, comments, subscribers across videos

# Setup
rm(list = ls())
library(dplyr)

# Load data
load("data/analysis_objects/racial_comments.rda")

if (!"platform" %in% colnames(joined_data) && "platform_source.y" %in% colnames(joined_data)) {
  joined_data$platform <- joined_data$platform_source.y
}

# Calculate video-level statistics
cat("=== VIDEO-LEVEL DESCRIPTIVE STATISTICS ===\n\n")

# Aggregate to video level
video_stats <- joined_data %>%
  group_by(video_id) %>%
  summarize(
    n_comments = n(),
    mean_views = mean(view_count, na.rm = TRUE),
    mean_likes = mean(like_count, na.rm = TRUE),
    mean_subscribers = mean(subscribers, na.rm = TRUE),
    platform = first(platform),
    .groups = "drop"
  ) %>%
  filter(!is.na(mean_views))

cat("Video-level statistics:\n")
cat(sprintf("  N videos: %d\n", nrow(video_stats)))
cat(sprintf("  Mean views: %.0f (SD = %.0f)\n", 
            mean(video_stats$mean_views, na.rm = TRUE),
            sd(video_stats$mean_views, na.rm = TRUE)))
cat(sprintf("  Mean likes: %.0f (SD = %.0f)\n", 
            mean(video_stats$mean_likes, na.rm = TRUE),
            sd(video_stats$mean_likes, na.rm = TRUE)))
cat(sprintf("  Mean comments per video: %.0f (SD = %.0f)\n", 
            mean(video_stats$n_comments, na.rm = TRUE),
            sd(video_stats$n_comments, na.rm = TRUE)))
cat(sprintf("  Mean subscribers: %.0f (SD = %.0f)\n", 
            mean(video_stats$mean_subscribers, na.rm = TRUE),
            sd(video_stats$mean_subscribers, na.rm = TRUE)))

# Platform breakdown
if ("platform" %in% colnames(video_stats)) {
  cat("\nBy platform:\n")
  platform_video_stats <- video_stats %>%
    group_by(platform) %>%
    summarize(
      n_videos = n(),
      mean_views = mean(mean_views, na.rm = TRUE),
      mean_likes = mean(mean_likes, na.rm = TRUE),
      .groups = "drop"
    )
  print(platform_video_stats)
}

# Save results
results <- list(
  video_statistics = video_stats,
  summary_stats = list(
    n_videos = nrow(video_stats),
    mean_views = mean(video_stats$mean_views, na.rm = TRUE),
    mean_likes = mean(video_stats$mean_likes, na.rm = TRUE),
    mean_comments = mean(video_stats$n_comments, na.rm = TRUE)
  )
)

saveRDS(results, "analysis/supplement_s3_sample_descriptives/SM3_video_level_descriptives_results.rds")

cat("\n=== RESULTS SAVED ===\n")
