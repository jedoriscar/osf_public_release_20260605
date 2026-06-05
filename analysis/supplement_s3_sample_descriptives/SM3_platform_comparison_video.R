# Purpose
# Compare video-level statistics across platforms (YouTube vs TikTok).
# Reports platform differences in views, likes, comments, subscribers.
#
# Reference: SI Appendix Section 3.3

# Setup
rm(list = ls())
library(dplyr)

# Load data
load("data/analysis_objects/racial_comments.rda")

if (!"platform" %in% colnames(joined_data) && "platform_source.y" %in% colnames(joined_data)) {
  joined_data$platform <- joined_data$platform_source.y
}

# Platform comparison at video level
cat("=== PLATFORM COMPARISON (VIDEO LEVEL) ===\n\n")

# Aggregate to video level
video_stats <- joined_data %>%
  group_by(video_id) %>%
  summarize(
    n_comments = n(),
    views = first(view_count),
    likes = first(like_count),
    subscribers = first(subscribers),
    platform = first(platform),
    .groups = "drop"
  ) %>%
  filter(!is.na(platform))

# Platform statistics
platform_stats <- video_stats %>%
  group_by(platform) %>%
  summarize(
    n_videos = n(),
    mean_views = mean(views, na.rm = TRUE),
    mean_likes = mean(likes, na.rm = TRUE),
    mean_comments = mean(n_comments, na.rm = TRUE),
    mean_subscribers = mean(subscribers, na.rm = TRUE),
    .groups = "drop"
  )

cat("Platform video statistics:\n")
print(platform_stats)

# Statistical tests
if ("YouTube" %in% platform_stats$platform && "TikTok" %in% platform_stats$platform) {
  youtube_videos <- video_stats %>% filter(platform == "YouTube")
  tiktok_videos <- video_stats %>% filter(platform == "TikTok")
  
  cat("\nStatistical tests:\n")
  
  if (nrow(youtube_videos) > 0 && nrow(tiktok_videos) > 0) {
    ttest_views <- t.test(views ~ platform, data = video_stats)
    cat(sprintf("  Views: t = %.2f, p = %.3f\n", 
                ttest_views$statistic, ttest_views$p.value))
    
    ttest_likes <- t.test(likes ~ platform, data = video_stats)
    cat(sprintf("  Likes: t = %.2f, p = %.3f\n", 
                ttest_likes$statistic, ttest_likes$p.value))
  }
}

# Save results
results <- list(
  platform_statistics = platform_stats,
  video_level_data = video_stats
)

saveRDS(results, "analysis/supplement_s3_sample_descriptives/SM3_platform_comparison_video_results.rds")

cat("\n=== RESULTS SAVED ===\n")
