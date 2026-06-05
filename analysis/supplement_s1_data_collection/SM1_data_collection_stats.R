# Purpose
# Calculate descriptive statistics for data collection process.
# Reports number of videos, comments, platform distribution.
#
# Reference: SI Appendix Section 1.1
# Reports: 106 videos, 101,103 comments, 59 YouTube, 47 TikTok

# Setup
rm(list = ls())

# Load data
source("analysis/setup/load_data.R")

# Calculate collection statistics
cat("=== DATA COLLECTION STATISTICS ===\n\n")

n_comments <- nrow(joined_data)
n_videos <- length(unique(joined_data$video_id))

cat("Total comments:", n_comments, "\n")
cat("Total videos:", n_videos, "\n\n")

# Platform distribution
if ("platform" %in% colnames(joined_data)) {
  platform_dist <- table(joined_data$platform, useNA = "ifany")
  platform_prop <- prop.table(platform_dist) * 100
  
  cat("Platform Distribution:\n")
  for (i in 1:length(platform_dist)) {
    cat(sprintf("  %s: n = %d (%.1f%%)\n", 
                names(platform_dist)[i], 
                platform_dist[i], 
                platform_prop[i]))
  }
}

# Video-level statistics
if ("video_id" %in% colnames(joined_data)) {
  video_stats <- joined_data %>%
    group_by(video_id) %>%
    summarize(n_comments = n(), .groups = "drop")
  
  cat("\nComments per video:\n")
  cat(sprintf("  Mean: %.1f\n", mean(video_stats$n_comments)))
  cat(sprintf("  Median: %.0f\n", median(video_stats$n_comments)))
  cat(sprintf("  Range: %d to %d\n", 
              min(video_stats$n_comments), 
              max(video_stats$n_comments)))
}

# Save results
results <- list(
  n_comments = n_comments,
  n_videos = n_videos,
  platform_distribution = if(exists("platform_dist")) platform_dist else NULL,
  n_comments_per_video = if(exists("video_stats")) video_stats$n_comments else NULL
)

saveRDS(results, "analysis/supplement_s1_data_collection/SM1_data_collection_stats_results.rds")

cat("\n=== RESULTS SAVED ===\n")
