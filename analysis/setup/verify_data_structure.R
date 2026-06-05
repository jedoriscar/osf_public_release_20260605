# Purpose
# Verify that the data structure matches expectations for all analyses.
# This script checks:
# - Expected columns exist
# - Sample sizes match reported N's in manuscript
# - Missing data patterns
# - Key variable ranges and distributions
#
# Reference: Main text reports N=101,103 comments across 106 videos

# Setup
rm(list = ls())

# Load data
# Use relative path from the public release folder root.
source("analysis/setup/load_data.R")

# Verify sample sizes
cat("=== SAMPLE SIZE VERIFICATION ===\n")
cat("Total comments:", nrow(joined_data), "\n")
cat("Expected (from manuscript): 101,103\n")
cat("Match:", nrow(joined_data) == 101103, "\n\n")

# Count unique videos
if ("video_id" %in% colnames(joined_data)) {
  n_videos <- length(unique(joined_data$video_id))
  cat("Unique videos:", n_videos, "\n")
  cat("Expected: 106\n")
  cat("Match:", n_videos == 106, "\n\n")
}

# Check expected columns
cat("=== COLUMN VERIFICATION ===\n")

# Key columns for main analyses
key_columns <- c(
  "harmoniousness_raw",      # Constructiveness index
  "divisiveness_raw",        # Destructiveness index
  "video_id",                # For multilevel models
  "comment_id",              # For comment-level analyses
  "like_count",               # For RQ2 (rewards)
  "reply_count",              # For RQ2 (rewards)
  "top_comment",              # For algorithmic surfacing
  "platform"                  # YouTube vs TikTok
)

missing <- key_columns[!key_columns %in% colnames(joined_data)]
if (length(missing) > 0) {
  cat("MISSING COLUMNS:\n")
  print(missing)
} else {
  cat("All key columns present.\n")
}

# Check missing data
cat("\n=== MISSING DATA CHECK ===\n")
if ("harmoniousness_raw" %in% colnames(joined_data)) {
  cat("Missing constructiveness:", sum(is.na(joined_data$harmoniousness_raw)), "\n")
}
if ("divisiveness_raw" %in% colnames(joined_data)) {
  cat("Missing destructiveness:", sum(is.na(joined_data$divisiveness_raw)), "\n")
}

# Check variable ranges
cat("\n=== VARIABLE RANGE CHECK ===\n")
if ("harmoniousness_raw" %in% colnames(joined_data)) {
  cat("Constructiveness range:", 
      range(joined_data$harmoniousness_raw, na.rm = TRUE), "\n")
  cat("Expected: 0 to 1\n")
}
if ("divisiveness_raw" %in% colnames(joined_data)) {
  cat("Destructiveness range:", 
      range(joined_data$divisiveness_raw, na.rm = TRUE), "\n")
  cat("Expected: 0 to 1\n")
}

# Platform distribution
cat("\n=== PLATFORM DISTRIBUTION ===\n")
if ("platform" %in% colnames(joined_data)) {
  print(table(joined_data$platform, useNA = "ifany"))
  cat("\nExpected: ~78,196 YouTube, ~22,907 TikTok\n")
}

cat("\n=== VERIFICATION COMPLETE ===\n")
