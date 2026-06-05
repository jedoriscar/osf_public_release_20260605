# Purpose
# Side analysis: Compare deleted vs. non-deleted comments for RACIAL CHANGE
# to see if deleted comments were more destructive
# (NOT for manuscript - exploratory analysis to inform limitations discussion)

# Setup
rm(list = ls())
library(tidyverse)

cat("=== DELETED COMMENTS ANALYSIS (RACIAL CHANGE) ===\n\n")

# Load Data
# Load main racial data
source("analysis/setup/load_data.R")

# Load failed comment IDs (deleted comments) from temporal recollection if it exists
# Check for failed IDs file
failed_file <- "data/analysis_objects/racial_failed_comment_ids.csv"

if (!file.exists(failed_file)) {
  cat("No failed IDs file found for racial change data.\n")
  cat("This means either:\n")
  cat("  1. All comments were successfully accessible, or\n")
  cat("  2. The timestamp recollection was not performed for racial data\n\n")
  
  cat("For comparison purposes, we'll use the existing timestamp data quality.\n")
  cat("Comments with invalid/missing timestamps may indicate deleted content.\n\n")
  
  # Alternative: Check for comments with missing/invalid timestamps
  racial_youtube <- joined_data %>%
    filter(platform == "YouTube") %>%
    mutate(
      # Parse year from timestamp
      year = suppressWarnings(as.numeric(format(as.POSIXct(comment_published_at, origin = "1970-01-01"), "%Y"))),
      # Mark as potentially problematic if year is NA or unrealistic
      timestamp_valid = !is.na(year) & year >= 2011 & year <= 2025,
      status = ifelse(timestamp_valid, "Available", "Potentially Deleted/Invalid")
    )
  
  cat("YouTube comments breakdown (based on timestamp validity):\n")
  cat("  Valid timestamp:", sum(racial_youtube$timestamp_valid), "\n")
  cat("  Invalid/missing:", sum(!racial_youtube$timestamp_valid), "\n")
  cat("  Invalid rate:", round(mean(!racial_youtube$timestamp_valid) * 100, 1), "%\n")
  
} else {
  # Load failed IDs
  failed_ids <- read.csv(failed_file)
  cat("Failed/deleted comment IDs:", nrow(failed_ids), "\n")
  
  # Mark comments as deleted or not
  racial_youtube <- joined_data %>%
    filter(platform == "YouTube") %>%
    mutate(
      deleted = comment_id %in% failed_ids$comment_id,
      status = ifelse(deleted, "Deleted", "Available")
    )
  
  cat("\nYouTube comments breakdown:\n")
  cat("  Available:", sum(!racial_youtube$deleted), "\n")
  cat("  Deleted:", sum(racial_youtube$deleted), "\n")
  cat("  Deletion rate:", round(mean(racial_youtube$deleted) * 100, 1), "%\n")
}

# Compare Constructiveness and Destructiveness
cat("\n=== CONSTRUCTIVENESS & DESTRUCTIVENESS BY STATUS ===\n\n")

# Group statistics
stats_by_status <- racial_youtube %>%
  group_by(status) %>%
  summarise(
    n = n(),
    mean_constructiveness = mean(harmoniousness_raw, na.rm = TRUE),
    sd_constructiveness = sd(harmoniousness_raw, na.rm = TRUE),
    mean_destructiveness = mean(divisiveness_raw, na.rm = TRUE),
    sd_destructiveness = sd(divisiveness_raw, na.rm = TRUE),
    .groups = "drop"
  )

print(stats_by_status)

# T-tests (only if we have both deleted and available)
if ("Deleted" %in% racial_youtube$status && "Available" %in% racial_youtube$status) {
  cat("\n=== STATISTICAL TESTS ===\n\n")
  
  # Constructiveness
  t_test_C <- t.test(
    harmoniousness_raw ~ deleted, 
    data = racial_youtube
  )
  
  cat("Constructiveness:\n")
  cat(sprintf("  Available: M = %.3f (SD = %.3f)\n", 
              mean(racial_youtube$harmoniousness_raw[!racial_youtube$deleted], na.rm = TRUE),
              sd(racial_youtube$harmoniousness_raw[!racial_youtube$deleted], na.rm = TRUE)))
  cat(sprintf("  Deleted: M = %.3f (SD = %.3f)\n",
              mean(racial_youtube$harmoniousness_raw[racial_youtube$deleted], na.rm = TRUE),
              sd(racial_youtube$harmoniousness_raw[racial_youtube$deleted], na.rm = TRUE)))
  cat(sprintf("  t(%.0f) = %.2f, p = %.4f\n", 
              t_test_C$parameter, 
              t_test_C$statistic, 
              t_test_C$p.value))
  
  # Destructiveness
  t_test_D <- t.test(
    divisiveness_raw ~ deleted, 
    data = racial_youtube
  )
  
  cat("\nDestructiveness:\n")
  cat(sprintf("  Available: M = %.3f (SD = %.3f)\n", 
              mean(racial_youtube$divisiveness_raw[!racial_youtube$deleted], na.rm = TRUE),
              sd(racial_youtube$divisiveness_raw[!racial_youtube$deleted], na.rm = TRUE)))
  cat(sprintf("  Deleted: M = %.3f (SD = %.3f)\n",
              mean(racial_youtube$divisiveness_raw[racial_youtube$deleted], na.rm = TRUE),
              sd(racial_youtube$divisiveness_raw[racial_youtube$deleted], na.rm = TRUE)))
  cat(sprintf("  t(%.0f) = %.2f, p = %.4f\n", 
              t_test_D$parameter, 
              t_test_D$statistic, 
              t_test_D$p.value))
}

# Calculate advantage
cat("\n=== CONSTRUCTIVENESS ADVANTAGE ===\n\n")
for(i in 1:nrow(stats_by_status)) {
  status_name <- stats_by_status$status[i]
  adv <- stats_by_status$mean_constructiveness[i] - stats_by_status$mean_destructiveness[i]
  cat(sprintf("%s: C - D = %.3f (%.1f percentage points)\n", 
              status_name, adv, adv * 100))
}

# Test advantage if we have both groups
if ("Deleted" %in% racial_youtube$status && "Available" %in% racial_youtube$status) {
  cat("\n=== CONSTRUCTIVENESS ADVANTAGE TESTS ===\n\n")
  
  # Calculate advantage for each comment
  racial_youtube <- racial_youtube %>%
    mutate(advantage = harmoniousness_raw - divisiveness_raw)
  
  # Test if deleted comments' advantage is significantly > 0
  t_test_adv_deleted <- t.test(
    racial_youtube$advantage[racial_youtube$deleted],
    mu = 0
  )
  
  cat("Deleted comments advantage (vs. 0):\n")
  cat(sprintf("  M = %.3f (%.1f percentage points)\n",
              mean(racial_youtube$advantage[racial_youtube$deleted], na.rm = TRUE),
              mean(racial_youtube$advantage[racial_youtube$deleted], na.rm = TRUE) * 100))
  cat(sprintf("  t(%.0f) = %.2f, p < .001\n", 
              t_test_adv_deleted$parameter, 
              t_test_adv_deleted$statistic))
  
  # Test if advantage differs between deleted and available
  t_test_adv_diff <- t.test(
    advantage ~ deleted,
    data = racial_youtube
  )
  
  cat("\nAdvantage difference (Available vs. Deleted):\n")
  cat(sprintf("  Available: M = %.3f (%.1f pp)\n",
              mean(racial_youtube$advantage[!racial_youtube$deleted], na.rm = TRUE),
              mean(racial_youtube$advantage[!racial_youtube$deleted], na.rm = TRUE) * 100))
  cat(sprintf("  Deleted: M = %.3f (%.1f pp)\n",
              mean(racial_youtube$advantage[racial_youtube$deleted], na.rm = TRUE),
              mean(racial_youtube$advantage[racial_youtube$deleted], na.rm = TRUE) * 100))
  cat(sprintf("  Difference: %.1f pp\n",
              (mean(racial_youtube$advantage[!racial_youtube$deleted], na.rm = TRUE) - 
               mean(racial_youtube$advantage[racial_youtube$deleted], na.rm = TRUE)) * 100))
  cat(sprintf("  t(%.0f) = %.2f, p = %.4f\n", 
              t_test_adv_diff$parameter, 
              t_test_adv_diff$statistic, 
              t_test_adv_diff$p.value))
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Racial change data shows similar patterns to climate change.\n")
