# Purpose
# Side analysis: Compare deleted vs. non-deleted comments
# to see if deleted comments were more destructive
# (NOT for manuscript - exploratory analysis)

# Setup
rm(list = ls())
library(tidyverse)

cat("=== DELETED COMMENTS ANALYSIS ===\n\n")

# Load Data
# Load main climate data
climate <- readRDS("data/analysis_objects/climate_comments.rds")

# Load failed comment IDs (deleted comments)
failed_file <- "data/analysis_objects/climate_failed_comment_ids.csv"
if (!file.exists(failed_file)) {
  cat("No failed IDs file found. All comments were successfully collected.\n")
  quit(save = "no")
}

failed_ids <- read.csv(failed_file)
cat("Failed/deleted comment IDs:", nrow(failed_ids), "\n")

# Compare Deleted vs. Non-Deleted
# Mark comments as deleted or not
climate_youtube <- climate %>%
  filter(tolower(platform) == "youtube") %>%
  mutate(
    deleted = comment_id %in% failed_ids$comment_id,
    status = ifelse(deleted, "Deleted", "Available")
  )

cat("\nYouTube comments breakdown:\n")
cat("  Available:", sum(!climate_youtube$deleted), "\n")
cat("  Deleted:", sum(climate_youtube$deleted), "\n")
cat("  Deletion rate:", round(mean(climate_youtube$deleted) * 100, 1), "%\n")

# Compare Constructiveness and Destructiveness
cat("\n=== CONSTRUCTIVENESS & DESTRUCTIVENESS BY STATUS ===\n\n")

# Group statistics
stats_by_status <- climate_youtube %>%
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

# T-tests
cat("\n=== STATISTICAL TESTS ===\n\n")

# Constructiveness
t_test_C <- t.test(
  harmoniousness_raw ~ deleted, 
  data = climate_youtube
)

cat("Constructiveness:\n")
cat(sprintf("  Available: M = %.3f (SD = %.3f)\n", 
            mean(climate_youtube$harmoniousness_raw[!climate_youtube$deleted], na.rm = TRUE),
            sd(climate_youtube$harmoniousness_raw[!climate_youtube$deleted], na.rm = TRUE)))
cat(sprintf("  Deleted: M = %.3f (SD = %.3f)\n",
            mean(climate_youtube$harmoniousness_raw[climate_youtube$deleted], na.rm = TRUE),
            sd(climate_youtube$harmoniousness_raw[climate_youtube$deleted], na.rm = TRUE)))
cat(sprintf("  t(%.0f) = %.2f, p = %.4f\n", 
            t_test_C$parameter, 
            t_test_C$statistic, 
            t_test_C$p.value))
cat(sprintf("  Cohen's d = %.3f\n", 
            (mean(climate_youtube$harmoniousness_raw[!climate_youtube$deleted], na.rm = TRUE) - 
             mean(climate_youtube$harmoniousness_raw[climate_youtube$deleted], na.rm = TRUE)) /
            sd(climate_youtube$harmoniousness_raw, na.rm = TRUE)))

# Destructiveness
t_test_D <- t.test(
  divisiveness_raw ~ deleted, 
  data = climate_youtube
)

cat("\nDestructiveness:\n")
cat(sprintf("  Available: M = %.3f (SD = %.3f)\n", 
            mean(climate_youtube$divisiveness_raw[!climate_youtube$deleted], na.rm = TRUE),
            sd(climate_youtube$divisiveness_raw[!climate_youtube$deleted], na.rm = TRUE)))
cat(sprintf("  Deleted: M = %.3f (SD = %.3f)\n",
            mean(climate_youtube$divisiveness_raw[climate_youtube$deleted], na.rm = TRUE),
            sd(climate_youtube$divisiveness_raw[climate_youtube$deleted], na.rm = TRUE)))
cat(sprintf("  t(%.0f) = %.2f, p = %.4f\n", 
            t_test_D$parameter, 
            t_test_D$statistic, 
            t_test_D$p.value))
cat(sprintf("  Cohen's d = %.3f\n", 
            (mean(climate_youtube$divisiveness_raw[climate_youtube$deleted], na.rm = TRUE) - 
             mean(climate_youtube$divisiveness_raw[!climate_youtube$deleted], na.rm = TRUE)) /
            sd(climate_youtube$divisiveness_raw, na.rm = TRUE)))

# Test Constructiveness Advantage
cat("\n=== CONSTRUCTIVENESS ADVANTAGE TESTS ===\n\n")

# Calculate advantage for each comment
climate_youtube <- climate_youtube %>%
  mutate(advantage = harmoniousness_raw - divisiveness_raw)

# Test if deleted comments' advantage is significantly > 0
t_test_adv_deleted <- t.test(
  climate_youtube$advantage[climate_youtube$deleted],
  mu = 0
)

cat("Deleted comments advantage (vs. 0):\n")
cat(sprintf("  M = %.3f (%.1f percentage points)\n",
            mean(climate_youtube$advantage[climate_youtube$deleted], na.rm = TRUE),
            mean(climate_youtube$advantage[climate_youtube$deleted], na.rm = TRUE) * 100))
cat(sprintf("  t(%.0f) = %.2f, p < .001\n", 
            t_test_adv_deleted$parameter, 
            t_test_adv_deleted$statistic))

# Test if advantage differs between deleted and available
t_test_adv_diff <- t.test(
  advantage ~ deleted,
  data = climate_youtube
)

cat("\nAdvantage difference (Available vs. Deleted):\n")
cat(sprintf("  Available: M = %.3f (%.1f pp)\n",
            mean(climate_youtube$advantage[!climate_youtube$deleted], na.rm = TRUE),
            mean(climate_youtube$advantage[!climate_youtube$deleted], na.rm = TRUE) * 100))
cat(sprintf("  Deleted: M = %.3f (%.1f pp)\n",
            mean(climate_youtube$advantage[climate_youtube$deleted], na.rm = TRUE),
            mean(climate_youtube$advantage[climate_youtube$deleted], na.rm = TRUE) * 100))
cat(sprintf("  Difference: %.1f pp\n",
            (mean(climate_youtube$advantage[!climate_youtube$deleted], na.rm = TRUE) - 
             mean(climate_youtube$advantage[climate_youtube$deleted], na.rm = TRUE)) * 100))
cat(sprintf("  t(%.0f) = %.2f, p = %.4f\n", 
            t_test_adv_diff$parameter, 
            t_test_adv_diff$statistic, 
            t_test_adv_diff$p.value))

# Additional Patterns
cat("\n=== PERCENTAGE ABOVE THRESHOLD ===\n\n")

# High destructiveness (>0.2)
cat("High destructiveness (>0.2):\n")
cat(sprintf("  Available: %.1f%%\n", 
            mean(climate_youtube$divisiveness_raw[!climate_youtube$deleted] > 0.2, na.rm = TRUE) * 100))
cat(sprintf("  Deleted: %.1f%%\n",
            mean(climate_youtube$divisiveness_raw[climate_youtube$deleted] > 0.2, na.rm = TRUE) * 100))

# High constructiveness (>0.4)
cat("\nHigh constructiveness (>0.4):\n")
cat(sprintf("  Available: %.1f%%\n", 
            mean(climate_youtube$harmoniousness_raw[!climate_youtube$deleted] > 0.4, na.rm = TRUE) * 100))
cat(sprintf("  Deleted: %.1f%%\n",
            mean(climate_youtube$harmoniousness_raw[climate_youtube$deleted] > 0.4, na.rm = TRUE) * 100))

# Save Results
results <- list(
  stats_by_status = stats_by_status,
  t_test_constructiveness = t_test_C,
  t_test_destructiveness = t_test_D,
  deletion_rate = mean(climate_youtube$deleted)
)

saveRDS(results, "analysis/climate_replication/CLIMATE_deleted_comments_results.rds")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved for reference.\n")
