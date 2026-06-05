# Purpose
# Compare constructiveness and destructiveness across platforms (YouTube vs TikTok).
# Tests whether platform differences explain the prevalence findings.
#
# Reference: Main text line 64

# Setup
rm(list = ls())
library(tidyverse)   # dplyr (%, filter, group_by, summarize)

# Load data
source("analysis/setup/load_data.R")

# Filter to comments with platform information
analysis_data <- joined_data %>%
  filter(!is.na(platform))

# Quick sanity checks
cat("=== PLATFORM COMPARISON ===\n")
cat("N YouTube:", sum(analysis_data$platform == "YouTube"), "\n")
cat("N TikTok:", sum(analysis_data$platform == "TikTok"), "\n\n")

# Platform descriptives
platform_stats <- analysis_data %>%
  group_by(platform) %>%
  summarize(
    n = n(),
    mean_C = mean(harmoniousness_raw, na.rm = TRUE),
    sd_C = sd(harmoniousness_raw, na.rm = TRUE),
    mean_D = mean(divisiveness_raw, na.rm = TRUE),
    sd_D = sd(divisiveness_raw, na.rm = TRUE)
  )

cat("Platform Statistics:\n")
print(platform_stats)

# Statistical test
cat("\n=== STATISTICAL TESTS ===\n")

# Constructiveness by platform
t_test_C <- t.test(harmoniousness_raw ~ platform, data = analysis_data)
cat("Constructiveness by platform:\n")
cat(sprintf("  t = %.2f, p = %.3f\n", t_test_C$statistic, t_test_C$p.value))
cat(sprintf("  YouTube M = %.3f, TikTok M = %.3f\n",
            mean(analysis_data$harmoniousness_raw[analysis_data$platform == "YouTube"], na.rm = TRUE),
            mean(analysis_data$harmoniousness_raw[analysis_data$platform == "TikTok"], na.rm = TRUE)))

# Destructiveness by platform
t_test_D <- t.test(divisiveness_raw ~ platform, data = analysis_data)
cat("\nDestructiveness by platform:\n")
cat(sprintf("  t = %.2f, p = %.3f\n", t_test_D$statistic, t_test_D$p.value))
cat(sprintf("  YouTube M = %.3f, TikTok M = %.3f\n",
            mean(analysis_data$divisiveness_raw[analysis_data$platform == "YouTube"], na.rm = TRUE),
            mean(analysis_data$divisiveness_raw[analysis_data$platform == "TikTok"], na.rm = TRUE)))

# Cohen's d (independent samples, pooled SD)
# d = (M1 - M2) / SD_pooled; positive = YouTube > TikTok for display
n_yt <- sum(analysis_data$platform == "YouTube")
n_tt <- sum(analysis_data$platform == "TikTok")
s_yt_C <- platform_stats$sd_C[platform_stats$platform == "YouTube"]
s_tt_C <- platform_stats$sd_C[platform_stats$platform == "TikTok"]
s_yt_D <- platform_stats$sd_D[platform_stats$platform == "YouTube"]
s_tt_D <- platform_stats$sd_D[platform_stats$platform == "TikTok"]
pooled_SD_C <- sqrt(((n_yt - 1) * s_yt_C^2 + (n_tt - 1) * s_tt_C^2) / (n_yt + n_tt - 2))
pooled_SD_D <- sqrt(((n_yt - 1) * s_yt_D^2 + (n_tt - 1) * s_tt_D^2) / (n_yt + n_tt - 2))
d_constructiveness <- (platform_stats$mean_C[platform_stats$platform == "YouTube"] -
                       platform_stats$mean_C[platform_stats$platform == "TikTok"]) / pooled_SD_C
d_destructiveness  <- (platform_stats$mean_D[platform_stats$platform == "YouTube"] -
                       platform_stats$mean_D[platform_stats$platform == "TikTok"]) / pooled_SD_D

cat("\nCohen's d (YouTube vs TikTok):\n")
cat(sprintf("  Constructiveness: d = %.2f\n", d_constructiveness))
cat(sprintf("  Destructiveness:  d = %.2f\n", d_destructiveness))

# Save results
results <- list(
  platform_stats = platform_stats,
  t_test_constructiveness = t_test_C,
  t_test_destructiveness = t_test_D,
  cohens_d_constructiveness = as.numeric(d_constructiveness),
  cohens_d_destructiveness = as.numeric(d_destructiveness)
)

saveRDS(results, "analysis/prevalence/RQ1_platform_comparison_results.rds")

cat("\n=== RESULTS SAVED ===\n")
