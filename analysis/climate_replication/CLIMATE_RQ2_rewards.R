# Purpose
# Replicate RQ2 (rewards) analyses in climate change dataset.
# Tests whether constructiveness is rewarded with more engagement.
#
# Reference: Main text 10_results_replication

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

# Load climate data
climate_data_path <- "data/analysis_objects/climate_comments.rds"

if (!file.exists(climate_data_path)) stop("Climate data not found.")

joined_data <- readRDS(climate_data_path)

# Climate uses "likes" and "replies" (comment-level)
# Platform in climate data is lowercase "youtube"
analysis_data <- joined_data %>%
  filter(tolower(platform) == "youtube" | is.na(platform)) %>%
  mutate(
    like_count = as.numeric(likes),
    reply_count = as.numeric(replies)
  ) %>%
  filter(!is.na(like_count), !is.na(reply_count), nchar(trimws(video_id)) > 0) %>%
  droplevels()

# Data checks (align with racial RQ2)
cat("=== CLIMATE REPLICATION: RQ2 REWARDS ===\n")
cat("N comments:", nrow(analysis_data), "\n")
cat("N videos:", length(unique(analysis_data$video_id)), "\n")
cat("Mean likes:", mean(analysis_data$like_count, na.rm = TRUE), "\n")
cat("Median likes:", median(analysis_data$like_count, na.rm = TRUE), "\n")
cat("Mean replies:", mean(analysis_data$reply_count, na.rm = TRUE), "\n")
cat("Median replies:", median(analysis_data$reply_count, na.rm = TRUE), "\n\n")

# Model 1: Likes

mod1_likes <- glmer.nb(
  like_count ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

coefs_likes <- fixef(mod1_likes)
irr_likes <- exp(coefs_likes)

cat("Likes:\n")
cat(sprintf("  Constructiveness IRR = %.2f\n", irr_likes[2]))
cat(sprintf("  Destructiveness IRR = %.2f\n", irr_likes[3]))

# Model 2: Replies
mod1_replies <- glmer.nb(
  reply_count ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

coefs_replies <- fixef(mod1_replies)
irr_replies <- exp(coefs_replies)

cat("\nReplies:\n")
cat(sprintf("  Constructiveness IRR = %.2f\n", irr_replies[2]))
cat(sprintf("  Destructiveness IRR = %.2f\n", irr_replies[3]))

cat("\nN =", nrow(analysis_data), "\n")

# Save results
results <- list(
  likes_model = mod1_likes,
  replies_model = mod1_replies,
  irr_likes = irr_likes,
  irr_replies = irr_replies,
  n = nrow(analysis_data)
)

saveRDS(results, "analysis/climate_replication/CLIMATE_RQ2_rewards_results.rds")

cat("\n=== RESULTS SAVED ===\n")
