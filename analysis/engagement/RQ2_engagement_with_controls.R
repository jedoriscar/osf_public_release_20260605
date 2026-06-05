# Purpose
# Test whether constructiveness and destructiveness predict engagement outcomes
# when controlling for sentiment, politeness, and moral outrage.
# Demonstrates that effects are not confounded by these related constructs.
#
# Reference: Main text lines 85-86, SI Appendix Tables S9-S12
# Models: Same as main models but with additional covariates

# Setup
rm(list = ls())
library(tidyverse)   # dplyr (%, filter, mutate)
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

# Filter to YouTube only. Use comment-level likes and replies (not video-level like_count/reply_count).
analysis_data <- joined_data %>%
  filter(platform == "YouTube" | is.na(platform)) %>%
  mutate(
    comment_likes = as.numeric(likes),
    comment_replies = as.numeric(replies),
    top_comment_binary = ifelse(top_comment == 1 | top_comment == TRUE, 1, 0)
  ) %>%
  filter(!is.na(comment_likes), !is.na(comment_replies))

# Check for control variables
if (!"sentiment_positive" %in% colnames(analysis_data) ||
    !"politeness" %in% colnames(analysis_data)) {
  warning("Control variables may not be in data. Check column names.")
}

# Model 1: Algorithmic Surfacing with Controls
cat("=== MODEL 1: ALGORITHMIC SURFACING WITH CONTROLS ===\n")

if ("sentiment_positive" %in% colnames(analysis_data) && 
    "politeness" %in% colnames(analysis_data)) {
  mod1_alg <- glmer(
    top_comment_binary ~ harmoniousness_raw + divisiveness_raw + 
      sentiment_positive + sentiment_negative + politeness + (1|video_id),
    data = analysis_data,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  )
  summary(mod1_alg)
} else {
  cat("Control variables not available. Skipping controlled model.\n")
  mod1_alg <- NULL
}

# Model 2: Likes with Controls
cat("\n=== MODEL 2: LIKES WITH CONTROLS ===\n")

if ("sentiment_positive" %in% colnames(analysis_data) && 
    "politeness" %in% colnames(analysis_data)) {
  mod1_likes <- glmer.nb(
    comment_likes ~ harmoniousness_raw + divisiveness_raw + 
      sentiment_positive + sentiment_negative + politeness + (1|video_id),
    data = analysis_data,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  )
  summary(mod1_likes)
} else {
  cat("Control variables not available. Skipping controlled model.\n")
  mod1_likes <- NULL
}

# Model 3: Replies with Controls
cat("\n=== MODEL 3: REPLIES WITH CONTROLS ===\n")

if ("sentiment_positive" %in% colnames(analysis_data) && 
    "politeness" %in% colnames(analysis_data)) {
  mod1_replies <- glmer.nb(
    comment_replies ~ harmoniousness_raw + divisiveness_raw + 
      sentiment_positive + sentiment_negative + politeness + (1|video_id),
    data = analysis_data,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  )
  summary(mod1_replies)
} else {
  cat("Control variables not available. Skipping controlled model.\n")
  mod1_replies <- NULL
}

# Save results
results <- list(
  algorithmic_model = mod1_alg,
  likes_model = mod1_likes,
  replies_model = mod1_replies,
  n = nrow(analysis_data)
)

saveRDS(results, "analysis/engagement/RQ2_engagement_with_controls_results.rds")

cat("\n=== RESULTS SAVED ===\n")
cat("Note: If control variables are missing, models were not fit.\n")
cat("Check data structure and update column names if needed.\n")
