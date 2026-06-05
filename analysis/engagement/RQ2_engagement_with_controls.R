# Purpose
# Test whether constructiveness and destructiveness predict engagement outcomes
# when controlling for sentiment, politeness, and moral outrage.
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

if (!"sentiment_positive" %in% colnames(analysis_data) && "vader_positive" %in% colnames(analysis_data)) {
  analysis_data$sentiment_positive <- analysis_data$vader_positive
}
if (!"sentiment_negative" %in% colnames(analysis_data) && "vader_negative" %in% colnames(analysis_data)) {
  analysis_data$sentiment_negative <- analysis_data$vader_negative
}
if (!"moral_outrage_binary" %in% colnames(analysis_data)) {
  if ("label_moral_outrage" %in% colnames(analysis_data)) {
    analysis_data$moral_outrage_binary <- as.numeric(analysis_data$label_moral_outrage %in% c(1, TRUE, "1", "true", "TRUE"))
  } else if ("prob_moral_outrage" %in% colnames(analysis_data)) {
    analysis_data$moral_outrage_binary <- as.numeric(analysis_data$prob_moral_outrage >= 0.6)
  }
}

control_vars <- c("sentiment_positive", "sentiment_negative", "politeness", "moral_outrage_binary")
has_controls <- all(control_vars %in% colnames(analysis_data))

# Model 1: Algorithmic Surfacing with Controls
cat("=== MODEL 1: ALGORITHMIC SURFACING WITH CONTROLS ===\n")

if (has_controls) {
  mod1_alg <- glmer(
    top_comment_binary ~ harmoniousness_raw + divisiveness_raw + 
      sentiment_positive + sentiment_negative + politeness + moral_outrage_binary + (1|video_id),
    data = analysis_data,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  )
  summary(mod1_alg)
} else {
  cat("Controlled surfacing model not estimated because one or more public control variables are unavailable.\n")
  mod1_alg <- NULL
}

# Model 2: Likes with Controls
cat("\n=== MODEL 2: LIKES WITH CONTROLS ===\n")

if (has_controls) {
  mod1_likes <- glmer.nb(
    comment_likes ~ harmoniousness_raw + divisiveness_raw + 
      sentiment_positive + sentiment_negative + politeness + moral_outrage_binary + (1|video_id),
    data = analysis_data,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  )
  summary(mod1_likes)
} else {
  cat("Controlled likes model not estimated because one or more public control variables are unavailable.\n")
  mod1_likes <- NULL
}

# Model 3: Replies with Controls
cat("\n=== MODEL 3: REPLIES WITH CONTROLS ===\n")

if (has_controls) {
  mod1_replies <- glmer.nb(
    comment_replies ~ harmoniousness_raw + divisiveness_raw + 
      sentiment_positive + sentiment_negative + politeness + moral_outrage_binary + (1|video_id),
    data = analysis_data,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  )
  summary(mod1_replies)
} else {
  cat("Controlled replies model not estimated because one or more public control variables are unavailable.\n")
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
cat("Control variables used:", paste(control_vars[control_vars %in% colnames(analysis_data)], collapse = ", "), "\n")
