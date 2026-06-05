# Purpose
# Test whether individual destructive feature (attack on author) predicts engagement outcomes.
# Reference: SI Appendix Section 4.6

# Setup
rm(list = ls())
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

analysis_data <- joined_data %>%
  filter(platform == "YouTube" | is.na(platform)) %>%
  mutate(
    like_count = as.numeric(like_count),
    reply_count = as.numeric(reply_count),
    top_comment_binary = ifelse(top_comment == 1 | top_comment == TRUE, 1, 0),
    attack_author_binary = as.numeric(prob_attack_on_author >= 0.6)
  ) %>%
  filter(!is.na(like_count), !is.na(reply_count))

# Models
mod1_likes <- glmer.nb(like_count ~ attack_author_binary + (1|video_id), data = analysis_data,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
mod1_replies <- glmer.nb(reply_count ~ attack_author_binary + (1|video_id), data = analysis_data,
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
mod1_alg <- glmer(top_comment_binary ~ attack_author_binary + (1|video_id), data = analysis_data,
                  family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(mod1_likes)
summary(mod1_replies)
summary(mod1_alg)

# Save
results <- list(likes_model = mod1_likes, replies_model = mod1_replies, algorithmic_model = mod1_alg)
saveRDS(results, "analysis/supplement_s4_6_feature_specific_models/SM4.6_attack_author_engagement_results.rds")
