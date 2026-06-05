# Purpose
# Test whether individual constructive feature (compassion) predicts engagement outcomes
# (likes, replies, algorithmic surfacing) independently of other features.
#
# This addresses whether composite index effects mask heterogeneity across features.
#
# Reference: SI Appendix Section 4.6, Main text line 270
# Models: Negative binomial MLM for likes/replies, logistic MLM for algorithmic surfacing

# Setup
rm(list = ls())
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

# Filter to YouTube only (TikTok doesn't have engagement metrics)
analysis_data <- joined_data %>%
  filter(platform == "YouTube" | is.na(platform)) %>%
  mutate(
    like_count = as.numeric(like_count),
    reply_count = as.numeric(reply_count),
    top_comment_binary = ifelse(top_comment == 1 | top_comment == TRUE, 1, 0)
  ) %>%
  filter(!is.na(like_count), !is.na(reply_count))

# Create binary compassion indicator (≥0.6 threshold)
# Adjust column name based on actual data structure
if ("compassion" %in% colnames(analysis_data)) {
  analysis_data$compassion_binary <- ifelse(analysis_data$compassion >= 0.6, 1, 0)
} else if ("compassion_raw" %in% colnames(analysis_data)) {
  analysis_data$compassion_binary <- ifelse(analysis_data$compassion_raw >= 0.6, 1, 0)
} else if ("prob_compassion" %in% colnames(analysis_data)) {
  analysis_data$compassion_binary <- ifelse(analysis_data$prob_compassion >= 0.6, 1, 0)
} else {
  stop("Compassion column not found. Check column names.")
}

# Quick sanity checks
cat("=== DATA CHECKS ===\n")
cat("N comments:", nrow(analysis_data), "\n")
cat("N videos:", length(unique(analysis_data$video_id)), "\n")
cat("Compassion prevalence:", mean(analysis_data$compassion_binary, na.rm = TRUE), "\n\n")

# Model 1: Likes
cat("=== MODEL 1: COMPASSION PREDICTING LIKES ===\n")

mod1_likes <- glmer.nb(
  like_count ~ compassion_binary + (1|video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

summary(mod1_likes)
confint(mod1_likes, method = "Wald")

coefs_likes <- fixef(mod1_likes)
ses_likes <- sqrt(diag(vcov(mod1_likes)))
irr_likes <- exp(coefs_likes)

cat("\nCompassion → Likes:\n")
cat(sprintf("  B = %.3f (SE = %.3f), IRR = %.2f\n", 
            coefs_likes[2], ses_likes[2], irr_likes[2]))

# Model 2: Replies
cat("\n=== MODEL 2: COMPASSION PREDICTING REPLIES ===\n")

mod1_replies <- glmer.nb(
  reply_count ~ compassion_binary + (1|video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

summary(mod1_replies)
confint(mod1_replies, method = "Wald")

coefs_replies <- fixef(mod1_replies)
ses_replies <- sqrt(diag(vcov(mod1_replies)))
irr_replies <- exp(coefs_replies)

cat("\nCompassion → Replies:\n")
cat(sprintf("  B = %.3f (SE = %.3f), IRR = %.2f\n", 
            coefs_replies[2], ses_replies[2], irr_replies[2]))

# Model 3: Algorithmic Surfacing
cat("\n=== MODEL 3: COMPASSION PREDICTING ALGORITHMIC SURFACING ===\n")

mod1_algorithmic <- glmer(
  top_comment_binary ~ compassion_binary + (1|video_id),
  data = analysis_data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

summary(mod1_algorithmic)
confint(mod1_algorithmic, method = "Wald")

coefs_alg <- fixef(mod1_algorithmic)
ses_alg <- sqrt(diag(vcov(mod1_algorithmic)))
or_alg <- exp(coefs_alg)

cat("\nCompassion → Algorithmic Surfacing:\n")
cat(sprintf("  Log Odds = %.3f (SE = %.3f), OR = %.2f\n", 
            coefs_alg[2], ses_alg[2], or_alg[2]))

# Save results
results <- list(
  likes_model = mod1_likes,
  replies_model = mod1_replies,
  algorithmic_model = mod1_algorithmic,
  likes_IRR = irr_likes[2],
  replies_IRR = irr_replies[2],
  algorithmic_OR = or_alg[2],
  n = nrow(analysis_data)
)

saveRDS(results, "analysis/supplement_s4_6_feature_specific_models/SM4.6_compassion_engagement_results.rds")

cat("\n=== RESULTS SAVED ===\n")
