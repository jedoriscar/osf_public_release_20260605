# Purpose
# Test whether constructiveness and destructiveness predict user replies.
# Uses negative binomial regression to handle overdispersion in count data.
#
# Reference: Main text lines 85-86
# Model: Negative binomial multilevel model with random intercepts for video

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

# Filter to YouTube only (TikTok doesn't have comment-level replies). Use comment-level replies (not video-level reply_count).
analysis_data <- joined_data %>%
  filter(platform == "YouTube" | is.na(platform)) %>%
  mutate(
    comment_replies = as.numeric(replies)
  ) %>%
  filter(!is.na(comment_replies))

# Quick sanity checks
cat("=== DATA CHECKS ===\n")
cat("N comments:", nrow(analysis_data), "\n")
cat("N videos:", length(unique(analysis_data$video_id)), "\n")
cat("Mean replies:", mean(analysis_data$comment_replies, na.rm = TRUE), "\n")
cat("Median replies:", median(analysis_data$comment_replies, na.rm = TRUE), "\n\n")

# Model: Replies (Negative Binomial MLM)
cat("=== MODEL: USER REPLIES ===\n")
cat("Predicting reply counts from constructiveness and destructiveness\n\n")

mod1 <- glmer.nb(
  comment_replies ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

summary(mod1)
confint(mod1, method = "Wald")

# Extract and interpret results
coefs <- fixef(mod1)
ses <- sqrt(diag(vcov(mod1)))
z_vals <- coefs / ses
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
irr_vals <- exp(coefs)
# Wald CI: exp(beta +/- 1.96*SE) for each fixed effect
ci_vals <- rbind(
  exp(coefs[1] + c(-1, 1) * 1.96 * ses[1]),
  exp(coefs[2] + c(-1, 1) * 1.96 * ses[2]),
  exp(coefs[3] + c(-1, 1) * 1.96 * ses[3])
)
rownames(ci_vals) <- names(coefs)

cat("\n=== KEY RESULTS ===\n")
cat("Constructiveness:\n")
cat(sprintf("  B = %.3f (SE = %.3f), z = %.2f, p < .001\n", 
            coefs[2], ses[2], z_vals[2]))
cat(sprintf("  IRR = %.2f, 95%% CI [%.2f, %.2f]\n", 
            irr_vals[2], ci_vals[2,1], ci_vals[2,2]))
cat(sprintf("  Interpretation: Each unit increase in constructiveness multiplies\n"))
cat(sprintf("    expected replies by %.2fx (%.0f%% increase)\n\n", 
            irr_vals[2], (irr_vals[2] - 1) * 100))

cat("Destructiveness:\n")
cat(sprintf("  B = %.3f (SE = %.3f), z = %.2f, p < .001\n", 
            coefs[3], ses[3], z_vals[3]))
cat(sprintf("  IRR = %.2f, 95%% CI [%.2f, %.2f]\n", 
            irr_vals[3], ci_vals[3,1], ci_vals[3,2]))
cat(sprintf("  Interpretation: Each unit increase in destructiveness multiplies\n"))
cat(sprintf("    expected replies by %.2fx (%.0f%% decrease)\n", 
            irr_vals[3], (1 - irr_vals[3]) * 100))

# Save results
results <- list(
  model = mod1,
  constructiveness = list(
    b = coefs[2],
    se = ses[2],
    z = z_vals[2],
    p = p_vals[2],
    irr = irr_vals[2],
    ci = ci_vals[2,]
  ),
  destructiveness = list(
    b = coefs[3],
    se = ses[3],
    z = z_vals[3],
    p = p_vals[3],
    irr = irr_vals[3],
    ci = ci_vals[3,]
  ),
  n = nrow(analysis_data),
  n_videos = length(unique(analysis_data$video_id))
)

saveRDS(results, "analysis/engagement/RQ2_replies_negbinom_results.rds")

cat("\n=== RESULTS SAVED ===\n")
