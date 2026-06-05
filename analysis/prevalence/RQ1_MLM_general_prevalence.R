# Purpose
# Test whether constructiveness exceeds destructiveness using a multilevel model
# that accounts for clustering by video. We compare each comment's own C score to
# its own D score (within-comment comparison), with random effects for comment and video.
#
# Design: Pivot data to long format so each comment has two rows—one for its
# constructiveness score, one for its destructiveness score. Then we model
# score ~ discourse_type (Constructiveness vs Destructiveness) with random
# intercepts for comment_id and video_id. The coefficient for discourse_type
# is the mean difference (C − D) on the 0–1 scale. This is an identity (linear)
# link: do NOT exponentiate (exponentiation is for log or logit links, e.g. OR/IRR).
#
# Reference: Main text line 56, SI Appendix Section 4.4

# Setup
rm(list = ls())
library(tidyverse)   # dplyr (%, select, mutate, filter, pull), tidyr (pivot_longer)
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

# Reshape to long format (each row → two rows: C and D)
# Use row_id (from load_data.R): one per row for both YouTube (comment_id) and
# TikTok (unique_comment_identifier or row index). So we get one row per "comment"
# and long format has exactly 2 rows per row_id.
prevalence_long <- joined_data %>%
  select(row_id, video_id, harmoniousness_raw, divisiveness_raw) %>%
  pivot_longer(
    cols = c(harmoniousness_raw, divisiveness_raw),
    names_to = "discourse_type",
    values_to = "score"
  ) %>%
  mutate(
    discourse_type = recode(discourse_type,
                           "harmoniousness_raw" = "Constructiveness",
                           "divisiveness_raw" = "Destructiveness"),
    discourse_type = factor(discourse_type, levels = c("Destructiveness", "Constructiveness"))
  )

cat("=== DATA PREPARATION ===\n")
cat("Wide (joined_data) rows:", nrow(joined_data), "\n")
cat("Unique row_id (one per comment; YouTube=comment_id, TikTok=unique_comment_identifier or row index):", n_distinct(joined_data$row_id), "\n")
cat("Long format rows (2 per wide row):", nrow(prevalence_long), "\n")
cat("Unique video_id:", n_distinct(prevalence_long$video_id), "\n\n")

# Model: MLM with random effects for comment and video
# DV = score (0–1). Fixed effect = discourse_type (Constructiveness vs Destructiveness).
# Coefficient = mean difference (C − D) on identity scale; do not exponentiate.
cat("=== MULTILEVEL MODEL ===\n")
cat("Predicting discourse score from discourse type (within-comment C vs D)\n")
cat("Random effects: comment_id + video_id\n\n")

mod1 <- lmer(
  score ~ discourse_type + (1|row_id) + (1|video_id),
  data = prevalence_long,
  REML = FALSE
)

summary(mod1)
confint(mod1, method = "Wald")

# Extract key results
coefs <- fixef(mod1)
ses <- sqrt(diag(vcov(mod1)))
t_vals <- coefs / ses
ci <- confint(mod1, parm = "beta_", method = "Wald")

cat("\n=== KEY RESULTS ===\n")
cat("Constructiveness exceeds Destructiveness by (mean difference on 0-1 scale; identity link, no exponentiation):\n")
cat(sprintf("  B = %.3f (SE = %.3f)\n", coefs[2], ses[2]))
cat(sprintf("  t = %.2f, p < .001\n", t_vals[2]))
cat(sprintf("  95%% CI: [%.3f, %.3f]\n", ci[2,1], ci[2,2]))

# Calculate effect size
constructiveness_scores <- prevalence_long %>% 
  filter(discourse_type == "Constructiveness") %>% 
  pull(score)
destructiveness_scores <- prevalence_long %>% 
  filter(discourse_type == "Destructiveness") %>% 
  pull(score)
cohens_d <- (mean(constructiveness_scores, na.rm = TRUE) - mean(destructiveness_scores, na.rm = TRUE)) / 
  sd(c(constructiveness_scores, destructiveness_scores), na.rm = TRUE)

cat(sprintf("  Cohen's d = %.2f\n", cohens_d))

# Save results
results <- list(
  model = mod1,
  coefficient = coefs[2],
  se = ses[2],
  t_value = t_vals[2],
  ci = ci[2,],
  cohens_d = cohens_d
)

saveRDS(results, "analysis/prevalence/RQ1_MLM_general_prevalence_results.rds")

cat("\n=== RESULTS SAVED ===\n")
