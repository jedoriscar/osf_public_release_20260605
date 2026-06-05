# Purpose
# Test whether destructiveness interacts with stance to predict likes.
# Model: Negative binomial MLM with stance × destructiveness interaction
#
# Reference: Complement to main text constructiveness × stance analysis

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

# Load stance classification data
stance_file <- "data/model_labels/racial_stance_labels.csv"
if (!file.exists(stance_file)) {
  stop("Stance classification file not found. Expected at: ", stance_file)
}

stance_data <- read.csv(stance_file) %>%
  select(comment_id, stance_label) %>%
  filter(stance_label %in% c("Pro-Diversity", "Anti-Diversity"), !is.na(stance_label))

# Merge with main data
analysis_data <- joined_data %>%
  inner_join(stance_data, by = "comment_id")

# Filter to Topic 0 with high probability (≥0.6)
if ("topic_0_diversity_and_immigration_in_north_america" %in% colnames(analysis_data)) {
  analysis_data <- analysis_data %>%
    filter(topic_0_diversity_and_immigration_in_north_america >= 0.6)
} else if ("topic_0_prob" %in% colnames(analysis_data)) {
  analysis_data <- analysis_data %>%
    filter(topic_0_prob >= 0.6)
}

# Filter to YouTube only. Use COMMENT-level engagement: likes (not video-level like_count).
analysis_data <- analysis_data %>%
  filter(platform == "YouTube" | platform_source.y == "YouTube" | is.na(platform)) %>%
  mutate(
    comment_likes = as.numeric(likes),
    stance_binary = ifelse(stance_label == "Anti-Diversity", 1, 0)
  ) %>%
  filter(!is.na(comment_likes), !is.na(divisiveness_raw))

# Model: Likes with Stance × Destructiveness Interaction
cat("=== MODEL: LIKES WITH STANCE × DESTRUCTIVENESS INTERACTION ===\n\n")

mod1 <- glmer.nb(
  comment_likes ~ stance_binary + divisiveness_raw + 
    stance_binary:divisiveness_raw + (1|video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

summary(mod1)
confint(mod1, method = "Wald")

# Extract and interpret results
# Engagement is COMMENT-level (comment_likes = likes per comment). Stance is COMMENT-level.
coefs <- fixef(mod1)
ses <- sqrt(diag(vcov(mod1)))
z_vals <- coefs / ses
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
irr_vals <- exp(coefs)
ci_vals <- exp(confint(mod1, method = "Wald"))
int_name <- names(coefs)[length(coefs)]
ci_int <- ci_vals[rownames(ci_vals) == int_name, ]

cat("\n=== KEY RESULTS ===\n")
cat("Stance × Destructiveness Interaction:\n")
cat(sprintf("  B = %.3f (SE = %.3f), z = %.2f, p = %.3f\n",
            coefs[4], ses[4], z_vals[4], p_vals[4]))
cat(sprintf("  IRR = %.2f, 95%% CI [%.2f, %.2f]\n",
            irr_vals[4], ci_int[1], ci_int[2]))

if (p_vals[4] < 0.05) {
  cat("  Interpretation: Destructiveness penalty differs by stance.\n")
} else {
  cat("  Interpretation: Destructiveness penalty does not differ by stance.\n")
}

# Save results
results <- list(
  model = mod1,
  interaction_coef = coefs[4],
  interaction_se = ses[4],
  interaction_z = z_vals[4],
  interaction_p = p_vals[4],
  interaction_irr = irr_vals[4],
  interaction_ci = ci_int,
  n = nrow(analysis_data)
)

saveRDS(results, "analysis/stance_robustness/ROB2_interaction_likes_destructiveness_results.rds")

cat("\n=== RESULTS SAVED ===\n")
