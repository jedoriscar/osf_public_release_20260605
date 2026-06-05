# Purpose
# Test whether constructiveness interacts with stance to predict replies.
# This is part of the main interaction analysis (also in Section 4.5).
#
# Reference: Main text line 160
# Model: Negative binomial MLM with stance × constructiveness interaction

# Setup
rm(list = ls())
library(tidyverse)   # dplyr (%, inner_join, filter)
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

# Load stance classification data (minimal columns to avoid .x/.y with joined_data)
stance_file <- "data/model_labels/racial_stance_labels.csv"
if (!file.exists(stance_file)) {
  stop("Stance classification file not found. Expected at: ", stance_file)
}

stance_data <- read.csv(stance_file)
stance_lookup <- stance_data %>%
  select(comment_id, stance_label, any_of("topic_0_prob"))

# Merge: use canonical joined_data + stance_label only
analysis_data <- joined_data %>%
  inner_join(stance_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(stance_label))

# Filter to Topic 0 with high probability (≥0.6) if column exists
if ("topic_0_prob" %in% colnames(analysis_data)) {
  analysis_data <- analysis_data %>%
    filter(topic_0_prob >= 0.6)
}

# Filter to YouTube only. Use comment-level replies (not video-level reply_count).
# Binary coding: stance_binary 0 = Pro-Diversity (reference), 1 = Anti-Diversity.
analysis_data <- analysis_data %>%
  filter(platform == "YouTube" | is.na(platform)) %>%
  mutate(
    comment_replies = as.numeric(replies),
    stance_binary = ifelse(stance_label == "Anti-Diversity", 1, 0)
  ) %>%
  filter(!is.na(comment_replies), stance_label %in% c("Pro-Diversity", "Anti-Diversity"))

# Model: Replies with Interaction
cat("=== MODEL: REPLIES WITH STANCE × CONSTRUCTIVENESS INTERACTION ===\n\n")

mod1 <- glmer.nb(
  comment_replies ~ stance_binary + harmoniousness_raw + 
    stance_binary:harmoniousness_raw + (1|video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

summary(mod1)
confint(mod1, method = "Wald")

# Extract and interpret results
# Engagement is COMMENT-level (comment_replies = replies per comment).
# Stance is COMMENT-level (one stance_label per comment_id).
coefs <- fixef(mod1)
ses <- sqrt(diag(vcov(mod1)))
z_vals <- coefs / ses
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
irr_vals <- exp(coefs)
ci_vals <- exp(confint(mod1, method = "Wald"))
int_name <- names(coefs)[length(coefs)]
ci_int <- ci_vals[rownames(ci_vals) == int_name, ]

cat("\n=== KEY RESULTS ===\n")
cat("Stance × Constructiveness Interaction:\n")
cat(sprintf("  B = %.3f (SE = %.3f), z = %.2f, p = %.3f\n",
            coefs[4], ses[4], z_vals[4], p_vals[4]))
cat(sprintf("  IRR = %.2f, 95%% CI [%.2f, %.2f]\n",
            irr_vals[4], ci_int[1], ci_int[2]))

if (p_vals[4] < 0.05) {
  cat("  Interpretation: Positive interaction (IRR > 1): the effect of constructiveness\n")
  cat("  on reply count is stronger for Anti-Diversity (1) than for Pro-Diversity (0).\n")
  cat("  So: constructiveness boosts replies more for anti-diversity comments.\n")
} else {
  cat("  Interpretation: No significant interaction; constructiveness benefit similar across stance.\n")
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

saveRDS(results, "analysis/stance_robustness/ROB2_interaction_replies_results.rds")

cat("\n=== RESULTS SAVED ===\n")
