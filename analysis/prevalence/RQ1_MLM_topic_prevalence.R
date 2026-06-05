# Purpose
# Test topic-level prevalence using multilevel model with crossed random effects
# for topic and video. Complements the paired t-test with proper nesting structure.
#
# Reference: Main text line 72, SI Appendix Section 4.4
# Expected: B = 0.21, SE = 0.004, t = 50.52, p < .001

# Setup
rm(list = ls())
library(lme4)
library(lmerTest)
library(tidyverse)

# Load data
# Use relative path from public release folder root
source("analysis/setup/load_data.R")

# Prepare topic-level data
# This requires topic probability-weighted scores aggregated to topic-video level
# Adjust based on your actual topic data structure

cat("=== PREPARING TOPIC-LEVEL DATA ===\n")

# Topic probability columns: topic_<number>_<description> (exclude _z columns)
topic_cols <- grep("^topic_-?\\d+_", colnames(joined_data), value = TRUE)
topic_cols <- topic_cols[!grepl("_z$", topic_cols)]
if (length(topic_cols) == 0) {
  stop("Topic probability columns not found. Expected: topic_0_..., topic_1_..., etc. (exclude _z).")
}

# Reshape to long format: each row is a topic-video combination
topic_long <- joined_data %>%
  select(row_id, video_id, harmoniousness_raw, divisiveness_raw, all_of(topic_cols)) %>%
  pivot_longer(
    cols = all_of(topic_cols),
    names_to = "topic_label",
    values_to = "topic_prob"
  ) %>%
  mutate(
    topic_label = gsub("^topic_", "", topic_label)
  ) %>%
  filter(topic_prob >= 0.25) %>%  # Threshold for topic inclusion
  mutate(
    weighted_C = harmoniousness_raw * topic_prob,
    weighted_D = divisiveness_raw * topic_prob
  )

# Aggregate to topic-video level
topic_video_data <- topic_long %>%
  group_by(topic_label, video_id) %>%
  summarize(
    weighted_mean_C = sum(weighted_C, na.rm = TRUE) / sum(topic_prob, na.rm = TRUE),
    weighted_mean_D = sum(weighted_D, na.rm = TRUE) / sum(topic_prob, na.rm = TRUE),
    n_comments = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(weighted_mean_C, weighted_mean_D),
    names_to = "discourse_type",
    values_to = "score"
  ) %>%
  mutate(
    discourse_type = recode(discourse_type,
                           "weighted_mean_C" = "Constructiveness",
                           "weighted_mean_D" = "Destructiveness"),
    discourse_type = factor(discourse_type, levels = c("Destructiveness", "Constructiveness"))
  )

cat("Topic-video combinations: N =", nrow(topic_video_data), "\n")
cat("Unique topics:", n_distinct(topic_video_data$topic_label), "\n")
cat("Unique videos:", n_distinct(topic_video_data$video_id), "\n\n")

# Model: Multilevel model with crossed random effects
cat("=== MULTILEVEL MODEL ===\n")
cat("Predicting topic-level score from discourse type\n")
cat("Random effects: topic_label (crossed) + video_id\n\n")

mod1 <- lmer(
  score ~ discourse_type + (1|topic_label) + (1|video_id),
  data = topic_video_data,
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
cat("Constructiveness exceeds Destructiveness across topics by:\n")
cat(sprintf("  B = %.3f (SE = %.3f)\n", coefs[2], ses[2]))
cat(sprintf("  t = %.2f, p < .001\n", t_vals[2]))
cat(sprintf("  95%% CI: [%.3f, %.3f]\n", ci[2,1], ci[2,2]))

# Save results
results <- list(
  model = mod1,
  coefficient = coefs[2],
  se = ses[2],
  t_value = t_vals[2],
  ci = ci[2,],
  n_topic_video = nrow(topic_video_data)
)

saveRDS(results, "analysis/prevalence/RQ1_MLM_topic_prevalence_results.rds")

cat("\n=== RESULTS SAVED ===\n")
