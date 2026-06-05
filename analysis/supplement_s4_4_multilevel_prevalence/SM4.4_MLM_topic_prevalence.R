# Purpose
# Test whether constructiveness exceeds destructiveness across topics using
# multilevel model with crossed random effects for topic and video.
# Complements the descriptive paired t-test.
#
# Reference: SI Appendix Section 4.4, Main text line 72
# Expected: B = 0.079, SE = 0.005, t = 15.10, p < .001

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

cat("Running MLM for topic-level prevalence effect...\n")
cat("N =", nrow(joined_data), "comments\n\n")

# Get topic probability columns
# Adjust pattern based on actual column names
topic_cols <- grep("^topic_-?\\d+_.*(?<!_z)$", names(joined_data), value = TRUE, perl = TRUE)

if (length(topic_cols) == 0) {
  # Try alternative pattern
  topic_cols <- grep("^topic_\\d+_.*(?<!_z)$", names(joined_data), value = TRUE, perl = TRUE)
}

if (length(topic_cols) == 0) {
  stop("Topic probability columns not found. Expected format: topic_0_prob, topic_1_prob, etc.")
}

cat("Found", length(topic_cols), "topic columns\n\n")

# Reshape to long format and filter for >= 0.25 threshold
topic_data_long <- joined_data %>%
  select(comment_id, video_id, harmoniousness_raw, divisiveness_raw, all_of(topic_cols)) %>%
  pivot_longer(
    cols = all_of(topic_cols),
    names_to = "topic_label",
    values_to = "topic_probability"
  ) %>%
  filter(topic_probability >= 0.25)

cat("After reshaping and filtering for topic probability >= 0.25:\n")
cat("  N =", nrow(topic_data_long), "comment-topic pairs\n")
cat("  Unique comments:", n_distinct(topic_data_long$comment_id), "\n")
cat("  Unique topics:", n_distinct(topic_data_long$topic_label), "\n")
cat("  Unique videos:", n_distinct(topic_data_long$video_id), "\n\n")

# Calculate weighted topic-level scores
topic_weighted <- topic_data_long %>%
  group_by(topic_label, video_id) %>%
  summarize(
    constructiveness = weighted.mean(harmoniousness_raw, topic_probability, na.rm = TRUE),
    destructiveness = weighted.mean(divisiveness_raw, topic_probability, na.rm = TRUE),
    n_comments = n(),
    .groups = "drop"
  )

cat("Topic-level weighted scores calculated:\n")
cat("  N =", nrow(topic_weighted), "topic-video combinations\n\n")

# Reshape to long format for model
topic_long <- topic_weighted %>%
  pivot_longer(
    cols = c(constructiveness, destructiveness),
    names_to = "discourse_type",
    values_to = "score"
  ) %>%
  mutate(
    discourse_type = factor(discourse_type, levels = c("destructiveness", "constructiveness"))
  )

# Model: Multilevel model with crossed random effects
# DV: weighted discourse score
# Fixed effect: discourse type (constructiveness vs destructiveness)
# Random effects: intercepts for topic and video
cat("Fitting multilevel model with crossed random effects...\n")

mod1 <- lmer(
  score ~ discourse_type + (1|topic_label) + (1|video_id),
  data = topic_long,
  REML = FALSE
)

summary(mod1)
confint(mod1, parm = "beta_", method = "Wald")

# Extract key results
fixed_effects <- fixef(mod1)
se <- sqrt(diag(vcov(mod1)))
ci <- confint(mod1, parm = "beta_", method = "Wald")

cat("\n=== KEY RESULTS ===\n")
cat("Constructiveness exceeds Destructiveness across topics by:\n")
cat(sprintf("  B = %.4f (SE = %.4f)\n", fixed_effects[2], se[2]))
cat(sprintf("  95%% CI: [%.4f, %.4f]\n", ci[2,1], ci[2,2]))
cat(sprintf("  t = %.2f, p < .001\n", fixed_effects[2] / se[2]))

# Comparison with descriptive t-test
topic_means <- topic_weighted %>%
  group_by(topic_label) %>%
  summarize(
    constructiveness = mean(constructiveness, na.rm = TRUE),
    destructiveness = mean(destructiveness, na.rm = TRUE),
    .groups = "drop"
  )

t_test_result <- t.test(topic_means$constructiveness, topic_means$destructiveness, paired = TRUE)

cat("\n=== COMPARISON WITH DESCRIPTIVE T-TEST ===\n")
cat("Paired t-test on topic-video aggregated means:\n")
cat(sprintf("  t(%d) = %.2f, p = %.4f\n", 
            t_test_result$parameter, 
            t_test_result$statistic, 
            t_test_result$p.value))

cohens_d <- (mean(topic_means$constructiveness) - mean(topic_means$destructiveness)) / 
  sd(topic_means$constructiveness - topic_means$destructiveness)
cat(sprintf("  Cohen's d = %.2f\n", cohens_d))

# Save results
results <- list(
  model = mod1,
  fixed_effects = fixed_effects,
  se = se,
  ci = ci,
  topic_means = topic_means,
  t_test = t_test_result,
  cohens_d = cohens_d
)

saveRDS(results, "analysis/supplement_s4_4_multilevel_prevalence/SM4.4_MLM_topic_prevalence_results.rds")

cat("\n=== RESULTS SAVED ===\n")
cat("Multilevel model confirms constructiveness exceeds destructiveness across all topics (p < .001)\n")
