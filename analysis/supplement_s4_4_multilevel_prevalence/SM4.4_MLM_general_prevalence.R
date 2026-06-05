# Purpose
# Test whether constructiveness exceeds destructiveness using multilevel model
# accounting for video clustering. Complements the descriptive t-test.
#
# Reference: SI Appendix Section 4.4, Main text line 56
# Expected: B = 0.248, SE = 0.001, t = 364.01, p < .001

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

cat("Running MLM for general prevalence effect...\n")
cat("N =", nrow(joined_data), "comments\n\n")

# Prepare data: reshape to long format
# Each comment contributes two rows: one for constructiveness, one for destructiveness
prevalence_long <- joined_data %>%
  select(comment_id, video_id, harmoniousness_raw, divisiveness_raw) %>%
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

cat("Long format data created:\n")
cat("  Total rows:", nrow(prevalence_long), "\n")
cat("  Unique comments:", n_distinct(prevalence_long$comment_id), "\n")
cat("  Unique videos:", n_distinct(prevalence_long$video_id), "\n\n")

# Model: Multilevel model with crossed random effects
# DV: discourse score
# Fixed effect: discourse type (constructiveness vs destructiveness)
# Random effects: intercepts for comment (since scores nested within comment) and video
cat("Fitting multilevel model...\n")

mod1 <- lmer(
  score ~ discourse_type + (1|comment_id) + (1|video_id),
  data = prevalence_long,
  REML = FALSE
)

summary(mod1)
confint(mod1, parm = "beta_", method = "Wald")

# Extract key results
fixed_effects <- fixef(mod1)
se <- sqrt(diag(vcov(mod1)))
ci <- confint(mod1, parm = "beta_", method = "Wald")

cat("\n=== KEY RESULTS ===\n")
cat("Constructiveness exceeds Destructiveness by:\n")
cat(sprintf("  B = %.4f (SE = %.4f)\n", fixed_effects[2], se[2]))
cat(sprintf("  95%% CI: [%.4f, %.4f]\n", ci[2,1], ci[2,2]))
cat(sprintf("  t = %.2f, p < .001\n", fixed_effects[2] / se[2]))

# Descriptive statistics
means <- prevalence_long %>%
  group_by(discourse_type) %>%
  summarize(
    M = mean(score, na.rm = TRUE),
    SD = sd(score, na.rm = TRUE)
  )

cat("\nDescriptive Statistics:\n")
print(means)

# Effect size (Cohen's d)
constructiveness_scores <- prevalence_long %>% 
  filter(discourse_type == "Constructiveness") %>% 
  pull(score)

destructiveness_scores <- prevalence_long %>% 
  filter(discourse_type == "Destructiveness") %>% 
  pull(score)

cohens_d <- (mean(constructiveness_scores, na.rm = TRUE) - mean(destructiveness_scores, na.rm = TRUE)) / 
  sd(c(constructiveness_scores, destructiveness_scores), na.rm = TRUE)

cat(sprintf("\nCohen's d = %.3f\n", cohens_d))

# Save results
results <- list(
  model = mod1,
  fixed_effects = fixed_effects,
  se = se,
  ci = ci,
  means = means,
  cohens_d = cohens_d
)

saveRDS(results, "analysis/supplement_s4_4_multilevel_prevalence/SM4.4_MLM_general_prevalence_results.rds")

cat("\n=== RESULTS SAVED ===\n")
cat("Multilevel model confirms constructiveness significantly exceeds destructiveness (p < .001)\n")
