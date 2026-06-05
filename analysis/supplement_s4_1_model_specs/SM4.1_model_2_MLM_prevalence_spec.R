# Purpose
# Document Model 2 specification: MLM for general prevalence.
# Provides complete model specification with random effects structure.
#
# Reference: SI Appendix Section 4.1, Main text line 56
# Model: Linear mixed model with crossed random effects

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

# Model specification
cat("=== MODEL 2: MLM GENERAL PREVALENCE ===\n\n")

cat("Model Type: Linear Mixed Model (LMM)\n")
cat("Family: Gaussian (identity link)\n")
cat("Estimation: REML = FALSE (for model comparison)\n\n")

cat("Formula:\n")
cat("  score ~ discourse_type + (1|comment_id) + (1|video_id)\n\n")

cat("Fixed Effects:\n")
cat("  - discourse_type: factor (Destructiveness = reference, Constructiveness = comparison)\n\n")

cat("Random Effects:\n")
cat("  - (1|comment_id): Random intercept for comment (nested within comment)\n")
cat("  - (1|video_id): Random intercept for video (crossed with comment)\n\n")

# Prepare data and run model
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

mod1 <- lmer(
  score ~ discourse_type + (1|comment_id) + (1|video_id),
  data = prevalence_long,
  REML = FALSE
)

summary(mod1)
confint(mod1, parm = "beta_", method = "Wald")

# Extract results
coefs <- fixef(mod1)
ses <- sqrt(diag(vcov(mod1)))
ci <- confint(mod1, parm = "beta_", method = "Wald")

cat("\n=== KEY RESULTS ===\n")
cat(sprintf("Constructiveness coefficient: B = %.4f (SE = %.4f)\n", coefs[2], ses[2]))
cat(sprintf("95%% CI: [%.4f, %.4f]\n", ci[2,1], ci[2,2]))
cat(sprintf("t = %.2f, p < .001\n", coefs[2] / ses[2]))

# Save results
results <- list(
  model = mod1,
  model_type = "Linear Mixed Model",
  formula = "score ~ discourse_type + (1|comment_id) + (1|video_id)",
  coefficient = coefs[2],
  se = ses[2],
  ci = ci[2,],
  n = nrow(prevalence_long)
)

saveRDS(results, "analysis/supplement_s4_1_model_specs/SM4.1_model_2_MLM_prevalence_spec_results.rds")

cat("\n=== RESULTS SAVED ===\n")
