# Purpose
# Document Model 3 specification: Algorithmic surfacing (logistic MLM).
# Provides complete model specification for rewards analyses.
#
# Reference: SI Appendix Section 4.1, Main text lines 84-85
# Model: Logistic multilevel model

# Setup
rm(list = ls())
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

# Filter to YouTube only
analysis_data <- joined_data %>%
  filter(platform == "YouTube" | is.na(platform)) %>%
  mutate(
    top_comment_binary = ifelse(top_comment == 1 | top_comment == TRUE, 1, 0)
  ) %>%
  filter(!is.na(top_comment_binary))

# Model specification
cat("=== MODEL 3: ALGORITHMIC SURFACING (LOGISTIC MLM) ===\n\n")

cat("Model Type: Generalized Linear Mixed Model (GLMM)\n")
cat("Family: Binomial (logit link)\n")
cat("Optimizer: bobyqa (max iterations = 100,000)\n\n")

cat("Formula:\n")
cat("  top_comment_binary ~ harmoniousness_raw + divisiveness_raw + (1|video_id)\n\n")

cat("Fixed Effects:\n")
cat("  - harmoniousness_raw: Constructiveness index (continuous, 0-1)\n")
cat("  - divisiveness_raw: Destructiveness index (continuous, 0-1)\n\n")

cat("Random Effects:\n")
cat("  - (1|video_id): Random intercept for video\n\n")

cat("N: ", nrow(analysis_data), " comments\n")
cat("N videos: ", length(unique(analysis_data$video_id)), "\n\n")

# Run model
mod1 <- glmer(
  top_comment_binary ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = analysis_data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

summary(mod1)
confint(mod1, method = "Wald")

# Extract results
coefs <- fixef(mod1)
ses <- sqrt(diag(vcov(mod1)))
or_vals <- exp(coefs)
ci_vals <- exp(confint(mod1, method = "Wald"))

cat("\n=== KEY RESULTS ===\n")
cat("Constructiveness:\n")
cat(sprintf("  Log Odds = %.3f (SE = %.3f)\n", coefs[2], ses[2]))
cat(sprintf("  OR = %.2f, 95%% CI [%.2f, %.2f]\n", or_vals[2], ci_vals[2,1], ci_vals[2,2]))

cat("\nDestructiveness:\n")
cat(sprintf("  Log Odds = %.3f (SE = %.3f)\n", coefs[3], ses[3]))
cat(sprintf("  OR = %.2f, 95%% CI [%.2f, %.2f]\n", or_vals[3], ci_vals[3,1], ci_vals[3,2]))

# Save results
results <- list(
  model = mod1,
  model_type = "Logistic Multilevel Model",
  formula = "top_comment_binary ~ harmoniousness_raw + divisiveness_raw + (1|video_id)",
  constructiveness = list(log_odds = coefs[2], se = ses[2], or = or_vals[2], ci = ci_vals[2,]),
  destructiveness = list(log_odds = coefs[3], se = ses[3], or = or_vals[3], ci = ci_vals[3,]),
  n = nrow(analysis_data)
)

saveRDS(results, "analysis/supplement_s4_1_model_specs/SM4.1_model_3_rewards_algorithmic_spec_results.rds")

cat("\n=== RESULTS SAVED ===\n")
