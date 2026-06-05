# Purpose
# Document Model 4 specification: Likes (negative binomial MLM).
# Provides complete model specification for engagement analyses.
#
# Reference: SI Appendix Section 4.1, Main text lines 85-86
# Model: Negative binomial multilevel model

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
    like_count = as.numeric(like_count)
  ) %>%
  filter(!is.na(like_count))

# Model specification
cat("=== MODEL 4: LIKES (NEGATIVE BINOMIAL MLM) ===\n\n")

cat("Model Type: Generalized Linear Mixed Model (GLMM)\n")
cat("Family: Negative Binomial (log link)\n")
cat("Optimizer: bobyqa (max iterations = 100,000)\n\n")

cat("Formula:\n")
cat("  like_count ~ harmoniousness_raw + divisiveness_raw + (1|video_id)\n\n")

cat("Fixed Effects:\n")
cat("  - harmoniousness_raw: Constructiveness index (continuous, 0-1)\n")
cat("  - divisiveness_raw: Destructiveness index (continuous, 0-1)\n\n")

cat("Random Effects:\n")
cat("  - (1|video_id): Random intercept for video\n\n")

cat("N: ", nrow(analysis_data), " comments\n")
cat("N videos: ", length(unique(analysis_data$video_id)), "\n\n")

# Run model
mod1 <- glmer.nb(
  like_count ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

summary(mod1)
confint(mod1, method = "Wald")

# Extract results
coefs <- fixef(mod1)
ses <- sqrt(diag(vcov(mod1)))
irr_vals <- exp(coefs)
ci_vals <- exp(confint(mod1, method = "Wald"))

cat("\n=== KEY RESULTS ===\n")
cat("Constructiveness:\n")
cat(sprintf("  B = %.3f (SE = %.3f)\n", coefs[2], ses[2]))
cat(sprintf("  IRR = %.2f, 95%% CI [%.2f, %.2f]\n", irr_vals[2], ci_vals[2,1], ci_vals[2,2]))

cat("\nDestructiveness:\n")
cat(sprintf("  B = %.3f (SE = %.3f)\n", coefs[3], ses[3]))
cat(sprintf("  IRR = %.2f, 95%% CI [%.2f, %.2f]\n", irr_vals[3], ci_vals[3,1], ci_vals[3,2]))

# Save results
results <- list(
  model = mod1,
  model_type = "Negative Binomial Multilevel Model",
  formula = "like_count ~ harmoniousness_raw + divisiveness_raw + (1|video_id)",
  constructiveness = list(b = coefs[2], se = ses[2], irr = irr_vals[2], ci = ci_vals[2,]),
  destructiveness = list(b = coefs[3], se = ses[3], irr = irr_vals[3], ci = ci_vals[3,]),
  n = nrow(analysis_data)
)

saveRDS(results, "analysis/supplement_s4_1_model_specs/SM4.1_model_4_rewards_likes_spec_results.rds")

cat("\n=== RESULTS SAVED ===\n")
