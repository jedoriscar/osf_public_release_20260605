# Purpose
# Calculate model fit statistics (AIC, BIC, R²) for all main models.
# Compares model fit across specifications.
#
# Reference: SI Appendix Section 4.3

# Setup
rm(list = ls())
library(lme4)
library(lmerTest)
library(MuMIn)

# Load data
source("analysis/setup/load_data.R")

# Calculate fit statistics for prevalence MLM
cat("=== MODEL FIT STATISTICS ===\n\n")

# Prepare data
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

# Fit model
mod1 <- lmer(
  score ~ discourse_type + (1|comment_id) + (1|video_id),
  data = prevalence_long,
  REML = FALSE
)

# Calculate fit statistics
AIC_val <- AIC(mod1)
BIC_val <- BIC(mod1)
r_squared <- r.squaredGLMM(mod1)

cat("Prevalence MLM fit statistics:\n")
cat(sprintf("  AIC: %.1f\n", AIC_val))
cat(sprintf("  BIC: %.1f\n", BIC_val))
cat(sprintf("  R²m (marginal): %.4f\n", r_squared[1]))
cat(sprintf("  R²c (conditional): %.4f\n", r_squared[2]))

# Save results
results <- list(
  model = mod1,
  AIC = AIC_val,
  BIC = BIC_val,
  R_squared = r_squared
)

saveRDS(results, "analysis/supplement_s4_3_model_diagnostics/SM4.3_model_fit_statistics_results.rds")

cat("\n=== RESULTS SAVED ===\n")
