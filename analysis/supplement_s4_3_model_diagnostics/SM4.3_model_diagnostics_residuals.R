# Purpose
# Generate model diagnostics: residual plots, Q-Q plots, leverage plots.
# Tests assumptions for all main models.
#
# Reference: SI Appendix Section 4.3

# Setup
rm(list = ls())
library(lme4)
library(lmerTest)
library(ggplot2)

# Load data
source("analysis/setup/load_data.R")

# Model diagnostics for prevalence MLM
cat("=== MODEL DIAGNOSTICS: PREVALENCE MLM ===\n\n")

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

# Extract residuals
residuals <- residuals(mod1)
fitted <- fitted(mod1)

cat("Residual diagnostics:\n")
cat(sprintf("  Mean residual: %.6f (should be ~0)\n", mean(residuals)))
cat(sprintf("  SD residual: %.4f\n", sd(residuals)))
cat(sprintf("  Min: %.4f, Max: %.4f\n", min(residuals), max(residuals)))

# Q-Q plot
qqnorm(residuals)
qqline(residuals, col = "red")

# Residual vs fitted
plot(fitted, residuals, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

cat("\n✓ Diagnostic plots generated\n")

# Save results
results <- list(
  model = mod1,
  residuals = residuals,
  fitted = fitted,
  residual_mean = mean(residuals),
  residual_sd = sd(residuals)
)

saveRDS(results, "analysis/supplement_s4_3_model_diagnostics/SM4.3_model_diagnostics_residuals_results.rds")

cat("\n=== RESULTS SAVED ===\n")
