# Purpose
# Extract variance components from multilevel models.
# Reports ICCs and variance explained by random effects.
#
# Reference: SI Appendix Section 4.3

# Setup
rm(list = ls())
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

# Calculate variance components for prevalence MLM
cat("=== VARIANCE COMPONENTS ===\n\n")

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

# Extract variance components
var_components <- VarCorr(mod1)
var_comment <- as.data.frame(var_components)$vcov[1]
var_video <- as.data.frame(var_components)$vcov[2]
var_residual <- as.data.frame(var_components)$vcov[3]

total_var <- var_comment + var_video + var_residual

icc_comment <- var_comment / total_var
icc_video <- var_video / total_var

cat("Variance components:\n")
cat(sprintf("  Comment-level variance: %.6f (ICC = %.3f)\n", var_comment, icc_comment))
cat(sprintf("  Video-level variance: %.6f (ICC = %.3f)\n", var_video, icc_video))
cat(sprintf("  Residual variance: %.6f\n", var_residual))
cat(sprintf("  Total variance: %.6f\n", total_var))

# Save results
results <- list(
  model = mod1,
  var_comment = var_comment,
  var_video = var_video,
  var_residual = var_residual,
  icc_comment = icc_comment,
  icc_video = icc_video
)

saveRDS(results, "analysis/supplement_s4_3_model_diagnostics/SM4.3_variance_components_results.rds")

cat("\n=== RESULTS SAVED ===\n")
