# Purpose
# Calculate influence diagnostics (Cook's distance, leverage, DFBETAS).
# Identifies influential observations that may affect results.
#
# Reference: SI Appendix Section 4.3

# Setup
rm(list = ls())
library(lme4)
library(lmerTest)
library(influence.ME)

# Load data
source("analysis/setup/load_data.R")

# Calculate influence diagnostics for prevalence MLM
cat("=== INFLUENCE DIAGNOSTICS ===\n\n")

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

# Calculate influence at video level
cat("Calculating influence diagnostics at video level...\n")
influence_video <- influence(mod1, group = "video_id")

# Cook's distance
cooks_d <- cooks.distance(influence_video)

cat("\nCook's distance (video level):\n")
cat(sprintf("  Mean: %.6f\n", mean(cooks_d, na.rm = TRUE)))
cat(sprintf("  Max: %.6f\n", max(cooks_d, na.rm = TRUE)))
cat(sprintf("  Videos with Cook's D > 4/n: %d\n", sum(cooks_d > 4/length(cooks_d), na.rm = TRUE)))

# Save results
results <- list(
  model = mod1,
  cooks_distance = cooks_d,
  max_cooks_d = max(cooks_d, na.rm = TRUE),
  n_influential = sum(cooks_d > 4/length(cooks_d), na.rm = TRUE)
)

saveRDS(results, "analysis/supplement_s4_3_model_diagnostics/SM4.3_influence_diagnostics_results.rds")

cat("\n=== RESULTS SAVED ===\n")
