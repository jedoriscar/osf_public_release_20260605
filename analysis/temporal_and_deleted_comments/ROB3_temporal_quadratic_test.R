# Purpose
# Test whether temporal trends are better captured by quadratic (non-linear) models.
# Tests for acceleration or deceleration in trends over time.
#
# Reference: SI Appendix (if quadratic models are reported)

# Setup
rm(list = ls())
library(lme4)
library(lmerTest)
library(lubridate)
library(dplyr)

# Load data
source("analysis/setup/load_data.R")

# Extract year from COMMENT date (when comment was posted), not video upload date
if ("comment_published_at" %in% colnames(joined_data)) {
  joined_data$year <- year(joined_data$comment_published_at)
} else if ("comment_date" %in% colnames(joined_data)) {
  joined_data$year <- year(as.Date(joined_data$comment_date))
} else {
  stop("Comment date column not found. Use comment_published_at for temporal analyses.")
}

analysis_data <- joined_data %>%
  filter(!is.na(year), !is.na(harmoniousness_raw), !is.na(divisiveness_raw)) %>%
  mutate(
    year_centered = year - mean(year, na.rm = TRUE),
    year_squared = year_centered^2
  )

# Model: Constructiveness with quadratic term
cat("=== MODEL: CONSTRUCTIVENESS WITH QUADRATIC TERM ===\n\n")

mod1_C_quad <- lmer(
  harmoniousness_raw ~ year_centered + year_squared + (1|video_id),
  data = analysis_data
)

summary(mod1_C_quad)

# Model: Destructiveness with quadratic term
cat("\n=== MODEL: DESTRUCTIVENESS WITH QUADRATIC TERM ===\n\n")

mod1_D_quad <- lmer(
  divisiveness_raw ~ year_centered + year_squared + (1|video_id),
  data = analysis_data
)

summary(mod1_D_quad)

# Model comparison (linear vs quadratic)
cat("\n=== MODEL COMPARISON ===\n")

# Linear models for comparison
mod1_C_linear <- lmer(
  harmoniousness_raw ~ year_centered + (1|video_id),
  data = analysis_data
)

mod1_D_linear <- lmer(
  divisiveness_raw ~ year_centered + (1|video_id),
  data = analysis_data
)

# Compare AIC
cat("Constructiveness models:\n")
cat(sprintf("  Linear AIC: %.1f\n", AIC(mod1_C_linear)))
cat(sprintf("  Quadratic AIC: %.1f\n", AIC(mod1_C_quad)))

cat("\nDestructiveness models:\n")
cat(sprintf("  Linear AIC: %.1f\n", AIC(mod1_D_linear)))
cat(sprintf("  Quadratic AIC: %.1f\n", AIC(mod1_D_quad)))

# Save results
results <- list(
  C_linear = mod1_C_linear,
  C_quadratic = mod1_C_quad,
  D_linear = mod1_D_linear,
  D_quadratic = mod1_D_quad,
  C_AIC_linear = AIC(mod1_C_linear),
  C_AIC_quad = AIC(mod1_C_quad),
  D_AIC_linear = AIC(mod1_D_linear),
  D_AIC_quad = AIC(mod1_D_quad)
)

saveRDS(results, "analysis/temporal_and_deleted_comments/ROB3_temporal_quadratic_test_results.rds")

cat("\n=== RESULTS SAVED ===\n")
