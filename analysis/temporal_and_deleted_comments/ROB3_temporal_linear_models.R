# Purpose
# Test temporal trends in constructiveness and destructiveness over time.
# Tests whether constructiveness has declined as bias and polarization increased.
#
# Reference: Main text lines 166-167
# Model: Linear mixed models with year as predictor

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)
library(lubridate)

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
  filter(!is.na(year), !is.na(harmoniousness_raw), !is.na(divisiveness_raw))

# Quick sanity checks
cat("=== DATA CHECKS ===\n")
cat("N comments:", nrow(analysis_data), "\n")
cat("Year range:", min(analysis_data$year, na.rm = TRUE), "to", max(analysis_data$year, na.rm = TRUE), "\n\n")

# Model 1: Constructiveness over time
cat("=== MODEL 1: CONSTRUCTIVENESS OVER TIME ===\n\n")

mod1_C <- lmer(
  harmoniousness_raw ~ year + (1|video_id),
  data = analysis_data
)

summary(mod1_C)
confint(mod1_C)

coefs_C <- fixef(mod1_C)
ses_C <- sqrt(diag(vcov(mod1_C)))
t_vals_C <- coefs_C / ses_C

cat("\nConstructiveness temporal trend:\n")
cat(sprintf("  B = %.4f (SE = %.4f), t = %.2f, p = %.3f\n", 
            coefs_C[2], ses_C[2], t_vals_C[2], 
            2 * (1 - pnorm(abs(t_vals_C[2])))))

# Model 2: Destructiveness over time
cat("\n=== MODEL 2: DESTRUCTIVENESS OVER TIME ===\n\n")

mod1_D <- lmer(
  divisiveness_raw ~ year + (1|video_id),
  data = analysis_data
)

summary(mod1_D)
confint(mod1_D)

coefs_D <- fixef(mod1_D)
ses_D <- sqrt(diag(vcov(mod1_D)))
t_vals_D <- coefs_D / ses_D

cat("\nDestructiveness temporal trend:\n")
cat(sprintf("  B = %.4f (SE = %.4f), t = %.2f, p = %.3f\n", 
            coefs_D[2], ses_D[2], t_vals_D[2], 
            2 * (1 - pnorm(abs(t_vals_D[2])))))

# Save results
results <- list(
  constructiveness_model = mod1_C,
  destructiveness_model = mod1_D,
  C_coef = coefs_C[2],
  C_se = ses_C[2],
  C_t = t_vals_C[2],
  D_coef = coefs_D[2],
  D_se = ses_D[2],
  D_t = t_vals_D[2],
  n = nrow(analysis_data)
)

saveRDS(results, "analysis/temporal_and_deleted_comments/ROB3_temporal_linear_models_results.rds")

cat("\n=== RESULTS SAVED ===\n")
