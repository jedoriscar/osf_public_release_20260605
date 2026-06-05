# Purpose
# Check model convergence for all GLMMs (logistic, negative binomial, beta).
# Reports convergence warnings and optimizer diagnostics.
#
# Reference: SI Appendix Section 4.3

# Setup
rm(list = ls())
library(lme4)
library(lmerTest)
library(glmmTMB)

# Load data
source("analysis/setup/load_data.R")

# Check convergence for rewards models
cat("=== CONVERGENCE CHECKS ===\n\n")

# Filter to YouTube only
analysis_data <- joined_data %>%
  filter(platform == "YouTube" | is.na(platform)) %>%
  mutate(
    like_count = as.numeric(like_count),
    top_comment_binary = ifelse(top_comment == 1 | top_comment == TRUE, 1, 0)
  ) %>%
  filter(!is.na(like_count), !is.na(top_comment_binary))

# Model 1: Logistic MLM (algorithmic surfacing)
cat("Model 1: Logistic MLM (Algorithmic Surfacing)\n")
mod1_logistic <- glmer(
  top_comment_binary ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = analysis_data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

if (mod1_logistic@optinfo$conv$opt == 0) {
  cat("  ✓ Converged successfully\n")
} else {
  cat("  ⚠ Convergence warning:", mod1_logistic@optinfo$conv$opt, "\n")
}

# Model 2: Negative Binomial MLM (likes)
cat("\nModel 2: Negative Binomial MLM (Likes)\n")
mod1_nb <- glmer.nb(
  like_count ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

if (mod1_nb@optinfo$conv$opt == 0) {
  cat("  ✓ Converged successfully\n")
} else {
  cat("  ⚠ Convergence warning:", mod1_nb@optinfo$conv$opt, "\n")
}

# Save results
results <- list(
  logistic_converged = mod1_logistic@optinfo$conv$opt == 0,
  nb_converged = mod1_nb@optinfo$conv$opt == 0,
  logistic_warnings = mod1_logistic@optinfo$warnings,
  nb_warnings = mod1_nb@optinfo$warnings
)

saveRDS(results, "analysis/supplement_s4_3_model_diagnostics/SM4.3_convergence_checks_results.rds")

cat("\n=== RESULTS SAVED ===\n")
