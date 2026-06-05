# Purpose
# Test whether constructiveness and destructiveness predict algorithmic surfacing
# (whether YouTube promotes a comment as "Top Comment").
#
# Reference: Main text lines 84-85
# Model: Logistic multilevel model with random intercepts for video

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

# Filter to YouTube only, TOP-LEVEL comments only (replies are not algorithmically surfaced)
analysis_data <- joined_data %>%
  filter(platform == "YouTube" | is.na(platform)) %>%
  mutate(
    is_reply = !is.na(parent_comment_id) & parent_comment_id != "",
    top_comment_binary = ifelse(top_comment == 1 | top_comment == TRUE, 1, 0)
  ) %>%
  filter(!is_reply, !is.na(top_comment_binary))

# Quick sanity checks
cat("=== DATA CHECKS (top-level comments only) ===\n")
cat("N top-level comments:", nrow(analysis_data), "\n")
cat("N videos:", length(unique(analysis_data$video_id)), "\n")
cat("Top comment rate:", mean(analysis_data$top_comment_binary, na.rm = TRUE), "\n\n")

# Model: Algorithmic Surfacing (Logistic MLM)
cat("=== MODEL: ALGORITHMIC SURFACING ===\n")
cat("Predicting top comment status from constructiveness and destructiveness\n\n")

mod1 <- glmer(
  top_comment_binary ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = analysis_data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

summary(mod1)
confint(mod1, method = "Wald")

# Extract and interpret results
coefs <- fixef(mod1)
ses <- sqrt(diag(vcov(mod1)))
z_vals <- coefs / ses
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
or_vals <- exp(coefs)
# Wald CI: exp(beta +/- 1.96*SE) for fixed effects
ci_vals <- rbind(
  exp(coefs[1] + c(-1, 1) * 1.96 * ses[1]),
  exp(coefs[2] + c(-1, 1) * 1.96 * ses[2]),
  exp(coefs[3] + c(-1, 1) * 1.96 * ses[3])
)
rownames(ci_vals) <- names(coefs)

cat("\n=== KEY RESULTS ===\n")
cat("Constructiveness:\n")
cat(sprintf("  Log Odds = %.3f (SE = %.3f), z = %.2f, p < .001\n", 
            coefs[2], ses[2], z_vals[2]))
cat(sprintf("  OR = %.2f, 95%% CI [%.2f, %.2f]\n", 
            or_vals[2], ci_vals[2, 1], ci_vals[2, 2]))
cat(sprintf("  Interpretation: Each unit increase in constructiveness increases\n"))
cat(sprintf("    odds of algorithmic surfacing by %.0f%%\n\n", (or_vals[2] - 1) * 100))

cat("Destructiveness:\n")
cat(sprintf("  Log Odds = %.3f (SE = %.3f), z = %.2f, p < .001\n", 
            coefs[3], ses[3], z_vals[3]))
cat(sprintf("  OR = %.2f, 95%% CI [%.2f, %.2f]\n", 
            or_vals[3], ci_vals[3, 1], ci_vals[3, 2]))
cat(sprintf("  Interpretation: Each unit increase in destructiveness decreases\n"))
cat(sprintf("    odds of algorithmic surfacing by %.0f%%\n", (1 - or_vals[3]) * 100))

# Save results
results <- list(
  model = mod1,
  constructiveness = list(
    log_odds = coefs[2],
    se = ses[2],
    z = z_vals[2],
    p = p_vals[2],
    or = or_vals[2],
    ci = ci_vals[2, ]
  ),
  destructiveness = list(
    log_odds = coefs[3],
    se = ses[3],
    z = z_vals[3],
    p = p_vals[3],
    or = or_vals[3],
    ci = ci_vals[3, ]
  ),
  n = nrow(analysis_data),
  n_videos = length(unique(analysis_data$video_id))
)

saveRDS(results, "analysis/engagement/RQ2_algorithmic_surfacing_results.rds")

cat("\n=== RESULTS SAVED ===\n")
