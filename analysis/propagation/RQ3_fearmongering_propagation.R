# Purpose
# Test whether parent comment constructiveness and destructiveness predict
# child comment fearmongering (language designed to provoke anxiety).
#
# Reference: Main text lines 118-119
# Model: Logistic multilevel model with random intercepts for video

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

# Load parent-child dyad data
dyad_path <- "data/analysis_objects/racial_parent_child_dyads.rda"
if (!file.exists(dyad_path)) {
  stop("Parent-child dyad data not found at: ", dyad_path)
}

load(dyad_path)

# One row per comment_id (avoids many-to-many join and memory blow-up)
comment_lookup <- joined_data %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE)

# Pre-rename to avoid .x/.y suffix if parent_child_data has same col names
parent_lookup <- comment_lookup %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw) %>%
  rename(parent_constructiveness = harmoniousness_raw, parent_destructiveness = divisiveness_raw)

# Merge parent and child features (rename child prob to avoid .x/.y if parent_child_data has same col)
child_fearmongering_lookup <- comment_lookup %>%
  select(comment_id, child_prob_fearmongering = prob_fearmongering)

dyads_analysis <- parent_child_data %>%
  left_join(parent_lookup, by = c("parent_comment_id" = "comment_id"), relationship = "many-to-one") %>%
  left_join(child_fearmongering_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(parent_constructiveness), !is.na(parent_destructiveness))

# Create binary fearmongering indicator (≥0.6 threshold)
dyads_analysis <- dyads_analysis %>%
  mutate(
    child_fearmongering_binary = as.numeric(child_prob_fearmongering >= 0.6)
  ) %>%
  filter(!is.na(child_fearmongering_binary))

# Model: Parent C and D → Child Fearmongering (Logistic MLM)
cat("=== MODEL: PARENT DISCOURSE → CHILD FEARMONGERING ===\n\n")

mod1 <- glmer(
  child_fearmongering_binary ~ parent_constructiveness + parent_destructiveness + (1|video_id),
  data = dyads_analysis,
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
ci_vals <- exp(confint(mod1, method = "Wald"))

cat("\n=== KEY RESULTS ===\n")
cat("Parent Constructiveness → Child Fearmongering:\n")
cat(sprintf("  Log Odds = %.3f (SE = %.3f), z = %.2f, p < .001\n", 
            coefs[2], ses[2], z_vals[2]))
cat(sprintf("  OR = %.2f, 95%% CI [%.2f, %.2f]\n", 
            or_vals[2], ci_vals[2,1], ci_vals[2,2]))

cat("\nParent Destructiveness → Child Fearmongering:\n")
cat(sprintf("  Log Odds = %.3f (SE = %.3f), z = %.2f, p = %.3f\n", 
            coefs[3], ses[3], z_vals[3], p_vals[3]))
cat(sprintf("  OR = %.2f, 95%% CI [%.2f, %.2f]\n", 
            or_vals[3], ci_vals[3,1], ci_vals[3,2]))

# Save results
results <- list(
  model = mod1,
  constructiveness = list(
    log_odds = coefs[2],
    se = ses[2],
    z = z_vals[2],
    p = p_vals[2],
    or = or_vals[2],
    ci = ci_vals[2,]
  ),
  destructiveness = list(
    log_odds = coefs[3],
    se = ses[3],
    z = z_vals[3],
    p = p_vals[3],
    or = or_vals[3],
    ci = ci_vals[3,]
  ),
  n = nrow(dyads_analysis)
)

saveRDS(results, "analysis/propagation/RQ3_fearmongering_propagation_results.rds")

cat("\n=== RESULTS SAVED ===\n")
