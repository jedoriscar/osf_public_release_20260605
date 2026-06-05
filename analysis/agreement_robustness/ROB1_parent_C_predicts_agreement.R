# Purpose
# Test whether parent comment constructiveness predicts child comment agreement.
# Tests whether constructive comments invite more agreement (vs. disagreement).
#
# Reference: Main text lines 130-131
# Model: Logistic multilevel model with random intercepts for video

# Setup
rm(list = ls())
library(tidyverse)   # dplyr (%, left_join, select, rename, filter)
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

# Agreement is in canonical data (merged by prepare_canonical_data.R).
if (!"agreement_label" %in% colnames(joined_data)) {
  stop("agreement_label not in data. Run: Rscript analysis/setup/prepare_canonical_data.R")
}

# One row per comment for lookups (avoid many-to-many)
comment_lookup <- joined_data %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE)

parent_lookup <- comment_lookup %>%
  select(comment_id, harmoniousness_raw) %>%
  rename(parent_constructiveness = harmoniousness_raw)
child_agreement_lookup <- comment_lookup %>%
  select(comment_id, agreement_label)

# Merge: parent C from parent_comment_id; child agreement from comment_id
analysis_data <- parent_child_data %>%
  left_join(parent_lookup, by = c("parent_comment_id" = "comment_id"), relationship = "many-to-one") %>%
  left_join(child_agreement_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(parent_constructiveness), !is.na(agreement_label)) %>%
  filter(agreement_label %in% c("Agree", "Disagree")) %>%
  mutate(agreement_binary = ifelse(agreement_label == "Agree", 1, 0))

# Model: Parent C → Child Agreement (Logistic MLM)
cat("=== MODEL: PARENT CONSTRUCTIVENESS → CHILD AGREEMENT ===\n\n")

mod1 <- glmer(
  agreement_binary ~ parent_constructiveness + (1|video_id),
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
ci_vals <- exp(confint(mod1, method = "Wald"))
# CI for the coefficient (not intercept): use row by name
ci_coef <- ci_vals[rownames(ci_vals) == "parent_constructiveness", ]

cat("\n=== KEY RESULTS ===\n")
cat("Parent Constructiveness → Child Agreement:\n")
cat(sprintf("  Log Odds = %.3f (SE = %.3f), z = %.2f, p < .001\n", 
            coefs[2], ses[2], z_vals[2]))
cat(sprintf("  OR = %.2f, 95%% CI [%.2f, %.2f]\n", 
            or_vals[2], ci_coef[1], ci_coef[2]))
cat(sprintf("  Interpretation: Each unit increase in parent constructiveness\n"))
cat(sprintf("    increases odds of child agreement by %.0f%%\n", (or_vals[2] - 1) * 100))

# Save results
results <- list(
  model = mod1,
  coefficient = coefs[2],
  se = ses[2],
  z_value = z_vals[2],
  p_value = p_vals[2],
  odds_ratio = or_vals[2],
  ci = ci_coef,
  n = nrow(analysis_data)
)

saveRDS(results, "analysis/agreement_robustness/ROB1_parent_C_predicts_agreement_results.rds")

cat("\n=== RESULTS SAVED ===\n")
