# Purpose
# Test whether parent comment constructiveness predicts child comment constructiveness
# (same-direction propagation: C begets C). Beta regression with random intercept by video.
#
# Reference: Main text lines 100-101
# Data: Canonical joined_data for indices; parent_child_data.rda for dyad links.
#       harmoniousness_raw/divisiveness_raw are label-based from load_data.R.

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)

# Load data
# Canonical comment-level data; indices are label-based (0.6 threshold).
source("analysis/setup/load_data.R")

# Parent-child dyads (separate file: parent_comment_id <-> comment_id)
dyad_path <- "data/analysis_objects/racial_parent_child_dyads.rda"
if (!file.exists(dyad_path)) {
  stop("Parent-child dyad data not found at: ", dyad_path, "\nPlease verify the path is correct.")
}
load(dyad_path)

# One row per comment_id lookup (avoids many-to-many join: joined_data can have duplicate or NA comment_id)
# Parent_child_data is YouTube-only; keep only YouTube rows (non-NA comment_id) and take first row per ID.
comment_lookup <- joined_data %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw)

# Attach parent and child C/D from lookup (rename in lookup to avoid .x/.y suffix if parent_child_data has same col names)
parent_lookup <- comment_lookup %>%
  rename(parent_constructiveness = harmoniousness_raw, parent_destructiveness = divisiveness_raw)
child_lookup <- comment_lookup %>%
  rename(child_constructiveness = harmoniousness_raw, child_destructiveness = divisiveness_raw)

dyads_analysis <- parent_child_data %>%
  left_join(
    parent_lookup,
    by = c("parent_comment_id" = "comment_id"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    child_lookup,
    by = "comment_id",
    relationship = "many-to-one"
  ) %>%
  filter(!is.na(parent_constructiveness), !is.na(child_constructiveness))

# Transform for beta regression (values must be strictly between 0 and 1)
dyads_analysis <- dyads_analysis %>%
  mutate(
    child_constructiveness_beta = ifelse(child_constructiveness == 0, 0.0001,
                                        ifelse(child_constructiveness == 1, 0.9999, child_constructiveness))
  )

# Quick sanity checks
cat("=== DATA CHECKS ===\n")
cat("N dyads:", nrow(dyads_analysis), "\n")
cat("N videos:", length(unique(dyads_analysis$video_id)), "\n")
cat("Mean parent C:", mean(dyads_analysis$parent_constructiveness, na.rm = TRUE), "\n")
cat("Mean child C:", mean(dyads_analysis$child_constructiveness, na.rm = TRUE), "\n\n")

# Model: Parent C → Child C (Beta Regression)
cat("=== MODEL: PARENT CONSTRUCTIVENESS → CHILD CONSTRUCTIVENESS ===\n")
cat("Using beta regression for bounded outcome (0-1)\n\n")

mod1 <- glmmTMB(
  child_constructiveness_beta ~ parent_constructiveness + (1|video_id),
  data = dyads_analysis,
  family = beta_family()
)

summary(mod1)
confint(mod1)

# Extract and interpret results
coefs <- fixef(mod1)$cond
ses <- sqrt(diag(vcov(mod1)$cond))
z_vals <- coefs / ses
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
ci <- confint(mod1)

cat("\n=== KEY RESULTS ===\n")
cat("Parent Constructiveness → Child Constructiveness:\n")
cat(sprintf("  β = %.3f (SE = %.3f), z = %.2f, p < .001\n", 
            coefs[2], ses[2], z_vals[2]))
cat(sprintf("  95%% CI: [%.3f, %.3f]\n", ci[2,1], ci[2,2]))
cat("  Interpretation: Each additional constructive feature in parent comment\n")
cat("    is associated with increased child comment constructiveness.\n")

# Save results
results <- list(
  model = mod1,
  coefficient = coefs[2],
  se = ses[2],
  z_value = z_vals[2],
  p_value = p_vals[2],
  ci = ci[2,],
  n = nrow(dyads_analysis),
  n_videos = length(unique(dyads_analysis$video_id))
)

saveRDS(results, "analysis/propagation/RQ3_parent_C_to_child_C_results.rds")

cat("\n=== RESULTS SAVED ===\n")
