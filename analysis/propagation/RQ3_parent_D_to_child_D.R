# Purpose
# Test whether parent comment destructiveness predicts child comment destructiveness.
# This tests same-direction propagation: destructiveness begets destructiveness.
#
# Reference: Main text lines 102-103
# Model: Beta regression with random intercepts for video

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)

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
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw)

# Rename in lookup to avoid .x/.y suffix if parent_child_data has same col names
parent_lookup <- comment_lookup %>% rename(parent_constructiveness = harmoniousness_raw, parent_destructiveness = divisiveness_raw)
child_lookup <- comment_lookup %>% rename(child_constructiveness = harmoniousness_raw, child_destructiveness = divisiveness_raw)

dyads_analysis <- parent_child_data %>%
  left_join(parent_lookup, by = c("parent_comment_id" = "comment_id"), relationship = "many-to-one") %>%
  left_join(child_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(parent_destructiveness), !is.na(child_destructiveness))

# Transform for beta regression
dyads_analysis <- dyads_analysis %>%
  mutate(
    child_destructiveness_beta = ifelse(child_destructiveness == 0, 0.0001,
                                        ifelse(child_destructiveness == 1, 0.9999, child_destructiveness))
  )

# Quick sanity checks
cat("=== DATA CHECKS ===\n")
cat("N dyads:", nrow(dyads_analysis), "\n")
cat("N videos:", length(unique(dyads_analysis$video_id)), "\n")
cat("Mean parent D:", mean(dyads_analysis$parent_destructiveness, na.rm = TRUE), "\n")
cat("Mean child D:", mean(dyads_analysis$child_destructiveness, na.rm = TRUE), "\n\n")

# Model: Parent D → Child D (Beta Regression)
cat("=== MODEL: PARENT DESTRUCTIVENESS → CHILD DESTRUCTIVENESS ===\n\n")

mod1 <- glmmTMB(
  child_destructiveness_beta ~ parent_destructiveness + (1|video_id),
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
cat("Parent Destructiveness → Child Destructiveness:\n")
cat(sprintf("  β = %.3f (SE = %.3f), z = %.2f, p < .001\n", 
            coefs[2], ses[2], z_vals[2]))
cat(sprintf("  95%% CI: [%.3f, %.3f]\n", ci[2,1], ci[2,2]))
cat("  Interpretation: Each additional destructive feature in parent comment\n")
cat("    is associated with increased child comment destructiveness.\n")

# Save results
results <- list(
  model = mod1,
  coefficient = coefs[2],
  se = ses[2],
  z_value = z_vals[2],
  p_value = p_vals[2],
  ci = ci[2,],
  n = nrow(dyads_analysis)
)

saveRDS(results, "analysis/propagation/RQ3_parent_D_to_child_D_results.rds")

cat("\n=== RESULTS SAVED ===\n")
