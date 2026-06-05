# Purpose
# Document Model 8 specifications: Cross-propagation effects (beta regression).
# Parent C → Child D and Parent D → Child C (chilling effect).
#
# Reference: SI Appendix Section 4.1, Main text lines 101, 102-104
# Models: Beta regression multilevel models

# Setup
rm(list = ls())
library(lme4)
library(lmerTest)
library(glmmTMB)

# Load data
source("analysis/setup/load_data.R")

# Load parent-child dyad data
dyad_path <- "data/analysis_objects/racial_parent_child_dyads.rda"
if (!file.exists(dyad_path)) {
  stop("Parent-child dyad data not found.")
}

load(dyad_path)

comment_lookup <- joined_data %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw)
parent_lookup <- comment_lookup %>%
  rename(parent_constructiveness = harmoniousness_raw, parent_destructiveness = divisiveness_raw)
child_lookup <- comment_lookup %>%
  rename(child_constructiveness = harmoniousness_raw, child_destructiveness = divisiveness_raw)

# Merge parent and child features
dyads_analysis <- parent_child_data %>%
  left_join(parent_lookup, by = c("parent_comment_id" = "comment_id"), relationship = "many-to-one") %>%
  left_join(child_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(parent_constructiveness), !is.na(parent_destructiveness),
         !is.na(child_constructiveness), !is.na(child_destructiveness))

# Transform for beta regression
dyads_analysis <- dyads_analysis %>%
  mutate(
    child_constructiveness_beta = ifelse(child_constructiveness == 0, 0.0001,
                                        ifelse(child_constructiveness == 1, 0.9999, child_constructiveness)),
    child_destructiveness_beta = ifelse(child_destructiveness == 0, 0.0001,
                                       ifelse(child_destructiveness == 1, 0.9999, child_destructiveness))
  )

# Model 8a: Parent C → Child D
cat("=== MODEL 8a: PARENT C → CHILD D (BETA REGRESSION) ===\n\n")

cat("Formula:\n")
cat("  child_destructiveness_beta ~ parent_constructiveness + (1|video_id)\n\n")

mod8a <- glmmTMB(
  child_destructiveness_beta ~ parent_constructiveness + (1|video_id),
  data = dyads_analysis,
  family = beta_family()
)

coefs_8a <- fixef(mod8a)$cond
ses_8a <- sqrt(diag(vcov(mod8a)$cond))
ci_8a <- confint(mod8a)

cat("Results:\n")
cat(sprintf("  β = %.3f (SE = %.3f), 95%% CI [%.3f, %.3f]\n", 
            coefs_8a[2], ses_8a[2], ci_8a[2,1], ci_8a[2,2]))

# Model 8b: Parent D → Child C (Chilling Effect)
cat("\n=== MODEL 8b: PARENT D → CHILD C (CHILLING EFFECT) ===\n\n")

cat("Formula:\n")
cat("  child_constructiveness_beta ~ parent_destructiveness + (1|video_id)\n\n")

mod8b <- glmmTMB(
  child_constructiveness_beta ~ parent_destructiveness + (1|video_id),
  data = dyads_analysis,
  family = beta_family()
)

coefs_8b <- fixef(mod8b)$cond
ses_8b <- sqrt(diag(vcov(mod8b)$cond))
ci_8b <- confint(mod8b)

cat("Results:\n")
cat(sprintf("  β = %.3f (SE = %.3f), 95%% CI [%.3f, %.3f]\n", 
            coefs_8b[2], ses_8b[2], ci_8b[2,1], ci_8b[2,2]))

cat("\nChilling effect: Destructiveness suppresses constructiveness more powerfully\n")
cat("than constructiveness suppresses destructiveness (asymmetric effect).\n")

# Save results
results <- list(
  model_8a = mod8a,
  model_8b = mod8b,
  model_8a_coef = coefs_8a[2],
  model_8a_se = ses_8a[2],
  model_8a_ci = ci_8a[2,],
  model_8b_coef = coefs_8b[2],
  model_8b_se = ses_8b[2],
  model_8b_ci = ci_8b[2,],
  n = nrow(dyads_analysis)
)

saveRDS(results, "analysis/supplement_s4_1_model_specs/SM4.1_model_8_propagation_cross_effects_spec_results.rds")

cat("\n=== RESULTS SAVED ===\n")
