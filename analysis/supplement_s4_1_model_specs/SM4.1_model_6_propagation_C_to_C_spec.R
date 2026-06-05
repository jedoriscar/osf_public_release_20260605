# Purpose
# Document Model 6 specification: Parent C → Child C (beta regression).
# Provides complete model specification for propagation analyses.
#
# Reference: SI Appendix Section 4.1, Main text lines 100-101
# Model: Beta regression multilevel model

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
  filter(!is.na(parent_constructiveness), !is.na(child_constructiveness))

# Transform for beta regression
dyads_analysis <- dyads_analysis %>%
  mutate(
    child_constructiveness_beta = ifelse(child_constructiveness == 0, 0.0001,
                                        ifelse(child_constructiveness == 1, 0.9999, child_constructiveness))
  )

# Model specification
cat("=== MODEL 6: PARENT C → CHILD C (BETA REGRESSION) ===\n\n")

cat("Model Type: Generalized Linear Mixed Model (GLMM)\n")
cat("Family: Beta (logit link)\n")
cat("Package: glmmTMB\n\n")

cat("Formula:\n")
cat("  child_constructiveness_beta ~ parent_constructiveness + (1|video_id)\n\n")

cat("Fixed Effects:\n")
cat("  - parent_constructiveness: Parent comment constructiveness (continuous, 0-1)\n\n")

cat("Random Effects:\n")
cat("  - (1|video_id): Random intercept for video\n\n")

cat("N: ", nrow(dyads_analysis), " parent-child dyads\n")
cat("N videos: ", length(unique(dyads_analysis$video_id)), "\n\n")

# Run model
mod1 <- glmmTMB(
  child_constructiveness_beta ~ parent_constructiveness + (1|video_id),
  data = dyads_analysis,
  family = beta_family()
)

summary(mod1)
confint(mod1)

# Extract results
coefs <- fixef(mod1)$cond
ses <- sqrt(diag(vcov(mod1)$cond))
ci <- confint(mod1)

cat("\n=== KEY RESULTS ===\n")
cat(sprintf("Parent Constructiveness coefficient: β = %.3f (SE = %.3f)\n", coefs[2], ses[2]))
cat(sprintf("95%% CI: [%.3f, %.3f]\n", ci[2,1], ci[2,2]))
cat(sprintf("z = %.2f, p < .001\n", coefs[2] / ses[2]))

# Save results
results <- list(
  model = mod1,
  model_type = "Beta Regression Multilevel Model",
  formula = "child_constructiveness_beta ~ parent_constructiveness + (1|video_id)",
  coefficient = coefs[2],
  se = ses[2],
  ci = ci[2,],
  n = nrow(dyads_analysis)
)

saveRDS(results, "analysis/supplement_s4_1_model_specs/SM4.1_model_6_propagation_C_to_C_spec_results.rds")

cat("\n=== RESULTS SAVED ===\n")
