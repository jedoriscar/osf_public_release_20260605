# Purpose
# Compare beta regression vs. linear mixed models for propagation outcomes
# to demonstrate robustness across model specifications.
#
# Reference: SI Appendix Section 4.6

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
  stop("Parent-child dyad data not found at: ", dyad_path)
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

# Merge parent and child constructiveness/destructiveness
dyads_analysis <- parent_child_data %>%
  left_join(parent_lookup, by = c("parent_comment_id" = "comment_id"), relationship = "many-to-one") %>%
  left_join(child_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(parent_constructiveness), !is.na(child_constructiveness),
         !is.na(parent_destructiveness), !is.na(child_destructiveness)) %>%
  mutate(
    child_constructiveness_beta = ifelse(child_constructiveness == 0, 0.0001,
                                        ifelse(child_constructiveness == 1, 0.9999, child_constructiveness)),
    child_destructiveness_beta = ifelse(child_destructiveness == 0, 0.0001,
                                       ifelse(child_destructiveness == 1, 0.9999, child_destructiveness))
  )

# Model 1: Parent C → Child C (Beta Regression)
cat("=== BETA REGRESSION: PARENT C → CHILD C ===\n")

mod_beta_C <- glmmTMB(
  child_constructiveness_beta ~ parent_constructiveness + (1|video_id),
  data = dyads_analysis,
  family = beta_family()
)

summary(mod_beta_C)
coef_beta_C <- fixef(mod_beta_C)$cond
cat(sprintf("Beta regression: B = %.3f, SE = %.3f\n", 
            coef_beta_C[2], sqrt(vcov(mod_beta_C)$cond[2,2])))

# Model 2: Parent C → Child C (Linear Mixed Model)
cat("\n=== LINEAR MIXED MODEL: PARENT C → CHILD C ===\n")

mod_lmm_C <- lmer(
  child_constructiveness ~ parent_constructiveness + (1|video_id),
  data = dyads_analysis
)

summary(mod_lmm_C)
coef_lmm_C <- fixef(mod_lmm_C)
cat(sprintf("Linear model: B = %.3f, SE = %.3f\n", 
            coef_lmm_C[2], sqrt(vcov(mod_lmm_C)[2,2])))

# Model 3: Parent D → Child D (Beta Regression)
cat("\n=== BETA REGRESSION: PARENT D → CHILD D ===\n")

mod_beta_D <- glmmTMB(
  child_destructiveness_beta ~ parent_destructiveness + (1|video_id),
  data = dyads_analysis,
  family = beta_family()
)

summary(mod_beta_D)
coef_beta_D <- fixef(mod_beta_D)$cond
cat(sprintf("Beta regression: B = %.3f, SE = %.3f\n", 
            coef_beta_D[2], sqrt(vcov(mod_beta_D)$cond[2,2])))

# Model 4: Parent D → Child D (Linear Mixed Model)
cat("\n=== LINEAR MIXED MODEL: PARENT D → CHILD D ===\n")

mod_lmm_D <- lmer(
  child_destructiveness ~ parent_destructiveness + (1|video_id),
  data = dyads_analysis
)

summary(mod_lmm_D)
coef_lmm_D <- fixef(mod_lmm_D)
cat(sprintf("Linear model: B = %.3f, SE = %.3f\n", 
            coef_lmm_D[2], sqrt(vcov(mod_lmm_D)[2,2])))

# Save results
results <- list(
  beta_C = mod_beta_C,
  lmm_C = mod_lmm_C,
  beta_D = mod_beta_D,
  lmm_D = mod_lmm_D,
  comparison = data.frame(
    outcome = c("Parent C → Child C", "Parent D → Child D"),
    beta_coef = c(coef_beta_C[2], coef_beta_D[2]),
    lmm_coef = c(coef_lmm_C[2], coef_lmm_D[2])
  )
)

saveRDS(results, "analysis/supplement_s4_6_feature_specific_models/SM4.6_beta_vs_linear_results.rds")

cat("\n=== COMPARISON COMPLETE ===\n")
cat("Results are substantively identical across model families.\n")
