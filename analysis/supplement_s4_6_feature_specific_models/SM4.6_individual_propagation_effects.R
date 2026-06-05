# Purpose
# Test whether individual features in parent comments predict child comment
# constructiveness and destructiveness.
#
# This addresses whether propagation effects generalize across all features
# or are driven by a subset.
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
  distinct(comment_id, .keep_all = TRUE)
parent_feature_lookup <- comment_lookup %>%
  select(comment_id,
         parent_compassion = prob_compassion,
         parent_curiosity = prob_curiosity,
         parent_nuance = prob_nuance,
         parent_personal_story = prob_personal_story,
         parent_reasoning = prob_reasoning,
         parent_toxicity = prob_toxic,
         parent_identity_attack = prob_identity_attack,
         parent_threat = prob_threat,
         parent_attack_author = prob_attack_on_author,
         parent_attack_commenter = prob_attack_on_commenter)
child_lookup <- comment_lookup %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw) %>%
  rename(child_constructiveness = harmoniousness_raw, child_destructiveness = divisiveness_raw)

# Merge parent features onto dyads
dyads_with_features <- parent_child_data %>%
  left_join(parent_feature_lookup, by = c("parent_comment_id" = "comment_id"), relationship = "many-to-one") %>%
  left_join(child_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(child_constructiveness), !is.na(child_destructiveness))

# Create binary features (≥0.6 threshold)
dyads_with_features <- dyads_with_features %>%
  mutate(
    parent_compassion_bin = as.numeric(parent_compassion >= 0.6),
    parent_curiosity_bin = as.numeric(parent_curiosity >= 0.6),
    parent_nuance_bin = as.numeric(parent_nuance >= 0.6),
    parent_personal_story_bin = as.numeric(parent_personal_story >= 0.6),
    parent_reasoning_bin = as.numeric(parent_reasoning >= 0.6),
    parent_toxicity_bin = as.numeric(parent_toxicity >= 0.6),
    parent_identity_attack_bin = as.numeric(parent_identity_attack >= 0.6),
    parent_threat_bin = as.numeric(parent_threat >= 0.6),
  parent_attack_author_bin = as.numeric(parent_attack_author >= 0.6),
    parent_attack_commenter_bin = as.numeric(parent_attack_commenter >= 0.6),
    child_constructiveness_beta = ifelse(child_constructiveness == 0, 0.0001,
                                        ifelse(child_constructiveness == 1, 0.9999, child_constructiveness)),
    child_destructiveness_beta = ifelse(child_destructiveness == 0, 0.0001,
                                       ifelse(child_destructiveness == 1, 0.9999, child_destructiveness))
  )

# Models: Constructive features → Child constructiveness
cat("=== CONSTRUCTIVE FEATURES → CHILD CONSTRUCTIVENESS ===\n")

# Beta regression for bounded outcome
mod_compassion_C <- glmmTMB(
  child_constructiveness_beta ~ parent_compassion_bin + (1|video_id),
  data = dyads_with_features,
  family = beta_family()
)

mod_curiosity_C <- glmmTMB(
  child_constructiveness_beta ~ parent_curiosity_bin + (1|video_id),
  data = dyads_with_features,
  family = beta_family()
)

mod_nuance_C <- glmmTMB(
  child_constructiveness_beta ~ parent_nuance_bin + (1|video_id),
  data = dyads_with_features,
  family = beta_family()
)

mod_personal_story_C <- glmmTMB(
  child_constructiveness_beta ~ parent_personal_story_bin + (1|video_id),
  data = dyads_with_features,
  family = beta_family()
)

mod_reasoning_C <- glmmTMB(
  child_constructiveness_beta ~ parent_reasoning_bin + (1|video_id),
  data = dyads_with_features,
  family = beta_family()
)

# Models: Destructive features → Child destructiveness
cat("\n=== DESTRUCTIVE FEATURES → CHILD DESTRUCTIVENESS ===\n")

mod_toxicity_D <- glmmTMB(
  child_destructiveness_beta ~ parent_toxicity_bin + (1|video_id),
  data = dyads_with_features,
  family = beta_family()
)

mod_identity_attack_D <- glmmTMB(
  child_destructiveness_beta ~ parent_identity_attack_bin + (1|video_id),
  data = dyads_with_features,
  family = beta_family()
)

mod_threat_D <- glmmTMB(
  child_destructiveness_beta ~ parent_threat_bin + (1|video_id),
  data = dyads_with_features,
  family = beta_family()
)

mod_attack_author_D <- glmmTMB(
  child_destructiveness_beta ~ parent_attack_author_bin + (1|video_id),
  data = dyads_with_features,
  family = beta_family()
)

mod_attack_commenter_D <- glmmTMB(
  child_destructiveness_beta ~ parent_attack_commenter_bin + (1|video_id),
  data = dyads_with_features,
  family = beta_family()
)

# Save results
results <- list(
  constructive_to_constructive = list(
    compassion = mod_compassion_C,
    curiosity = mod_curiosity_C,
    nuance = mod_nuance_C,
    personal_story = mod_personal_story_C,
    reasoning = mod_reasoning_C
  ),
  destructive_to_destructive = list(
    toxicity = mod_toxicity_D,
    identity_attack = mod_identity_attack_D,
    threat = mod_threat_D,
    attack_author = mod_attack_author_D,
    attack_commenter = mod_attack_commenter_D
  ),
  n = nrow(dyads_with_features)
)

saveRDS(results, "analysis/supplement_s4_6_feature_specific_models/SM4.6_individual_propagation_results.rds")

cat("\n=== RESULTS SAVED ===\n")
