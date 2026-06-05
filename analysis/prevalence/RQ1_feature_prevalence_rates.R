# Purpose
# Calculate prevalence rates for each individual discourse feature.
# Shows which constructive and destructive features are most common.
#
# Reference: Main text lines 64-65
# Reports: Compassion 30.1%, Curiosity 21.5%, etc.

# Setup
rm(list = ls())
library(tidyverse)   # dplyr (%, mutate)

# Load data
source("analysis/setup/load_data.R")

# Create binary feature indicators (≥0.6 threshold)
cat("=== FEATURE PREVALENCE RATES ===\n\n")

# Constructive features
joined_data <- joined_data %>%
  mutate(
    compassion_binary = as.numeric(prob_compassion >= 0.6),
    curiosity_binary = as.numeric(prob_curiosity >= 0.6),
    nuance_binary = as.numeric(prob_nuance >= 0.6),
    personal_story_binary = as.numeric(prob_personal_story >= 0.6),
    reasoning_binary = as.numeric(prob_reasoning >= 0.6),
    toxicity_binary = as.numeric(prob_toxic >= 0.6),
    identity_attack_binary = as.numeric(prob_identity_attack >= 0.6),
    threat_binary = as.numeric(prob_threat >= 0.6),
    attack_author_binary = as.numeric(prob_attack_on_author >= 0.6),
    attack_commenter_binary = as.numeric(prob_attack_on_commenter >= 0.6)
  )

# Calculate prevalence rates
cat("CONSTRUCTIVE FEATURES:\n")
compassion_rate <- mean(joined_data$compassion_binary, na.rm = TRUE) * 100
curiosity_rate <- mean(joined_data$curiosity_binary, na.rm = TRUE) * 100
nuance_rate <- mean(joined_data$nuance_binary, na.rm = TRUE) * 100
personal_story_rate <- mean(joined_data$personal_story_binary, na.rm = TRUE) * 100
reasoning_rate <- mean(joined_data$reasoning_binary, na.rm = TRUE) * 100

cat(sprintf("  Compassion: %.1f%%\n", compassion_rate))
cat(sprintf("  Curiosity: %.1f%%\n", curiosity_rate))
cat(sprintf("  Personal Story: %.1f%%\n", personal_story_rate))
cat(sprintf("  Reasoning: %.1f%%\n", reasoning_rate))
cat(sprintf("  Nuance: %.1f%%\n", nuance_rate))

cat("\nDESTRUCTIVE FEATURES:\n")
toxicity_rate <- mean(joined_data$toxicity_binary, na.rm = TRUE) * 100
identity_attack_rate <- mean(joined_data$identity_attack_binary, na.rm = TRUE) * 100
threat_rate <- mean(joined_data$threat_binary, na.rm = TRUE) * 100
attack_author_rate <- mean(joined_data$attack_author_binary, na.rm = TRUE) * 100
attack_commenter_rate <- mean(joined_data$attack_commenter_binary, na.rm = TRUE) * 100

cat(sprintf("  Attack on Commenter: %.1f%%\n", attack_commenter_rate))
cat(sprintf("  Toxicity: %.1f%%\n", toxicity_rate))
cat(sprintf("  Attack on Author: %.1f%%\n", attack_author_rate))
cat(sprintf("  Identity Attack: %.1f%%\n", identity_attack_rate))
cat(sprintf("  Threat: %.2f%%\n", threat_rate))

# Save results
results <- list(
  constructive_features = list(
    compassion = compassion_rate,
    curiosity = curiosity_rate,
    nuance = nuance_rate,
    personal_story = personal_story_rate,
    reasoning = reasoning_rate
  ),
  destructive_features = list(
    toxicity = toxicity_rate,
    identity_attack = identity_attack_rate,
    threat = threat_rate,
    attack_author = attack_author_rate,
    attack_commenter = attack_commenter_rate
  ),
  n = nrow(joined_data)
)

saveRDS(results, "analysis/prevalence/RQ1_feature_prevalence_rates_results.rds")

cat("\n=== RESULTS SAVED ===\n")
