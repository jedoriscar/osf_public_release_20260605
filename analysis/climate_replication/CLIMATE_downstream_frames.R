# Purpose
# Replicate downstream frames (alienation, fearmongering, scapegoating) in climate.
# Logistic MLM: parent constructiveness/destructiveness → child binary (prob >= 0.6)
# Mirrors RQ3_alienation_propagation.R, RQ3_fearmongering_propagation.R, RQ3_scapegoating_propagation.R
#
# Reference: Main text 10_results_replication (ORs for downstream patterns)

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

# Load data
climate_path <- "data/analysis_objects/climate_comments.rds"
dyad_path <- "data/analysis_objects/climate_parent_child_dyads.rds"

joined_data <- readRDS(climate_path)
parent_child_data <- readRDS(dyad_path)

# Merge dyads with parent/child features from joined_data
run_frame <- function(outcome_col) {
  dyads <- parent_child_data %>%
    left_join(
      joined_data %>% select(comment_id, harmoniousness_raw, divisiveness_raw),
      by = c("parent_comment_id" = "comment_id")
    ) %>%
    rename(parent_constructiveness = harmoniousness_raw, parent_destructiveness = divisiveness_raw) %>%
    left_join(
      joined_data %>% select(comment_id, !!sym(outcome_col)),
      by = c("child_comment_id" = "comment_id")
    ) %>%
    mutate(child_binary = as.numeric(!!sym(outcome_col) >= 0.6)) %>%
    filter(!is.na(child_binary), !is.na(parent_constructiveness), !is.na(parent_destructiveness))

  mod <- glmer(
    child_binary ~ parent_constructiveness + parent_destructiveness + (1|video_id),
    data = dyads,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  )

  coefs <- fixef(mod)
  ses <- sqrt(diag(vcov(mod)))
  ors <- exp(coefs)
  
  # Wald CI for fixed effects: exp(beta +/- 1.96*SE)
  ci_C <- exp(coefs[2] + c(-1, 1) * 1.96 * ses[2])
  ci_D <- exp(coefs[3] + c(-1, 1) * 1.96 * ses[3])

  list(
    C_or = ors[2], C_ci = ci_C, C_p = 2*(1-pnorm(abs(coefs[2]/ses[2]))),
    D_or = ors[3], D_ci = ci_D, D_p = 2*(1-pnorm(abs(coefs[3]/ses[3]))),
    n = nrow(dyads)
  )
}

# Run all three
cat("=== CLIMATE REPLICATION: DOWNSTREAM FRAMES ===\n\n")

alienation <- run_frame("prob_alienation")
fearmongering <- run_frame("prob_fearmongering")
scapegoating <- run_frame("prob_scapegoating")

cat("Alienation:\n")
cat(sprintf("  Parent constructiveness: OR = %.2f [%.2f, %.2f], p = %.3f\n", alienation$C_or, alienation$C_ci[1], alienation$C_ci[2], alienation$C_p))
cat(sprintf("  Parent destructiveness:  OR = %.2f [%.2f, %.2f], p = %.3f\n", alienation$D_or, alienation$D_ci[1], alienation$D_ci[2], alienation$D_p))

cat("\nFearmongering:\n")
cat(sprintf("  Parent constructiveness: OR = %.2f [%.2f, %.2f], p = %.3f\n", fearmongering$C_or, fearmongering$C_ci[1], fearmongering$C_ci[2], fearmongering$C_p))
cat(sprintf("  Parent destructiveness:  OR = %.2f [%.2f, %.2f], p = %.3f\n", fearmongering$D_or, fearmongering$D_ci[1], fearmongering$D_ci[2], fearmongering$D_p))

cat("\nScapegoating:\n")
cat(sprintf("  Parent constructiveness: OR = %.2f [%.2f, %.2f], p = %.3f\n", scapegoating$C_or, scapegoating$C_ci[1], scapegoating$C_ci[2], scapegoating$C_p))
cat(sprintf("  Parent destructiveness:  OR = %.2f [%.2f, %.2f], p = %.3f\n", scapegoating$D_or, scapegoating$D_ci[1], scapegoating$D_ci[2], scapegoating$D_p))

# OR ranges for manuscript (constructiveness reduces; destructiveness increases alienation/scapegoating)
C_ors <- c(alienation$C_or, fearmongering$C_or, scapegoating$C_or)
D_sig <- c(alienation$D_or, scapegoating$D_or)  # alienation and scapegoating (fearmongering ns)
cat("\n=== MANUSCRIPT OR RANGES ===\n")
cat(sprintf("Constructiveness (reduces): ORs = %.2f–%.2f\n", min(C_ors), max(C_ors)))
cat(sprintf("Destructiveness (alienation, scapegoating): ORs = %.2f–%.2f\n", min(D_sig), max(D_sig)))

results <- list(alienation = alienation, fearmongering = fearmongering, scapegoating = scapegoating)
saveRDS(results, "analysis/climate_replication/CLIMATE_downstream_frames_results.rds")
cat("\n=== RESULTS SAVED ===\n")
