# Purpose
# Test cross-propagation (chilling) effects in climate change dataset:
# 1. Parent C → Child D (constructiveness suppresses destructiveness)
# 2. Parent D → Child C (destructiveness suppresses constructiveness - "chilling effect")
#
# Reference: Main text 10_results_replication

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)

# Load climate data
climate_data_path <- "data/analysis_objects/climate_comments.rds"
dyad_path <- "data/analysis_objects/climate_parent_child_dyads.rds"

if (!file.exists(climate_data_path)) stop("Climate data not found.")
if (!file.exists(dyad_path)) stop("Climate parent-child dyad data not found.")

joined_data <- readRDS(climate_data_path)
parent_child_data <- readRDS(dyad_path)

# Join with joined_data to get harmoniousness_raw/divisiveness_raw
parent_feat <- joined_data %>% transmute(comment_id = comment_id, parent_constructiveness = harmoniousness_raw, parent_destructiveness = divisiveness_raw)
child_feat <- joined_data %>% transmute(comment_id = comment_id, child_constructiveness = harmoniousness_raw, child_destructiveness = divisiveness_raw)
dyads_analysis <- parent_child_data %>%
  left_join(parent_feat, by = c("parent_comment_id" = "comment_id")) %>%
  left_join(child_feat, by = c("child_comment_id" = "comment_id")) %>%
  filter(!is.na(parent_constructiveness), !is.na(child_constructiveness),
         !is.na(parent_destructiveness), !is.na(child_destructiveness))

# Transform for beta regression
dyads_analysis <- dyads_analysis %>%
  mutate(
    child_constructiveness_beta = ifelse(child_constructiveness == 0, 0.0001,
                                        ifelse(child_constructiveness == 1, 0.9999, child_constructiveness)),
    child_destructiveness_beta = ifelse(child_destructiveness == 0, 0.0001,
                                       ifelse(child_destructiveness == 1, 0.9999, child_destructiveness))
  )

# Model 1: Parent C → Child D
cat("=== CLIMATE CHILLING EFFECTS ===\n\n")
cat("Model 1: Parent Constructiveness → Child Destructiveness\n")

mod_C_to_D <- glmmTMB(
  child_destructiveness_beta ~ parent_constructiveness + (1|video_id),
  data = dyads_analysis,
  family = beta_family()
)

coef_C_D <- fixef(mod_C_to_D)$cond
se_C_D <- sqrt(diag(vcov(mod_C_to_D)$cond))
z_C_D <- coef_C_D / se_C_D
p_C_D <- 2 * (1 - pnorm(abs(z_C_D)))

cat(sprintf("  β = %.3f (SE = %.3f), z = %.2f, p = %.3f\n", 
            coef_C_D[2], se_C_D[2], z_C_D[2], p_C_D[2]))

# Model 2: Parent D → Child C (Chilling Effect)
cat("\nModel 2: Parent Destructiveness → Child Constructiveness (Chilling Effect)\n")

mod_D_to_C <- glmmTMB(
  child_constructiveness_beta ~ parent_destructiveness + (1|video_id),
  data = dyads_analysis,
  family = beta_family()
)

coef_D_C <- fixef(mod_D_to_C)$cond
se_D_C <- sqrt(diag(vcov(mod_D_to_C)$cond))
z_D_C <- coef_D_C / se_D_C
p_D_C <- 2 * (1 - pnorm(abs(z_D_C)))

cat(sprintf("  β = %.3f (SE = %.3f), z = %.2f, p = %.3f\n", 
            coef_D_C[2], se_D_C[2], z_D_C[2], p_D_C[2]))

# Compare asymmetry
cat("\n=== ASYMMETRIC CHILLING EFFECT ===\n")
cat(sprintf("Constructiveness suppresses destructiveness: β = %.3f\n", coef_C_D[2]))
cat(sprintf("Destructiveness suppresses constructiveness: β = %.3f\n", coef_D_C[2]))
if (abs(coef_D_C[2]) > abs(coef_C_D[2])) {
  ratio <- abs(coef_D_C[2]) / abs(coef_C_D[2])
  cat(sprintf("Asymmetry ratio: %.1fx (destructiveness chills %.1f times more powerfully)\n", ratio, ratio))
} else {
  cat("No asymmetric chilling effect detected.\n")
}

cat("\nN dyads:", nrow(dyads_analysis), "\n")

# Save results
results <- list(
  C_to_D_model = mod_C_to_D,
  D_to_C_model = mod_D_to_C,
  C_to_D_beta = coef_C_D[2],
  C_to_D_se = se_C_D[2],
  C_to_D_p = p_C_D[2],
  D_to_C_beta = coef_D_C[2],
  D_to_C_se = se_D_C[2],
  D_to_C_p = p_D_C[2],
  n = nrow(dyads_analysis)
)

saveRDS(results, "analysis/climate_replication/CLIMATE_RQ3_chilling_effects_results.rds")

cat("\n=== RESULTS SAVED ===\n")
