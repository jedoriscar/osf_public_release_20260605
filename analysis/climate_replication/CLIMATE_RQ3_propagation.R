# Purpose
# Replicate RQ3 (propagation) analyses in climate change dataset.
# Tests whether discourse features propagate through reply chains.
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

# Join with joined_data to get harmoniousness_raw/divisiveness_raw (0-1 scale, matches racial)
parent_feat <- joined_data %>% transmute(comment_id = comment_id, parent_constructiveness = harmoniousness_raw, parent_destructiveness = divisiveness_raw)
child_feat <- joined_data %>% transmute(comment_id = comment_id, child_constructiveness = harmoniousness_raw, child_destructiveness = divisiveness_raw)
dyads_analysis <- parent_child_data %>%
  left_join(parent_feat, by = c("parent_comment_id" = "comment_id")) %>%
  left_join(child_feat, by = c("child_comment_id" = "comment_id")) %>%
  filter(!is.na(parent_constructiveness), !is.na(child_constructiveness))

# Transform for beta regression
dyads_analysis <- dyads_analysis %>%
  mutate(
    child_constructiveness_beta = ifelse(child_constructiveness == 0, 0.0001,
                                        ifelse(child_constructiveness == 1, 0.9999, child_constructiveness)),
    child_destructiveness_beta = ifelse(child_destructiveness == 0, 0.0001,
                                       ifelse(child_destructiveness == 1, 0.9999, child_destructiveness))
  )

# Models: Same-direction propagation
cat("=== CLIMATE REPLICATION: RQ3 PROPAGATION ===\n\n")

mod_C_to_C <- glmmTMB(
  child_constructiveness_beta ~ parent_constructiveness + (1|video_id),
  data = dyads_analysis,
  family = beta_family()
)

mod_D_to_D <- glmmTMB(
  child_destructiveness_beta ~ parent_destructiveness + (1|video_id),
  data = dyads_analysis,
  family = beta_family()
)

coef_C_C <- fixef(mod_C_to_C)$cond
coef_D_D <- fixef(mod_D_to_D)$cond

cat("Parent constructiveness → child constructiveness:\n")
cat(sprintf("  β = %.3f (SE = %.3f)\n", coef_C_C[2], sqrt(diag(vcov(mod_C_to_C)$cond))[2]))

cat("\nParent destructiveness → child destructiveness:\n")
cat(sprintf("  β = %.3f (SE = %.3f)\n", coef_D_D[2], sqrt(diag(vcov(mod_D_to_D)$cond))[2]))

cat("\nN dyads:", nrow(dyads_analysis), "\n")

# Save results
results <- list(
  C_to_C_model = mod_C_to_C,
  D_to_D_model = mod_D_to_D,
  C_to_C_beta = coef_C_C[2],
  D_to_D_beta = coef_D_D[2],
  n = nrow(dyads_analysis)
)

saveRDS(results, "analysis/climate_replication/CLIMATE_RQ3_propagation_results.rds")

cat("\n=== RESULTS SAVED ===\n")
