# Purpose
# Re-run the four climate propagation beta regressions restricting to NOVEL
# COMMENTERS ONLY (child author != parent author). Parallel to
# RQ3_propagation_novel_commenters_only.R for racial change.
#
# This is the climate version used to promote the different-person dyad
# analysis to PRIMARY in the manuscript (Eli comment #8).
#
# Reference: Main text replication section
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)

# Load climate data
climate_data_path <- "data/analysis_objects/climate_comments.rds"
dyad_path         <- "data/analysis_objects/climate_parent_child_dyads.rds"

if (!file.exists(climate_data_path)) stop("Climate joined data not found.")
if (!file.exists(dyad_path))         stop("Climate parent-child dyad data not found.")

joined_data       <- readRDS(climate_data_path)
parent_child_data <- readRDS(dyad_path)

# Join harmoniousness_raw / divisiveness_raw (0-1 label-based indices) onto dyads.
parent_feat <- joined_data %>%
  transmute(comment_id = comment_id,
            parent_constructiveness = harmoniousness_raw,
            parent_destructiveness  = divisiveness_raw)
child_feat <- joined_data %>%
  transmute(comment_id = comment_id,
            child_constructiveness = harmoniousness_raw,
            child_destructiveness  = divisiveness_raw)

dyads_with_author <- parent_child_data %>%
  left_join(parent_feat, by = c("parent_comment_id" = "comment_id")) %>%
  left_join(child_feat,  by = c("child_comment_id"  = "comment_id")) %>%
  filter(!is.na(parent_constructiveness), !is.na(child_constructiveness),
         !is.na(parent_destructiveness),  !is.na(child_destructiveness)) %>%
  mutate(
    parent_author = trimws(tolower(replace_na(as.character(parent_username), ""))),
    child_author  = trimws(tolower(replace_na(as.character(child_username),  "")))
  )

# Novel commenters only: child is a different person than parent.
dyads_novel <- dyads_with_author %>%
  filter(parent_author != child_author) %>%
  filter(nchar(parent_author) > 0 | nchar(child_author) > 0)

# Beta-bounded transform
dyads_novel <- dyads_novel %>%
  mutate(
    child_constructiveness_beta = ifelse(child_constructiveness == 0, 0.0001,
                                  ifelse(child_constructiveness == 1, 0.9999, child_constructiveness)),
    child_destructiveness_beta  = ifelse(child_destructiveness == 0, 0.0001,
                                  ifelse(child_destructiveness == 1, 0.9999, child_destructiveness))
  )

# Sanity checks
cat("=== CLIMATE RQ3 NOVEL COMMENTERS ONLY ===\n")
cat("N all dyads (with C/D):", nrow(dyads_with_author), "\n")
cat("N novel-commenter dyads (child author != parent author):", nrow(dyads_novel), "\n")
cat("N same-person dyads excluded:", nrow(dyads_with_author) - nrow(dyads_novel), "\n")
cat("N unique videos in novel subset:", length(unique(dyads_novel$video_id)), "\n\n")

# Four beta regressions
run_beta <- function(formula, data, label) {
  mod   <- glmmTMB(formula, data = data, family = beta_family())
  coefs <- fixef(mod)$cond
  ses   <- sqrt(diag(vcov(mod)$cond))
  zs    <- coefs / ses
  ps    <- 2 * (1 - pnorm(abs(zs)))
  ci    <- confint(mod)
  cat(sprintf("--- %s ---\n", label))
  cat(sprintf("  beta = %.4f, SE = %.4f, z = %.3f, p = %.3e, 95%% CI [%.4f, %.4f], N = %d\n\n",
              coefs[2], ses[2], zs[2], ps[2], ci[2,1], ci[2,2], nrow(data)))
  list(label = label, model = mod,
       coef = coefs[2], se = ses[2], z = zs[2], p = ps[2],
       ci_lo = ci[2,1], ci_hi = ci[2,2], n = nrow(data))
}

res_C_to_C <- run_beta(
  child_constructiveness_beta ~ parent_constructiveness + (1 | video_id),
  dyads_novel, "Parent C -> Child C"
)
res_D_to_D <- run_beta(
  child_destructiveness_beta  ~ parent_destructiveness  + (1 | video_id),
  dyads_novel, "Parent D -> Child D"
)
res_C_to_D <- run_beta(
  child_destructiveness_beta  ~ parent_constructiveness + (1 | video_id),
  dyads_novel, "Parent C -> Child D"
)
res_D_to_C <- run_beta(
  child_constructiveness_beta ~ parent_destructiveness  + (1 | video_id),
  dyads_novel, "Parent D -> Child C (chilling)"
)

# Save
results <- list(
  dyads_novel_n = nrow(dyads_novel),
  dyads_all_n   = nrow(dyads_with_author),
  n_videos      = length(unique(dyads_novel$video_id)),
  C_to_C        = res_C_to_C,
  D_to_D        = res_D_to_D,
  C_to_D        = res_C_to_D,
  D_to_C        = res_D_to_C
)
saveRDS(results, "analysis/climate_replication/CLIMATE_RQ3_propagation_novel_commenters_only_results.rds")
cat("=== RESULTS SAVED ===\n")
