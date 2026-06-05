# Purpose
# Formally test whether the same-direction propagation effect differs in
# magnitude between the constructive-feature index (C->C) and the destructive-
# feature index (D->D) in the PRIMARY different-person dyad subset.
#
# Approach: Stack each dyad into two rows --
#   row 1 contributes the C path  (outcome = child C, predictor = parent C)
#   row 2 contributes the D path  (outcome = child D, predictor = parent D)
# Fit a single multilevel beta regression with random intercepts for video and
# random intercepts for dyad (to handle the two rows per dyad being correlated
# within the same child comment), with fixed effects:
#   matching_parent (continuous, on 0-1 scale)
# + path (C or D)
# + matching_parent : path  <-- this is the test of interest
#
# The interaction matching_parent:pathD = (beta_DD - beta_CC). A Wald test on
# this coefficient is the formal test of whether the same-direction effects
# differ between the two indices.
#
# Run for racial change AND climate change novel-commenter dyads.

# Note: load_data.R does rm(list = ls()), so we source it FIRST, then define
# helpers, then run.
rm(list = ls())
source("analysis/setup/load_data.R")
joined_racial <- joined_data
rm(joined_data)

suppressPackageStartupMessages({
  library(tidyverse)
  library(glmmTMB)
})

extract_one <- function(mod, label) {
  s  <- summary(mod)$coefficients$cond
  ci <- confint(mod)
  cat(sprintf("\n--- %s ---\n", label))
  print(round(s, 4))
  rn      <- rownames(s)
  int_row <- grep(":", rn, value = TRUE)
  cat(sprintf("\nInteraction term: %s\n", int_row))
  cat(sprintf("  Estimate (beta_DD - beta_CC) = %.4f\n",
              s[int_row, "Estimate"]))
  cat(sprintf("  SE                             = %.4f\n",
              s[int_row, "Std. Error"]))
  cat(sprintf("  z                              = %.3f\n",
              s[int_row, "z value"]))
  cat(sprintf("  p                              = %.4f\n",
              s[int_row, "Pr(>|z|)"]))
  cat(sprintf("  95%% CI on difference: [%.4f, %.4f]\n",
              ci[int_row, 1], ci[int_row, 2]))
  invisible(list(estimate = s[int_row, "Estimate"],
                 se = s[int_row, "Std. Error"],
                 z  = s[int_row, "z value"],
                 p  = s[int_row, "Pr(>|z|)"],
                 ci_lo = ci[int_row, 1],
                 ci_hi = ci[int_row, 2]))
}

build_stacked <- function(dyads) {
  long_C <- dyads %>%
    transmute(video_id, dyad_id,
              path = "C",
              matching_parent = parent_C,
              child_outcome   = child_C)
  long_D <- dyads %>%
    transmute(video_id, dyad_id,
              path = "D",
              matching_parent = parent_D,
              child_outcome   = child_D)
  bind_rows(long_C, long_D) %>%
    mutate(
      path = factor(path, levels = c("C", "D")),
      child_outcome_beta = pmin(pmax(child_outcome, 0.0001), 0.9999)
    )
}

############################################################
# RACIAL CHANGE
############################################################
cat("\n==================================================\n")
cat("RACIAL CHANGE: stacked-model test for C->C vs D->D\n")
cat("==================================================\n")

load("data/analysis_objects/racial_parent_child_dyads.rda")

parent_username_lookup <- joined_racial %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, username) %>%
  rename(parent_comment_id = comment_id, parent_username = username)

comment_lookup <- joined_racial %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw)

parent_lookup <- comment_lookup %>%
  rename(parent_C = harmoniousness_raw, parent_D = divisiveness_raw)
child_lookup <- comment_lookup %>%
  rename(child_C = harmoniousness_raw, child_D = divisiveness_raw)

dyads_with_author <- parent_child_data %>%
  left_join(parent_username_lookup, by = "parent_comment_id") %>%
  left_join(parent_lookup, by = c("parent_comment_id" = "comment_id"),
            relationship = "many-to-one") %>%
  left_join(child_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(parent_C), !is.na(child_C),
         !is.na(parent_D), !is.na(child_D)) %>%
  mutate(
    parent_author = trimws(tolower(replace_na(as.character(parent_username), ""))),
    child_author  = trimws(tolower(replace_na(as.character(username), "")))
  )

dyads_novel_racial <- dyads_with_author %>%
  filter(parent_author != child_author) %>%
  filter(nchar(parent_author) > 0 | nchar(child_author) > 0) %>%
  mutate(dyad_id = row_number()) %>%
  select(dyad_id, video_id, parent_C, parent_D, child_C, child_D)

cat("N racial novel-commenter dyads:", nrow(dyads_novel_racial), "\n")

stacked_racial <- build_stacked(dyads_novel_racial)
cat("N stacked rows (racial):", nrow(stacked_racial), "\n")

cat("\nFitting stacked beta regression (racial)...\n")
mod_racial <- glmmTMB(
  child_outcome_beta ~ matching_parent * path +
    (1 | video_id) + (1 | dyad_id),
  data   = stacked_racial,
  family = beta_family()
)

res_racial <- extract_one(mod_racial, "RACIAL stacked beta")

s_r <- summary(mod_racial)$coefficients$cond
beta_CC_joint <- s_r["matching_parent", "Estimate"]
beta_DD_joint <- s_r["matching_parent", "Estimate"] +
                 s_r["matching_parent:pathD", "Estimate"]
cat(sprintf("\nImplied same-direction effects (RACIAL, joint model):\n"))
cat(sprintf("  beta_CC (C path) = %.4f\n", beta_CC_joint))
cat(sprintf("  beta_DD (D path) = %.4f\n", beta_DD_joint))
cat(sprintf("  Difference beta_DD - beta_CC = %.4f\n",
            s_r["matching_parent:pathD", "Estimate"]))

############################################################
# CLIMATE CHANGE
############################################################
cat("\n==================================================\n")
cat("CLIMATE CHANGE: stacked-model test for C->C vs D->D\n")
cat("==================================================\n")

climate_data_path <- "data/analysis_objects/climate_comments.rds"
dyad_path_clim    <- "data/analysis_objects/climate_parent_child_dyads.rds"
joined_climate   <- readRDS(climate_data_path)
pc_climate       <- readRDS(dyad_path_clim)

parent_feat_clim <- joined_climate %>%
  transmute(comment_id = comment_id,
            parent_C = harmoniousness_raw,
            parent_D = divisiveness_raw)
child_feat_clim  <- joined_climate %>%
  transmute(comment_id = comment_id,
            child_C = harmoniousness_raw,
            child_D = divisiveness_raw)

dyads_clim_all <- pc_climate %>%
  left_join(parent_feat_clim, by = c("parent_comment_id" = "comment_id")) %>%
  left_join(child_feat_clim,  by = c("child_comment_id"  = "comment_id")) %>%
  filter(!is.na(parent_C), !is.na(child_C),
         !is.na(parent_D), !is.na(child_D)) %>%
  mutate(
    parent_author = trimws(tolower(replace_na(as.character(parent_username), ""))),
    child_author  = trimws(tolower(replace_na(as.character(child_username),  "")))
  )

dyads_novel_clim <- dyads_clim_all %>%
  filter(parent_author != child_author) %>%
  filter(nchar(parent_author) > 0 | nchar(child_author) > 0) %>%
  mutate(dyad_id = row_number()) %>%
  select(dyad_id, video_id, parent_C, parent_D, child_C, child_D)

cat("N climate novel-commenter dyads:", nrow(dyads_novel_clim), "\n")

stacked_clim <- build_stacked(dyads_novel_clim)
cat("N stacked rows (climate):", nrow(stacked_clim), "\n")

cat("\nFitting stacked beta regression (climate)...\n")
mod_clim <- glmmTMB(
  child_outcome_beta ~ matching_parent * path +
    (1 | video_id) + (1 | dyad_id),
  data   = stacked_clim,
  family = beta_family()
)

res_clim <- extract_one(mod_clim, "CLIMATE stacked beta")

s_c <- summary(mod_clim)$coefficients$cond
beta_CC_joint_clim <- s_c["matching_parent", "Estimate"]
beta_DD_joint_clim <- s_c["matching_parent", "Estimate"] +
                      s_c["matching_parent:pathD", "Estimate"]
cat(sprintf("\nImplied same-direction effects (CLIMATE, joint model):\n"))
cat(sprintf("  beta_CC (C path) = %.4f\n", beta_CC_joint_clim))
cat(sprintf("  beta_DD (D path) = %.4f\n", beta_DD_joint_clim))
cat(sprintf("  Difference beta_DD - beta_CC = %.4f\n",
            s_c["matching_parent:pathD", "Estimate"]))

############################################################
# Save
############################################################
out <- list(
  racial = list(
    n_dyads     = nrow(dyads_novel_racial),
    model       = mod_racial,
    interaction = res_racial,
    beta_CC     = beta_CC_joint,
    beta_DD     = beta_DD_joint
  ),
  climate = list(
    n_dyads     = nrow(dyads_novel_clim),
    model       = mod_clim,
    interaction = res_clim,
    beta_CC     = beta_CC_joint_clim,
    beta_DD     = beta_DD_joint_clim
  )
)
saveRDS(out, "analysis/propagation/RQ3_test_CC_vs_DD_propagation_results.rds")
cat("\n=== RESULTS SAVED ===\n")
