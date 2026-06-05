# Purpose
# Re-run the four propagation beta regressions restricting to NOVEL COMMENTERS ONLY
# (child author != parent author). Tests whether propagation is between-person
# rather than same-person consistency (parent replying to self).
#
# Supplemental: SUPPLEMENTAL_ANALYSES_TO_RUN.md §1.1
# Compares coefficients and N to main RQ3 (all dyads).
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)

# Load data
source("analysis/setup/load_data.R")

dyad_path <- "data/analysis_objects/racial_parent_child_dyads.rda"
if (!file.exists(dyad_path)) stop("parent_child_data.rda not found.")
load(dyad_path)

# Parent username lookup (parent_comment_id -> parent_username)
parent_username_lookup <- joined_data %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, username) %>%
  rename(parent_comment_id = comment_id, parent_username = username)

comment_lookup <- joined_data %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw)

parent_lookup <- comment_lookup %>%
  rename(parent_constructiveness = harmoniousness_raw, parent_destructiveness = divisiveness_raw)
child_lookup <- comment_lookup %>%
  rename(child_constructiveness = harmoniousness_raw, child_destructiveness = divisiveness_raw)

# Dyads with author identifiers: child username is in parent_child_data; add parent username
dyads_with_author <- parent_child_data %>%
  left_join(parent_username_lookup, by = "parent_comment_id") %>%
  left_join(parent_lookup, by = c("parent_comment_id" = "comment_id"), relationship = "many-to-one") %>%
  left_join(child_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(parent_constructiveness), !is.na(child_constructiveness)) %>%
  mutate(
    parent_author = trimws(tolower(replace_na(as.character(parent_username), ""))),
    child_author  = trimws(tolower(replace_na(as.character(username), "")))
  )

# Novel commenters only: child is a different person than parent
dyads_novel <- dyads_with_author %>%
  filter(parent_author != child_author)

# Drop dyads where we couldn't determine author (both empty) to avoid counting as "novel"
dyads_novel <- dyads_novel %>%
  filter(nchar(parent_author) > 0 | nchar(child_author) > 0)

# Transform for beta (child C and child D)
dyads_novel <- dyads_novel %>%
  mutate(
    child_constructiveness_beta = ifelse(child_constructiveness == 0, 0.0001,
      ifelse(child_constructiveness == 1, 0.9999, child_constructiveness)),
    child_destructiveness_beta = ifelse(child_destructiveness == 0, 0.0001,
      ifelse(child_destructiveness == 1, 0.9999, child_destructiveness))
  )

# Sanity checks
cat("=== RQ3 NOVEL COMMENTERS ONLY ===\n")
cat("N all dyads (with C/D):", nrow(dyads_with_author), "\n")
cat("N novel commenter dyads (child author != parent author):", nrow(dyads_novel), "\n")
cat("N same-person dyads excluded:", nrow(dyads_with_author) - nrow(dyads_novel), "\n\n")

# Four beta regressions (same spec as main RQ3)
run_beta <- function(formula, data, label) {
  mod <- glmmTMB(formula, data = data, family = beta_family())
  coefs <- fixef(mod)$cond
  ses <- sqrt(diag(vcov(mod)$cond))
  ci <- confint(mod)
  list(
    label = label,
    model = mod,
    coef = coefs[2],
    se = ses[2],
    ci = ci[2,],
    n = nrow(data)
  )
}

res_C_to_C <- run_beta(
  child_constructiveness_beta ~ parent_constructiveness + (1|video_id),
  dyads_novel,
  "Parent C -> Child C"
)
res_C_to_D <- run_beta(
  child_destructiveness_beta ~ parent_constructiveness + (1|video_id),
  dyads_novel,
  "Parent C -> Child D"
)
res_D_to_C <- run_beta(
  child_constructiveness_beta ~ parent_destructiveness + (1|video_id),
  dyads_novel,
  "Parent D -> Child C (chilling)"
)
res_D_to_D <- run_beta(
  child_destructiveness_beta ~ parent_destructiveness + (1|video_id),
  dyads_novel,
  "Parent D -> Child D"
)

# Print comparison table
results_list <- list(res_C_to_C, res_C_to_D, res_D_to_C, res_D_to_D)
cat("=== COEFFICIENTS (NOVEL COMMENTERS ONLY) ===\n")
for (r in results_list) {
  cat(sprintf("%s: beta = %.3f (SE = %.3f), 95%% CI [%.3f, %.3f], N = %d\n",
    r$label, r$coef, r$se, r$ci[1], r$ci[2], r$n))
}
cat("\nCompare to main RQ3 (all dyads) to see if propagation holds when excluding same-person replies.\n")

# Save
results <- list(
  dyads_novel_n = nrow(dyads_novel),
  dyads_all_n = nrow(dyads_with_author),
  C_to_C = res_C_to_C,
  C_to_D = res_C_to_D,
  D_to_C = res_D_to_C,
  D_to_D = res_D_to_D
)
saveRDS(results, "analysis/propagation/RQ3_propagation_novel_commenters_only_results.rds")
cat("\n=== RESULTS SAVED ===\n")
