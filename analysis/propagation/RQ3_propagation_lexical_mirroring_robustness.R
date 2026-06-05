# Purpose
# Robustness check for Eli comment #9: show that same-direction propagation in
# the different-person dyad analysis is not reducible to simple lexical
# mirroring (parents and children sharing words). Re-estimate the four beta
# regression models from RQ3_propagation_novel_commenters_only.R, adding a
# parent-child text similarity covariate (Jaccard on lowercase word tokens).
#
# Reference: Supplement (RQ3 robustness)
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)

# Load racial data
source("analysis/setup/load_data.R")

dyad_path <- "data/analysis_objects/racial_parent_child_dyads.rda"
if (!file.exists(dyad_path)) stop("parent_child_data.rda not found.")
load(dyad_path)

# Lookup tables for username + indices, keyed by comment_id.
# The public release uses precomputed parent_child_jaccard in parent_child_data,
# because full comment text is redacted from the public analytic files.
comment_lookup <- joined_data %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, username, harmoniousness_raw, divisiveness_raw)

parent_lookup <- comment_lookup %>%
  rename(parent_comment_id      = comment_id,
         parent_username        = username,
         parent_constructiveness = harmoniousness_raw,
         parent_destructiveness  = divisiveness_raw)
child_lookup <- comment_lookup %>%
  select(-username) %>%
  rename(child_constructiveness = harmoniousness_raw,
         child_destructiveness  = divisiveness_raw)

dyads_with_author <- parent_child_data %>%
  left_join(parent_lookup, by = "parent_comment_id") %>%
  left_join(child_lookup,  by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(parent_constructiveness), !is.na(child_constructiveness),
         !is.na(parent_destructiveness),  !is.na(child_destructiveness)) %>%
  mutate(
    parent_author = trimws(tolower(replace_na(as.character(parent_username), ""))),
    child_author  = trimws(tolower(replace_na(as.character(username),        "")))
  )

# Novel commenters only (different person)
dyads_novel <- dyads_with_author %>%
  filter(parent_author != child_author) %>%
  filter(nchar(parent_author) > 0 | nchar(child_author) > 0)

# Parent-child lexical similarity
if (!"parent_child_jaccard" %in% colnames(dyads_novel)) {
  stop("parent_child_jaccard is not available. The public release omits full text; use the provided public dyad data with precomputed lexical similarity.")
}

jacc <- as.numeric(dyads_novel$parent_child_jaccard)

cat("Jaccard summary:\n"); print(summary(jacc))

# Drop dyads where Jaccard is NA (both texts empty after tokenizing)
dyads_novel <- dyads_novel %>% filter(!is.na(parent_child_jaccard))

# Beta-bounded transform
dyads_novel <- dyads_novel %>%
  mutate(
    child_constructiveness_beta = ifelse(child_constructiveness == 0, 0.0001,
                                  ifelse(child_constructiveness == 1, 0.9999, child_constructiveness)),
    child_destructiveness_beta  = ifelse(child_destructiveness == 0, 0.0001,
                                  ifelse(child_destructiveness == 1, 0.9999, child_destructiveness))
  )

# Sanity checks
cat("\n=== RQ3 LEXICAL MIRRORING ROBUSTNESS (RACIAL) ===\n")
cat("N novel-commenter dyads (with text):", nrow(dyads_novel), "\n")
cat("N unique videos:", length(unique(dyads_novel$video_id)), "\n\n")

# Four beta regressions WITH parent-child Jaccard as a covariate
run_beta_with_jacc <- function(formula, data, label) {
  mod   <- glmmTMB(formula, data = data, family = beta_family())
  coefs <- fixef(mod)$cond
  ses   <- sqrt(diag(vcov(mod)$cond))
  zs    <- coefs / ses
  ps    <- 2 * (1 - pnorm(abs(zs)))
  ci    <- confint(mod)
  cat(sprintf("--- %s (controlling for Jaccard) ---\n", label))
  for (term in names(coefs)) {
    cat(sprintf("  %-30s beta = %8.4f, SE = %.4f, z = %7.3f, p = %.3e\n",
                term, coefs[[term]], ses[[term]], zs[[term]], ps[[term]]))
  }
  cat("\n")
  list(label = label, model = mod, coefs = coefs, ses = ses, zs = zs, ps = ps,
       ci = ci, n = nrow(data))
}

res_C_to_C <- run_beta_with_jacc(
  child_constructiveness_beta ~ parent_constructiveness + parent_child_jaccard + (1 | video_id),
  dyads_novel, "Parent C -> Child C"
)
res_D_to_D <- run_beta_with_jacc(
  child_destructiveness_beta  ~ parent_destructiveness  + parent_child_jaccard + (1 | video_id),
  dyads_novel, "Parent D -> Child D"
)
res_C_to_D <- run_beta_with_jacc(
  child_destructiveness_beta  ~ parent_constructiveness + parent_child_jaccard + (1 | video_id),
  dyads_novel, "Parent C -> Child D"
)
res_D_to_C <- run_beta_with_jacc(
  child_constructiveness_beta ~ parent_destructiveness  + parent_child_jaccard + (1 | video_id),
  dyads_novel, "Parent D -> Child C"
)

# Save
results <- list(
  n              = nrow(dyads_novel),
  n_videos       = length(unique(dyads_novel$video_id)),
  jaccard_summary = summary(jacc[!is.na(jacc)]),
  C_to_C         = res_C_to_C,
  D_to_D         = res_D_to_D,
  C_to_D         = res_C_to_D,
  D_to_C         = res_D_to_C
)
saveRDS(results, "analysis/propagation/RQ3_propagation_lexical_mirroring_robustness_results.rds")
cat("=== RESULTS SAVED ===\n")
