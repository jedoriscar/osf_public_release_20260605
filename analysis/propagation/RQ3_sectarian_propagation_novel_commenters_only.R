# Purpose
# Re-run Models 13 (child alienation), 14 (child fearmongering), and 15 (child
# scapegoating) on the PRIMARY different-person dyad subset (N = 17,370), so
# the SI is internally consistent with the propagation block (Models 9-12),
# which now leads with different-person dyads.
#
# Outcomes are binary indicators (prob >= 0.6 threshold) for whether the child
# comment expresses the corresponding sectarian-discourse pattern.
# Predictors: parent_constructiveness and parent_destructiveness.
# Random intercepts for video_id.
rm(list = ls())
source("analysis/setup/load_data.R")
joined_racial <- joined_data
rm(joined_data)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(lmerTest)
})

# Build different-person dyad dataframe with parent C/D AND child binary
# indicators for alienation, fearmongering, scapegoating
load("data/analysis_objects/racial_parent_child_dyads.rda")

parent_username_lookup <- joined_racial %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, username) %>%
  rename(parent_comment_id = comment_id, parent_username = username)

comment_lookup <- joined_racial %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id,
         harmoniousness_raw, divisiveness_raw,
         prob_alienation, prob_fearmongering, prob_scapegoating)

parent_lookup <- comment_lookup %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw) %>%
  rename(parent_comment_id = comment_id,
         parent_constructiveness = harmoniousness_raw,
         parent_destructiveness  = divisiveness_raw)

child_lookup <- comment_lookup %>%
  select(comment_id,
         child_prob_alienation     = prob_alienation,
         child_prob_fearmongering  = prob_fearmongering,
         child_prob_scapegoating   = prob_scapegoating)

dyads_with_author <- parent_child_data %>%
  left_join(parent_username_lookup, by = "parent_comment_id") %>%
  left_join(parent_lookup,           by = "parent_comment_id",
            relationship = "many-to-one") %>%
  left_join(child_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(parent_constructiveness), !is.na(parent_destructiveness),
         !is.na(child_prob_alienation),
         !is.na(child_prob_fearmongering),
         !is.na(child_prob_scapegoating)) %>%
  mutate(
    parent_author = trimws(tolower(replace_na(as.character(parent_username), ""))),
    child_author  = trimws(tolower(replace_na(as.character(username),        "")))
  )

dyads_novel <- dyads_with_author %>%
  filter(parent_author != child_author) %>%
  filter(nchar(parent_author) > 0 | nchar(child_author) > 0) %>%
  mutate(
    child_alienation_bin     = as.numeric(child_prob_alienation    >= 0.6),
    child_fearmongering_bin  = as.numeric(child_prob_fearmongering >= 0.6),
    child_scapegoating_bin   = as.numeric(child_prob_scapegoating  >= 0.6)
  )

cat("\n=== RQ3 SECTARIAN PROPAGATION (NOVEL COMMENTERS ONLY) ===\n")
cat("N different-person dyads:", nrow(dyads_novel), "\n")
cat("N unique videos:", length(unique(dyads_novel$video_id)), "\n")
cat("\nBase rates of binary child outcomes (prob >= 0.6):\n")
cat(sprintf("  Alienation:    %.3f (%d / %d)\n",
            mean(dyads_novel$child_alienation_bin),
            sum(dyads_novel$child_alienation_bin), nrow(dyads_novel)))
cat(sprintf("  Fearmongering: %.3f (%d / %d)\n",
            mean(dyads_novel$child_fearmongering_bin),
            sum(dyads_novel$child_fearmongering_bin), nrow(dyads_novel)))
cat(sprintf("  Scapegoating:  %.3f (%d / %d)\n",
            mean(dyads_novel$child_scapegoating_bin),
            sum(dyads_novel$child_scapegoating_bin), nrow(dyads_novel)))

# Helper: fit one logistic MLM and report parent_C and parent_D coefficients
run_sectarian <- function(outcome_var, data, label) {
  cat(sprintf("\n--- Model: Parent C, D -> Child %s ---\n", label))
  form <- as.formula(paste0(
    outcome_var, " ~ parent_constructiveness + parent_destructiveness + (1|video_id)"
  ))
  mod  <- glmer(form, data = data, family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 100000)))
  coefs <- fixef(mod)
  ses   <- sqrt(diag(vcov(mod)))
  zs    <- coefs / ses
  ps    <- 2 * (1 - pnorm(abs(zs)))
  ors   <- exp(coefs)
  cis   <- exp(confint(mod, method = "Wald"))
  for (term in c("parent_constructiveness", "parent_destructiveness")) {
    cat(sprintf(
      "  %s: LO = %+.4f, SE = %.4f, z = %+.3f, p = %.4f, OR = %.3f, 95%% CI [%.3f, %.3f]\n",
      term, coefs[[term]], ses[[term]], zs[[term]], ps[[term]],
      ors[[term]], cis[term, 1], cis[term, 2]
    ))
  }
  list(
    label  = label,
    model  = mod,
    coefs  = coefs, ses = ses, zs = zs, ps = ps,
    ors    = ors,   cis = cis,
    n      = nrow(data)
  )
}

res_alienation    <- run_sectarian("child_alienation_bin",    dyads_novel, "Alienation")
res_fearmongering <- run_sectarian("child_fearmongering_bin", dyads_novel, "Fearmongering")
res_scapegoating  <- run_sectarian("child_scapegoating_bin",  dyads_novel, "Scapegoating")

# Save
out <- list(
  n_dyads        = nrow(dyads_novel),
  n_videos       = length(unique(dyads_novel$video_id)),
  alienation     = res_alienation,
  fearmongering  = res_fearmongering,
  scapegoating   = res_scapegoating
)
saveRDS(out,
        "analysis/propagation/RQ3_sectarian_propagation_novel_commenters_only_results.rds")
cat("\n=== RESULTS SAVED ===\n")
