# Purpose
# Test whether "more replies for constructive comments" is driven by more
# *unique* repliers vs. the same person replying repeatedly. Same negbinom
# specification as RQ2_replies_negbinom.R but with DV = number of unique repliers
# (by username) per comment. If constructiveness still predicts unique repliers,
# the effect is not just same-person verbosity.
#
# Supplemental: SUPPLEMENTAL_ANALYSES_TO_RUN.md §2.3
# Data: parent_child_data for reply-level replier identity; joined_data for C/D and video.

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

# Load data
source("analysis/setup/load_data.R")

dyad_path <- "data/analysis_objects/racial_parent_child_dyads.rda"
if (!file.exists(dyad_path)) stop("parent_child_data.rda not found.")
load(dyad_path)

# Per-parent counts from dyads: reply count and unique repliers (by username)
replier_agg <- parent_child_data %>%
  filter(!is.na(username), trimws(as.character(username)) != "") %>%
  group_by(parent_comment_id) %>%
  summarise(
    n_reply_rows = n(),
    n_unique_repliers = n_distinct(trimws(tolower(as.character(username)))),
    .groups = "drop"
  )

# YouTube comments: merge unique replier count (0 if comment has no replies in our dyad data)
analysis_data <- joined_data %>%
  filter(platform == "YouTube" | is.na(platform)) %>%
  filter(!is.na(comment_id)) %>%
  left_join(
    replier_agg %>% rename(comment_id = parent_comment_id),
    by = "comment_id"
  ) %>%
  mutate(
    n_unique_repliers = replace_na(n_unique_repliers, 0L),
    n_reply_rows = replace_na(n_reply_rows, 0L)
  )

# Sanity checks
cat("=== RQ2 UNIQUE REPLIERS: DATA CHECKS ===\n")
cat("N comments (YouTube):", nrow(analysis_data), "\n")
cat("N with at least 1 reply (in dyad data):", sum(analysis_data$n_reply_rows >= 1, na.rm = TRUE), "\n")
cat("Mean unique repliers:", mean(analysis_data$n_unique_repliers, na.rm = TRUE), "\n")
cat("Median unique repliers:", median(analysis_data$n_unique_repliers, na.rm = TRUE), "\n")
cat("Correlation reply_count vs n_unique_repliers (for comments with replies):\n")
sub <- analysis_data %>% filter(n_reply_rows >= 1)
if (nrow(sub) > 0) {
  cat("  cor(reply_count, n_unique_repliers):", cor(sub$reply_count, sub$n_unique_repliers, use = "pairwise.complete.obs"), "\n")
  cat("  Mean reply_count vs mean n_unique_repliers:", mean(sub$reply_count, na.rm = TRUE), "vs", mean(sub$n_unique_repliers, na.rm = TRUE), "\n\n")
}

# Model: Unique repliers ~ C + D (same spec as main replies model)
cat("=== MODEL: UNIQUE REPLIERS ~ CONSTRUCTIVENESS + DESTRUCTIVENESS ===\n")

mod_unique <- glmer.nb(
  n_unique_repliers ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

summary(mod_unique)
ci_unique <- confint(mod_unique, method = "Wald")

coefs <- fixef(mod_unique)
ses <- sqrt(diag(vcov(mod_unique)))
irr <- exp(coefs)
ci_irr <- exp(coefs + outer(ses, c(-1.96, 1.96)))

cat("\n=== KEY RESULTS (UNIQUE REPLIERS) ===\n")
cat(sprintf("Constructiveness: IRR = %.3f, 95%% CI [%.3f, %.3f]\n", irr[2], ci_irr[2,1], ci_irr[2,2]))
cat(sprintf("Destructiveness:  IRR = %.3f, 95%% CI [%.3f, %.3f]\n", irr[3], ci_irr[3,1], ci_irr[3,2]))
cat("Compare to main RQ2 replies model: if IRRs are similar, reply advantage is not driven by same person replying repeatedly.\n\n")

# Optional: same model on count of replies from NOVEL repliers only (exclude self-replies)
# Requires parent username: add to parent_child_data via join, then filter to replier != parent_author, then aggregate.
parent_username_lookup <- joined_data %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, username) %>%
  rename(parent_comment_id = comment_id, parent_username = username)

dyads_with_parent_author <- parent_child_data %>%
  left_join(parent_username_lookup, by = "parent_comment_id") %>%
  mutate(
    parent_author = trimws(tolower(replace_na(as.character(parent_username), ""))),
    child_author = trimws(tolower(replace_na(as.character(username), "")))
  )

# Count replies from novel repliers only (replier != parent author) per parent
novel_reply_agg <- dyads_with_parent_author %>%
  filter(parent_author != child_author | parent_author == "" | child_author == "") %>%
  group_by(parent_comment_id) %>%
  summarise(n_novel_replies = n(), .groups = "drop")

analysis_data2 <- analysis_data %>%
  left_join(novel_reply_agg %>% rename(comment_id = parent_comment_id), by = "comment_id") %>%
  mutate(n_novel_replies = replace_na(n_novel_replies, 0L))

cat("=== MODEL: NOVEL-REPLIER COUNT ONLY (exclude self-replies) ===\n")
mod_novel <- tryCatch(
  glmer.nb(
    n_novel_replies ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
    data = analysis_data2,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  ),
  error = function(e) { cat("Model error:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(mod_novel)) {
  summary(mod_novel)
  coefs_n <- fixef(mod_novel)
  irr_n <- exp(coefs_n)
  cat(sprintf("Constructiveness: IRR = %.3f\n", irr_n[2]))
  cat(sprintf("Destructiveness:  IRR = %.3f\n", irr_n[3]))
}

# Save
results <- list(
  model_unique_repliers = mod_unique,
  model_novel_replies = if (exists("mod_novel") && !is.null(mod_novel)) mod_novel else NULL,
  irr_constructiveness_unique = irr[2],
  irr_destructiveness_unique = irr[3],
  n = nrow(analysis_data)
)
saveRDS(results, "analysis/engagement/RQ2_replies_unique_repliers_results.rds")
cat("\n=== RESULTS SAVED ===\n")
