# Goal (SUPPLEMENTAL_ANALYSES §1.3 + "constructiveness brings users back")
# 1. Build thread depth: (a) timestamp-based (order of reply within thread),
#    (b) tree-based (depth = 1 + depth(parent)).
# 2. Per-thread: does root constructiveness/destructiveness predict max depth
#    or reply count?
# 3. Among disagreement threads (parents with >=1 Disagree reply): does parent
#    C/D predict reply count (or depth)? "Constructiveness brings users back."
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

source("analysis/setup/load_data.R")
dyad_path <- "data/analysis_objects/racial_parent_child_dyads.rda"
load(dyad_path)

# ---- 1a. Timestamp-based depth (within-thread order) ----
pc <- parent_child_data %>%
  mutate(comment_published_at = as.POSIXct(comment_published_at, tz = "UTC", optional = TRUE)) %>%
  filter(!is.na(comment_published_at)) %>%
  group_by(parent_comment_id) %>%
  arrange(comment_published_at) %>%
  mutate(depth_timestamp = row_number()) %>%
  ungroup()

thread_agg <- pc %>%
  group_by(parent_comment_id) %>%
  summarise(n_replies = n(), max_depth_timestamp = max(depth_timestamp, na.rm = TRUE), .groups = "drop")

# ---- Merge root C/D ----
comment_lookup <- joined_data %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw, video_id)
root_lookup <- comment_lookup %>% rename(parent_comment_id = comment_id, root_C = harmoniousness_raw, root_D = divisiveness_raw)
thread_agg <- thread_agg %>%
  left_join(root_lookup %>% select(parent_comment_id, root_C, root_D, video_id), by = "parent_comment_id") %>%
  filter(!is.na(root_C), !is.na(video_id))

# ---- Agreement: parents with >=1 Disagree reply ----
if ("agreement_label" %in% colnames(joined_data)) {
  child_agreement <- joined_data %>% filter(!is.na(comment_id)) %>% distinct(comment_id, .keep_all = TRUE) %>% select(comment_id, agreement_label)
  pc_agr <- pc %>% left_join(child_agreement, by = "comment_id")
  parents_with_disagree <- pc_agr %>% filter(agreement_label == "Disagree") %>% distinct(parent_comment_id) %>% pull(parent_comment_id)
  thread_agg$has_disagree <- thread_agg$parent_comment_id %in% parents_with_disagree
} else { thread_agg$has_disagree <- NA }

cat("=== THREAD DEPTH & CONSTRUCTIVENESS BRINGS USERS BACK ===\n\n")
cat("N threads:", nrow(thread_agg), "\n")
cat("Mean reply count per thread:", round(mean(thread_agg$n_replies), 2), "\n")
cat("Mean max depth (timestamp):", round(mean(thread_agg$max_depth_timestamp, na.rm = TRUE), 2), "\n")
if (!all(is.na(thread_agg$has_disagree))) cat("N threads with >=1 Disagree:", sum(thread_agg$has_disagree, na.rm = TRUE), "\n\n")

# Model 1: Reply count ~ root C + root D
cat("=== MODEL 1: Reply count ~ root C + root D ===\n")
m1 <- glmer.nb(n_replies ~ root_C + root_D + (1|video_id), data = thread_agg,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
print(summary(m1))
cat("Constructiveness IRR:", round(exp(fixef(m1)[2]), 3), "| Destructiveness IRR:", round(exp(fixef(m1)[3]), 3), "\n\n")

# Model 2: Max depth ~ root C + root D
cat("=== MODEL 2: Max depth (timestamp) ~ root C + root D ===\n")
m2 <- glmer.nb(max_depth_timestamp ~ root_C + root_D + (1|video_id), data = thread_agg,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
print(summary(m2))
cat("Constructiveness IRR:", round(exp(fixef(m2)[2]), 3), "| Destructiveness IRR:", round(exp(fixef(m2)[3]), 3), "\n\n")

# Model 3: Disagreement threads only
if (!all(is.na(thread_agg$has_disagree)) && sum(thread_agg$has_disagree, na.rm = TRUE) >= 50) {
  cat("=== MODEL 3: Reply count ~ root C + root D (DISAGREEMENT THREADS ONLY) ===\n")
  disagree_threads <- thread_agg %>% filter(has_disagree == TRUE)
  cat("N disagreement threads:", nrow(disagree_threads), "\n")
  m3 <- tryCatch(glmer.nb(n_replies ~ root_C + root_D + (1|video_id), data = disagree_threads,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))), error = function(e) { cat("Error:", conditionMessage(e), "\n"); NULL })
  if (!is.null(m3)) {
    print(summary(m3))
    cat("Constructiveness IRR (disagreement):", round(exp(fixef(m3)[2]), 3), "| Destructiveness IRR:", round(exp(fixef(m3)[3]), 3), "\n")
    cat("If C IRR > 1: constructive parents get more replies even when someone disagreed.\n")
  }
} else { m3 <- NULL; cat("Skipping disagreement model (no agreement_label or N < 50).\n") }

results <- list(thread_agg_n = nrow(thread_agg), mod_reply_count = m1, mod_max_depth = m2, mod_disagreement = m3)
saveRDS(results, "analysis/propagation/RQ3_thread_depth_and_engagement_results.rds")
cat("\nSaved RQ3_thread_depth_and_engagement_results.rds\n")
