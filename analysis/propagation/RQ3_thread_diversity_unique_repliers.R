# Goal (SUPPLEMENTAL_ANALYSES §1.2 — diversity of commenters in a thread)
# Does thread "diversity" (number of unique repliers) differ for constructive vs
# destructive parent comments? Reuse per-parent unique-replier count from RQ2;
# merge parent C/D and run model: n_unique_repliers ~ parent_C + parent_D + (1|video_id).
# Also compare mean unique repliers for high-C vs high-D parents (median split).
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

source("analysis/setup/load_data.R")

dyad_path <- "data/analysis_objects/racial_parent_child_dyads.rda"
if (!file.exists(dyad_path)) stop("parent_child_data.rda not found.")
load(dyad_path)

# Per-parent unique replier count (same as RQ2_replies_unique_repliers.R)
replier_agg <- parent_child_data %>%
  filter(!is.na(username), trimws(as.character(username)) != "") %>%
  group_by(parent_comment_id) %>%
  summarise(
    n_reply_rows = n(),
    n_unique_repliers = n_distinct(trimws(tolower(as.character(username)))),
    .groups = "drop"
  )

# Merge parent C/D and video_id
comment_lookup <- joined_data %>%
  filter(!is.na(comment_id)) %>%
  distinct(comment_id, .keep_all = TRUE) %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw, video_id) %>%
  rename(parent_comment_id = comment_id, parent_C = harmoniousness_raw, parent_D = divisiveness_raw)

thread_data <- replier_agg %>%
  left_join(comment_lookup, by = "parent_comment_id") %>%
  filter(!is.na(parent_C), !is.na(parent_D), !is.na(video_id)) %>%
  mutate(n_unique_repliers = replace_na(n_unique_repliers, 0L), n_reply_rows = replace_na(n_reply_rows, 0L))

# Restrict to YouTube if needed (optional; RQ2 uses YouTube)
# thread_data <- thread_data %>% filter(joined_data$platform[ match(thread_data$parent_comment_id, joined_data$comment_id) ] == "YouTube")
# Simpler: keep all that have video_id (thread_data doesn't have platform; we'd need to merge)
cat("=== THREAD DIVERSITY: UNIQUE REPLIERS ~ PARENT C/D ===\n\n")
cat("N parent comments (threads):", nrow(thread_data), "\n")
cat("Mean unique repliers per thread:", round(mean(thread_data$n_unique_repliers, na.rm = TRUE), 2), "\n")
cat("Median unique repliers:", median(thread_data$n_unique_repliers, na.rm = TRUE), "\n\n")

# Model: same as RQ2 unique-repliers model
mod <- glmer.nb(
  n_unique_repliers ~ parent_C + parent_D + (1|video_id),
  data = thread_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
print(summary(mod))
coefs <- fixef(mod)
ses <- sqrt(diag(vcov(mod)))
irr <- exp(coefs)
cat("\nIRRs: Constructiveness", round(irr["parent_C"], 3), "; Destructiveness", round(irr["parent_D"], 3), "\n")

# Comparison: high-C vs high-D parents (median split)
med_C <- median(thread_data$parent_C, na.rm = TRUE)
med_D <- median(thread_data$parent_D, na.rm = TRUE)
thread_data <- thread_data %>%
  mutate(
    high_C = parent_C > med_C,
    high_D = parent_D > med_D
  )
high_C_threads <- thread_data %>% filter(high_C)
high_D_threads <- thread_data %>% filter(high_D)
cat("\n=== HIGH-C vs HIGH-D PARENTS (median split) ===\n")
cat("High-C threads: N =", nrow(high_C_threads), ", mean unique repliers =", round(mean(high_C_threads$n_unique_repliers), 2), "\n")
cat("High-D threads: N =", nrow(high_D_threads), ", mean unique repliers =", round(mean(high_D_threads$n_unique_repliers), 2), "\n")
tt <- t.test(high_C_threads$n_unique_repliers, high_D_threads$n_unique_repliers)
cat("t-test (mean unique repliers, high-C vs high-D): t =", round(tt$statistic, 2), ", p =", format.pval(tt$p.value, digits = 2), "\n")

# Save
results <- list(
  model = mod,
  irr_C = as.numeric(irr["parent_C"]),
  irr_D = as.numeric(irr["parent_D"]),
  n_threads = nrow(thread_data),
  mean_unique_repliers_high_C = mean(high_C_threads$n_unique_repliers),
  mean_unique_repliers_high_D = mean(high_D_threads$n_unique_repliers),
  t_test = tt
)
saveRDS(results, "analysis/propagation/RQ3_thread_diversity_unique_repliers_results.rds")
cat("\n=== RESULTS SAVED ===\n")
