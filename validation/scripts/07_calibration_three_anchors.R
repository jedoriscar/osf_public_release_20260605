# ---------------------------------------------------------------------------
# 07_calibration_three_anchors.R
#
# Repeat the per-feature threshold calibration using three different anchors:
#   (1) RA 1 (Coder1) alone
#   (2) RA 2 (Coder2) alone
#   (3) the average of the two RAs' positive rates (primary anchor for the
#       supplement, since it treats both coders symmetrically)
#
# For each anchor we report the calibrated threshold per feature, full-corpus
# prevalence at that threshold, and the constructive/destructive index means
# and the C/D ratio.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr) })

ROOT <- normalizePath(getwd(), mustWork = TRUE)
DATA_DIR  <- file.path(ROOT, "validation/data")
TABLE_DIR <- file.path(ROOT, "validation/tables")

CONSTRUCTIVE <- c("compassion","curiosity","nuance","personal_story","reasoning")
DESTRUCTIVE  <- c("toxicity","identity_attack","threat","attack_on_author","attack_on_commenter")
FEATURES <- c(CONSTRUCTIVE, DESTRUCTIVE)

feat_val <- read.csv(file.path(DATA_DIR, "merged_features.csv"),
                     stringsAsFactors = FALSE)
prob_val <- function(f) paste0("prob_", f, ".key")

# For each feature, return the threshold that produces a given target positive rate
# on the validation sample.
threshold_for_rate <- function(probs, target_rate) {
  probs <- probs[!is.na(probs)]
  n <- length(probs)
  k <- round(target_rate * n)
  if (k <= 0) return(max(probs) + 1e-6)
  if (k >= n) return(0)
  sort(probs, decreasing = TRUE)[k]
}

# Calibration table for each of the three anchors
calibrate <- function(target_fn, label) {
  rows <- lapply(FEATURES, function(f) {
    p  <- feat_val[[ prob_val(f) ]]
    so <- feat_val[[ paste0(f, "_coder1")  ]]
    sh <- feat_val[[ paste0(f, "_coder2") ]]
    target <- target_fn(so, sh)
    t <- threshold_for_rate(p, target)
    data.frame(feature = f,
               anchor = label,
               target_rate = target,
               calibrated_threshold = t,
               stringsAsFactors = FALSE)
  })
  bind_rows(rows)
}

cal_coder1  <- calibrate(function(s, h) mean(s, na.rm=TRUE),                "Coder1")
cal_coder2 <- calibrate(function(s, h) mean(h, na.rm=TRUE),                "Coder2")
cal_average <- calibrate(function(s, h) mean(c(mean(s, na.rm=TRUE),
                                               mean(h, na.rm=TRUE))),       "Average")

all_cal <- bind_rows(cal_coder1, cal_coder2, cal_average)

# Apply to full corpus
load(file.path(ROOT, "data/analysis_objects/racial_comments.rda"))
canon_prob <- function(f) if (f == "toxicity") "prob_toxic" else paste0("prob_", f)

apply_to_corpus <- function(cal_df) {
  thr <- setNames(cal_df$calibrated_threshold, cal_df$feature)
  c_idx <- rowMeans(sapply(CONSTRUCTIVE,
            function(f) as.numeric(joined_data[[canon_prob(f)]] >= thr[[f]])), na.rm=TRUE)
  d_idx <- rowMeans(sapply(DESTRUCTIVE,
            function(f) as.numeric(joined_data[[canon_prob(f)]] >= thr[[f]])), na.rm=TRUE)
  list(c = c_idx, d = d_idx, thr = thr)
}

ix_coder1  <- apply_to_corpus(cal_coder1)
ix_coder2 <- apply_to_corpus(cal_coder2)
ix_average <- apply_to_corpus(cal_average)

# Also the manuscript's 0.6
ix_manuscript <- list(
  c = rowMeans(sapply(CONSTRUCTIVE,
        function(f) as.numeric(joined_data[[canon_prob(f)]] >= 0.6)), na.rm=TRUE),
  d = rowMeans(sapply(DESTRUCTIVE,
        function(f) as.numeric(joined_data[[canon_prob(f)]] >= 0.6)), na.rm=TRUE)
)

# Summary
make_row <- function(label, ix) {
  mc <- mean(ix$c, na.rm=TRUE); md <- mean(ix$d, na.rm=TRUE)
  data.frame(thresholding = label,
             mean_constructive = mc,
             mean_destructive  = md,
             ratio_C_over_D    = mc / md)
}
ratio_tbl <- bind_rows(
  make_row("Manuscript (all features @ 0.6)", ix_manuscript),
  make_row("Calibrated to Coder1",            ix_coder1),
  make_row("Calibrated to Coder2",           ix_coder2),
  make_row("Calibrated to average (Coder1 & Coder2)", ix_average)
)

cat("\n=== Calibration anchors comparison ===\n\n")

# Per-feature thresholds, wide
thr_wide <- all_cal %>%
  select(feature, anchor, calibrated_threshold) %>%
  tidyr::pivot_wider(names_from = anchor, values_from = calibrated_threshold)
target_wide <- all_cal %>%
  select(feature, anchor, target_rate) %>%
  tidyr::pivot_wider(names_from = anchor, values_from = target_rate,
                     names_prefix = "rate_")

merged <- thr_wide %>% left_join(target_wide, by = "feature") %>%
  mutate(side = ifelse(feature %in% CONSTRUCTIVE, "C", "D"))

print(merged %>% mutate(across(where(is.numeric), ~round(.x, 3))),
      row.names = FALSE)

cat("\n=== Constructive vs Destructive prevalence ratio (N = 101,103) ===\n\n")
print(ratio_tbl %>% mutate(across(where(is.numeric), ~round(.x, 4))),
      row.names = FALSE)

# Write outputs
write.csv(all_cal,  file.path(TABLE_DIR, "calibration_three_anchors_thresholds.csv"),
          row.names = FALSE)
write.csv(ratio_tbl, file.path(TABLE_DIR, "calibration_three_anchors_ratio.csv"),
          row.names = FALSE)

# Save the AVERAGE-anchored indices keyed on comment_id for Agent 1's RQ2/RQ3 reruns
saveRDS(list(
  constructive_calibrated_avg = ix_average$c,
  destructive_calibrated_avg  = ix_average$d,
  thresholds                  = ix_average$thr,
  comment_id                  = joined_data$comment_id
), file.path(TABLE_DIR, "ra_calibrated_indices_avg_anchor.rds"))

cat("\nWrote:\n",
    "  ", file.path(TABLE_DIR, "calibration_three_anchors_thresholds.csv"), "\n",
    "  ", file.path(TABLE_DIR, "calibration_three_anchors_ratio.csv"), "\n",
    "  ", file.path(TABLE_DIR, "ra_calibrated_indices_avg_anchor.rds"), "\n", sep="")
