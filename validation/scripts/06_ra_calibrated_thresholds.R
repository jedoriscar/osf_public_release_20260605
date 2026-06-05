# ---------------------------------------------------------------------------
# 06_ra_calibrated_thresholds.R
#
# Step 1: From the validation sample (298 double-coded comments), find a
#         per-feature threshold t_f such that the model's positive rate
#         (count of comments with prob_f >= t_f) matches Coder1's positive
#         count.
#
# Step 2: Apply those 10 RA-calibrated thresholds to the full 101,103-comment
#         corpus, rebuild constructive and destructive indices using the
#         same proportion-of-5 rule the manuscript uses, and compare to
#         the original 0.6-threshold indices.
#
# Outputs:
#   - outputs/tables/ra_calibrated_thresholds.csv   (per-feature thresholds)
#   - outputs/tables/ra_calibrated_index_summary.csv (full-corpus comparison)
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

ROOT <- normalizePath(getwd(), mustWork = TRUE)
DATA_DIR  <- file.path(ROOT, "validation/data")
TABLE_DIR <- file.path(ROOT, "validation/tables")

CONSTRUCTIVE <- c("compassion","curiosity","nuance","personal_story","reasoning")
DESTRUCTIVE  <- c("toxicity","identity_attack","threat","attack_on_author","attack_on_commenter")
FEATURES <- c(CONSTRUCTIVE, DESTRUCTIVE)

# ---------------------------------------------------------------------------
# Step 1: derive per-feature RA-calibrated thresholds
# ---------------------------------------------------------------------------
feat_val <- read.csv(file.path(DATA_DIR, "merged_features.csv"),
                     stringsAsFactors = FALSE)
prob_col_val <- function(f) paste0("prob_", f, ".key")  # validation copy

# For each feature, find the threshold t such that mean(prob >= t) on the
# validation sample matches Coder1's positive rate. We use Coder1 rather
# than averaging RAs because (a) she completed the task slightly more fully
# and (b) this script uses Coder1 as the single-coder anchor.
calibrate <- function(f) {
  p     <- feat_val[[ prob_col_val(f) ]]
  so    <- feat_val[[ paste0(f, "_coder1") ]]
  ok    <- !is.na(p) & !is.na(so)
  p     <- p[ok];  so <- so[ok]
  n     <- length(p)
  k_so  <- sum(so == 1)
  prev_coder1 <- k_so / n

  # Pick the threshold = (k_so)-th highest probability in the validation
  # sample. This guarantees the model fires on exactly k_so comments here.
  if (k_so == 0) {
    # Coder1 never marked it positive; pick a threshold above the max prob
    t <- max(p) + 1e-6
  } else if (k_so >= n) {
    t <- 0
  } else {
    sorted_desc <- sort(p, decreasing = TRUE)
    t <- sorted_desc[k_so]   # smallest prob still flagged as positive
  }

  prev_model_at_06    <- mean(p >= 0.6)
  prev_model_at_calib <- mean(p >= t)

  data.frame(
    feature = f,
    side = ifelse(f %in% CONSTRUCTIVE, "constructive", "destructive"),
    coder1_prev_val = prev_coder1,
    prev_model_val_at_0.6 = prev_model_at_06,
    calibrated_threshold = t,
    prev_model_val_at_calib = prev_model_at_calib,
    stringsAsFactors = FALSE
  )
}

thresholds <- bind_rows(lapply(FEATURES, calibrate))
write.csv(thresholds,
          file.path(TABLE_DIR, "ra_calibrated_thresholds.csv"),
          row.names = FALSE)

cat("\n=== Per-feature RA-calibrated thresholds (anchored to Coder1) ===\n\n")
print(thresholds %>%
        mutate(across(where(is.numeric), ~round(.x, 3))),
      row.names = FALSE)

# ---------------------------------------------------------------------------
# Step 2: apply calibrated thresholds to full corpus, rebuild indices
# ---------------------------------------------------------------------------
load(file.path(ROOT, "data/analysis_objects/racial_comments.rda"))

# joined_data uses prob_toxic (not prob_toxicity); rename for uniform lookup
canon_prob_col <- function(f) {
  if (f == "toxicity") "prob_toxic" else paste0("prob_", f)
}

# Sanity: original constructiveness/destructiveness at 0.6 should match
# the canonical harmoniousness_raw / divisiveness_raw (we verified this).
constr_06 <- rowMeans(sapply(CONSTRUCTIVE,
                       function(f) as.numeric(joined_data[[canon_prob_col(f)]] >= 0.6)),
                      na.rm = TRUE)
destr_06  <- rowMeans(sapply(DESTRUCTIVE,
                       function(f) as.numeric(joined_data[[canon_prob_col(f)]] >= 0.6)),
                      na.rm = TRUE)

# RA-calibrated binaries
thr <- setNames(thresholds$calibrated_threshold, thresholds$feature)
constr_cal <- rowMeans(sapply(CONSTRUCTIVE,
                       function(f) as.numeric(joined_data[[canon_prob_col(f)]] >= thr[[f]])),
                       na.rm = TRUE)
destr_cal  <- rowMeans(sapply(DESTRUCTIVE,
                       function(f) as.numeric(joined_data[[canon_prob_col(f)]] >= thr[[f]])),
                       na.rm = TRUE)

# Per-feature population prevalence at 0.6 vs at calibrated
per_feat <- bind_rows(lapply(FEATURES, function(f) {
  p <- joined_data[[ canon_prob_col(f) ]]
  data.frame(
    feature = f,
    side = ifelse(f %in% CONSTRUCTIVE, "constructive", "destructive"),
    pop_prev_at_0.6  = mean(p >= 0.6, na.rm = TRUE),
    pop_prev_at_calib = mean(p >= thr[[f]], na.rm = TRUE),
    calibrated_threshold = thr[[f]]
  )
}))

cat("\n\n=== Population prevalence per feature (N = 101,103) ===\n\n")
print(per_feat %>% mutate(across(where(is.numeric), ~round(.x, 3))),
      row.names = FALSE)

# Index-level summary
summarise_idx <- function(x, name) {
  data.frame(
    index = name,
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd   = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    pct_any = mean(x > 0, na.rm = TRUE)
  )
}
idx_summary <- bind_rows(
  summarise_idx(constr_06,  "constructive @ 0.6 (original)"),
  summarise_idx(constr_cal, "constructive @ RA-calibrated"),
  summarise_idx(destr_06,   "destructive  @ 0.6 (original)"),
  summarise_idx(destr_cal,  "destructive  @ RA-calibrated")
)

cat("\n\n=== Index-level summary on the full corpus ===\n\n")
print(idx_summary %>% mutate(across(where(is.numeric), ~round(.x, 4))),
      row.names = FALSE)

# Prevalence-ratio comparison
ratio_table <- data.frame(
  thresholding = c("original (all features @ 0.6)", "RA-calibrated (per-feature)"),
  mean_constructive = c(mean(constr_06,  na.rm = TRUE),
                        mean(constr_cal, na.rm = TRUE)),
  mean_destructive  = c(mean(destr_06,  na.rm = TRUE),
                        mean(destr_cal, na.rm = TRUE))
) %>% mutate(ratio_C_over_D = mean_constructive / mean_destructive)

cat("\n\n=== Constructive vs Destructive prevalence ratio ===\n\n")
print(ratio_table %>% mutate(across(where(is.numeric), ~round(.x, 4))),
      row.names = FALSE)

# t test on the calibrated indices, mirroring the manuscript's main test
tt <- t.test(constr_cal, destr_cal, paired = TRUE)
cat(sprintf("\nPaired t-test (calibrated) constructive vs destructive:\n  t(%d) = %.2f, p = %s, mean diff = %.4f\n",
            tt$parameter, tt$statistic,
            format.pval(tt$p.value, digits = 3), tt$estimate))

# Original (for comparison)
tt0 <- t.test(constr_06, destr_06, paired = TRUE)
cat(sprintf("Paired t-test (original 0.6)  constructive vs destructive:\n  t(%d) = %.2f, p = %s, mean diff = %.4f\n",
            tt0$parameter, tt0$statistic,
            format.pval(tt0$p.value, digits = 3), tt0$estimate))

# Correlation between original and calibrated indices
cat("\n\n=== Correlation between original and RA-calibrated indices ===\n")
cat(sprintf("  Constructive: r = %.3f (Pearson), rho = %.3f (Spearman)\n",
            cor(constr_06,  constr_cal, use = "complete.obs"),
            cor(constr_06,  constr_cal, use = "complete.obs", method = "spearman")))
cat(sprintf("  Destructive:  r = %.3f (Pearson), rho = %.3f (Spearman)\n",
            cor(destr_06,   destr_cal,  use = "complete.obs"),
            cor(destr_06,   destr_cal,  use = "complete.obs", method = "spearman")))

# Save outputs
write.csv(per_feat,    file.path(TABLE_DIR, "ra_calibrated_per_feature_population.csv"), row.names = FALSE)
write.csv(idx_summary, file.path(TABLE_DIR, "ra_calibrated_index_summary.csv"),         row.names = FALSE)
write.csv(ratio_table, file.path(TABLE_DIR, "ra_calibrated_prevalence_ratio.csv"),      row.names = FALSE)

# Save the full-corpus index values so Agent 1 can rerun the RQ models against them
saveRDS(list(
  constructive_calibrated = constr_cal,
  destructive_calibrated  = destr_cal,
  thresholds              = thr,
  comment_id              = joined_data$comment_id
), file.path(TABLE_DIR, "ra_calibrated_indices_full_corpus.rds"))

cat("\n\nWrote:\n",
    "  ", file.path(TABLE_DIR, "ra_calibrated_thresholds.csv"), "\n",
    "  ", file.path(TABLE_DIR, "ra_calibrated_per_feature_population.csv"), "\n",
    "  ", file.path(TABLE_DIR, "ra_calibrated_index_summary.csv"), "\n",
    "  ", file.path(TABLE_DIR, "ra_calibrated_prevalence_ratio.csv"), "\n",
    "  ", file.path(TABLE_DIR, "ra_calibrated_indices_full_corpus.rds"), "\n", sep = "")
