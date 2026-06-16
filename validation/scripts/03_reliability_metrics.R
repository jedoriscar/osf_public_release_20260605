# ---------------------------------------------------------------------------
# 03_reliability_metrics.R
#
# Compute reliability statistics for:
#   (A) the 10 binary discourse features
#         - RA vs RA  (Coder1 x Coder2)
#         - RA vs model binary label (*_ML from CODING_ANSWER_KEY)
#         - RA binary vs model probability (point-biserial correlation, AUC)
#         - composite "constructiveness" (sum / mean of the 5 constructive
#           features) and composite "destructiveness" (5 destructive features),
#           comparing RA composite to model composite (ICC + Pearson r)
#   (B) stance (pro / anti / neutral)
#   (C) agreement in parent-child dyads, reported for the full four-category
#       task and for the clear agree/disagree subset used in the manuscript
#
# Metrics used:
#   - Binary features:  percent agreement, Cohen's kappa, Krippendorff's alpha
#                       For the model-vs-RA comparison we also compute
#                       point-biserial r between the human binary code and
#                       the model probability, and AUC of the probability
#                       in predicting the RA's binary code.
#   - Composite indices: ICC(3,1) (treating RA and model as fixed raters),
#                        Pearson r.
#   - Stance / Agreement: percent agreement, Cohen's kappa, confusion matrix,
#                         per-class precision/recall/F1, macro-F1.
#
# Cronbach's alpha is *not* used here. It is a measure of internal consistency
# of items that share a common factor and is not the right tool for inter-rater
# agreement on nominal labels.
#
# All Ns are reported explicitly so downstream readers can see how much each
# statistic rests on.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(irr)     # kappa2, kripp.alpha, icc
  library(pROC)    # AUC
})

ROOT <- normalizePath(getwd(), mustWork = TRUE)
DATA_DIR  <- file.path(ROOT, "validation/data")
TABLE_DIR <- file.path(ROOT, "validation/tables")
dir.create(TABLE_DIR, showWarnings = FALSE, recursive = TRUE)

LOG <- file.path(TABLE_DIR, "reliability_log.txt")
log_lines <- c()
log <- function(...) {
  msg <- paste0(..., collapse = "")
  log_lines[[length(log_lines) + 1]] <<- msg
  cat(msg, "\n")
}

FEATURES <- c("compassion","curiosity","nuance","personal_story","reasoning",
              "toxicity","identity_attack","threat","attack_on_author",
              "attack_on_commenter")
CONSTRUCTIVE <- FEATURES[1:5]
DESTRUCTIVE  <- FEATURES[6:10]

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
binary_agreement <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  n  <- sum(ok)
  if (n == 0) return(list(n = 0, pct = NA, kappa = NA, alpha = NA))
  pct <- mean(a[ok] == b[ok])
  # kappa2 needs a 2-col matrix/df with no NAs
  m <- cbind(a[ok], b[ok])
  k <- tryCatch(kappa2(m)$value, error = function(e) NA_real_)
  alp <- tryCatch(kripp.alpha(t(m), method = "nominal")$value,
                  error = function(e) NA_real_)
  list(n = n, pct = pct, kappa = k, alpha = alp)
}

nominal_agreement <- function(a, b) binary_agreement(a, b)

confusion_table <- function(a, b, levels = NULL) {
  ok <- !is.na(a) & !is.na(b)
  if (!is.null(levels)) {
    a <- factor(a, levels = levels)
    b <- factor(b, levels = levels)
  }
  table(a[ok], b[ok])
}

per_class_prf <- function(true, pred, classes) {
  out <- data.frame(class = classes, n_true = NA_integer_,
                    precision = NA_real_, recall = NA_real_, f1 = NA_real_)
  for (i in seq_along(classes)) {
    cl <- classes[i]
    tp <- sum(true == cl & pred == cl, na.rm = TRUE)
    fp <- sum(true != cl & pred == cl, na.rm = TRUE)
    fn <- sum(true == cl & pred != cl, na.rm = TRUE)
    prec <- if (tp + fp == 0) NA_real_ else tp / (tp + fp)
    rec  <- if (tp + fn == 0) NA_real_ else tp / (tp + fn)
    f1   <- if (is.na(prec) || is.na(rec) || (prec + rec) == 0) NA_real_
            else 2 * prec * rec / (prec + rec)
    out$n_true[i]    <- sum(true == cl, na.rm = TRUE)
    out$precision[i] <- prec
    out$recall[i]    <- rec
    out$f1[i]        <- f1
  }
  out$macro_f1 <- mean(out$f1, na.rm = TRUE)
  out
}

# ---------------------------------------------------------------------------
# (A) FEATURES
# ---------------------------------------------------------------------------
feat <- read.csv(file.path(DATA_DIR, "merged_features.csv"), stringsAsFactors = FALSE)
log("=== (A) FEATURE reliability ===")
log("Merged file rows: ", nrow(feat))
log("")

# A.1 RA vs RA
log("-- A.1 RA-vs-RA agreement, per feature --")
rara <- lapply(FEATURES, function(f) {
  a <- feat[[paste0(f, "_coder1")]]
  b <- feat[[paste0(f, "_coder2")]]
  res <- binary_agreement(a, b)
  data.frame(feature = f, comparison = "Coder1_vs_Coder2",
             n = res$n, pct_agreement = res$pct,
             cohens_kappa = res$kappa, krippendorff_alpha = res$alpha,
             prevalence_coder1  = mean(a, na.rm = TRUE),
             prevalence_coder2 = mean(b, na.rm = TRUE))
}) %>% bind_rows()
print(rara, row.names = FALSE)

# A.2 RA vs model (binary)
ml_binary_col <- function(f) {
  # Prefer the *_ML column from the answer-key join
  paste0(f, "_ML")
}
rama <- lapply(FEATURES, function(f) {
  ml <- feat[[ml_binary_col(f)]]
  if (is.null(ml)) return(NULL)
  out <- bind_rows(
    {
      r <- binary_agreement(feat[[paste0(f, "_coder1")]], ml)
      data.frame(feature = f, ra = "Coder1",
                 n = r$n, pct_agreement = r$pct,
                 cohens_kappa = r$kappa, krippendorff_alpha = r$alpha,
                 ra_prevalence = mean(feat[[paste0(f, "_coder1")]], na.rm = TRUE),
                 model_prevalence = mean(ml, na.rm = TRUE))
    },
    {
      r <- binary_agreement(feat[[paste0(f, "_coder2")]], ml)
      data.frame(feature = f, ra = "Coder2",
                 n = r$n, pct_agreement = r$pct,
                 cohens_kappa = r$kappa, krippendorff_alpha = r$alpha,
                 ra_prevalence = mean(feat[[paste0(f, "_coder2")]], na.rm = TRUE),
                 model_prevalence = mean(ml, na.rm = TRUE))
    }
  )
  out
}) %>% bind_rows()
log("\n-- A.2 RA-vs-model binary agreement --")
print(rama, row.names = FALSE)

# A.3 RA binary vs model probability: point-biserial r + AUC
prob_col <- function(f) paste0("prob_", f, ".key")  # canonical & key probs are identical
if (!all(sapply(FEATURES, function(f) paste0("prob_", f, ".key") %in% names(feat)))) {
  # fallback when no canonical join happened
  prob_col <- function(f) paste0("prob_", f)
}

rama_prob <- lapply(FEATURES, function(f) {
  p <- feat[[prob_col(f)]]
  if (is.null(p)) return(NULL)
  bind_rows(
    lapply(c("Coder1","Coder2"), function(ra) {
      ra_col <- paste0(f, "_", tolower(ra))
      y <- feat[[ra_col]]
      ok <- !is.na(p) & !is.na(y)
      n <- sum(ok)
      if (n < 5) {
        return(data.frame(feature = f, ra = ra, n = n,
                          point_biserial_r = NA_real_, auc = NA_real_))
      }
      r <- suppressWarnings(cor(p[ok], y[ok]))
      auc <- NA_real_
      if (length(unique(y[ok])) == 2) {
        auc <- tryCatch(as.numeric(auc(roc(y[ok], p[ok], quiet = TRUE,
                                           levels = c(0,1), direction = "<"))),
                        error = function(e) NA_real_)
      }
      data.frame(feature = f, ra = ra, n = n,
                 point_biserial_r = r, auc = auc)
    })
  )
}) %>% bind_rows()
log("\n-- A.3 RA binary vs. model probability --")
print(rama_prob, row.names = FALSE)

# A.4 Composite indices
mk_composite <- function(df, who, set) {
  mat <- as.matrix(df[, paste0(set, "_", who)])
  # Composite = mean of available features for the comment (ignores NA per cell)
  rowMeans(mat, na.rm = TRUE)
}
feat$construct_coder1  <- mk_composite(feat, "coder1",  CONSTRUCTIVE)
feat$construct_coder2 <- mk_composite(feat, "coder2", CONSTRUCTIVE)
feat$destruct_coder1   <- mk_composite(feat, "coder1",  DESTRUCTIVE)
feat$destruct_coder2  <- mk_composite(feat, "coder2", DESTRUCTIVE)

ml_construct <- rowMeans(sapply(CONSTRUCTIVE, function(f) feat[[paste0(f,"_ML")]]),
                         na.rm = TRUE)
ml_destruct  <- rowMeans(sapply(DESTRUCTIVE,  function(f) feat[[paste0(f,"_ML")]]),
                         na.rm = TRUE)
feat$construct_ML <- ml_construct
feat$destruct_ML  <- ml_destruct

# Continuous model "intensity" composite = mean of the probabilities
construct_prob_cols <- sapply(CONSTRUCTIVE, prob_col)
destruct_prob_cols  <- sapply(DESTRUCTIVE,  prob_col)
feat$construct_prob <- rowMeans(feat[, construct_prob_cols], na.rm = TRUE)
feat$destruct_prob  <- rowMeans(feat[, destruct_prob_cols],  na.rm = TRUE)

icc_pair <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 5) return(c(n = sum(ok), icc = NA_real_, r = NA_real_))
  m <- cbind(x[ok], y[ok])
  i <- tryCatch(icc(m, model = "twoway", type = "agreement", unit = "single")$value,
                error = function(e) NA_real_)
  r <- suppressWarnings(cor(x[ok], y[ok]))
  c(n = sum(ok), icc = i, r = r)
}

comp_rows <- bind_rows(
  data.frame(t(icc_pair(feat$construct_coder1, feat$construct_coder2))) |>
    mutate(composite = "constructiveness", comparison = "RA-vs-RA (mean of binary codes)"),
  data.frame(t(icc_pair(feat$construct_coder1, feat$construct_ML))) |>
    mutate(composite = "constructiveness", comparison = "Coder1 vs Model binary mean"),
  data.frame(t(icc_pair(feat$construct_coder2, feat$construct_ML))) |>
    mutate(composite = "constructiveness", comparison = "Coder2 vs Model binary mean"),
  data.frame(t(icc_pair(feat$construct_coder1, feat$construct_prob))) |>
    mutate(composite = "constructiveness", comparison = "Coder1 vs Model probability mean"),
  data.frame(t(icc_pair(feat$construct_coder2, feat$construct_prob))) |>
    mutate(composite = "constructiveness", comparison = "Coder2 vs Model probability mean"),
  data.frame(t(icc_pair(feat$destruct_coder1, feat$destruct_coder2))) |>
    mutate(composite = "destructiveness", comparison = "RA-vs-RA (mean of binary codes)"),
  data.frame(t(icc_pair(feat$destruct_coder1, feat$destruct_ML))) |>
    mutate(composite = "destructiveness", comparison = "Coder1 vs Model binary mean"),
  data.frame(t(icc_pair(feat$destruct_coder2, feat$destruct_ML))) |>
    mutate(composite = "destructiveness", comparison = "Coder2 vs Model binary mean"),
  data.frame(t(icc_pair(feat$destruct_coder1, feat$destruct_prob))) |>
    mutate(composite = "destructiveness", comparison = "Coder1 vs Model probability mean"),
  data.frame(t(icc_pair(feat$destruct_coder2, feat$destruct_prob))) |>
    mutate(composite = "destructiveness", comparison = "Coder2 vs Model probability mean")
) %>% select(composite, comparison, n, icc, r)

log("\n-- A.4 Composite-index reliability --")
print(comp_rows, row.names = FALSE)

# Write feature tables
write.csv(rara,      file.path(TABLE_DIR, "feature_reliability_RA_vs_RA.csv"),     row.names = FALSE)
write.csv(rama,      file.path(TABLE_DIR, "feature_reliability_RA_vs_model.csv"),  row.names = FALSE)
write.csv(rama_prob, file.path(TABLE_DIR, "feature_RA_vs_model_probability.csv"),  row.names = FALSE)
write.csv(comp_rows, file.path(TABLE_DIR, "feature_composite_reliability.csv"),    row.names = FALSE)

# ---------------------------------------------------------------------------
# (B) STANCE
# ---------------------------------------------------------------------------
log("\n=== (B) STANCE reliability ===")
st <- read.csv(file.path(DATA_DIR, "merged_stance.csv"), stringsAsFactors = FALSE)
st_so <- st %>% filter(ra == "Coder1",  !is.na(stance_RA))
st_sh <- st %>% filter(ra == "Coder2", !is.na(stance_RA))
log(sprintf("N coded -- Coder1: %d ; Coder2: %d (overlap on comment_id: %d)",
            nrow(st_so), nrow(st_sh),
            length(intersect(st_so$comment_id, st_sh$comment_id))))

# RA vs RA
st_pair <- inner_join(
  st_so %>% select(comment_id, stance_coder1 = stance_RA),
  st_sh %>% select(comment_id, stance_coder2 = stance_RA),
  by = "comment_id"
)
log(sprintf("RA-vs-RA stance overlap: N = %d", nrow(st_pair)))
res_rara <- nominal_agreement(st_pair$stance_coder1, st_pair$stance_coder2)
log(sprintf("  pct agree=%.3f  kappa=%.3f  alpha=%.3f", res_rara$pct, res_rara$kappa, res_rara$alpha))
stance_levels <- c("pro_diversity","anti_diversity","neutral_unclear")
cm_rara <- confusion_table(st_pair$stance_coder1, st_pair$stance_coder2, stance_levels)

# RA vs model
stance_rows <- list()
for (ra in c("Coder1","Coder2")) {
  sub <- st %>% filter(ra == !!ra, !is.na(stance_RA), !is.na(stance_ML))
  if (nrow(sub) == 0) next
  res <- nominal_agreement(sub$stance_RA, sub$stance_ML)
  prf <- per_class_prf(sub$stance_RA, sub$stance_ML, stance_levels)
  stance_rows[[ra]] <- list(ra = ra, n = res$n, pct = res$pct,
                            kappa = res$kappa, alpha = res$alpha,
                            prf = prf,
                            cm = confusion_table(sub$stance_RA, sub$stance_ML, stance_levels))
}
stance_summary <- bind_rows(lapply(stance_rows, function(r) {
  data.frame(ra = r$ra, n = r$n, pct_agreement = r$pct,
             cohens_kappa = r$kappa, krippendorff_alpha = r$alpha,
             macro_f1 = r$prf$macro_f1[1])
}))
log("\n-- RA vs Model stance --")
print(stance_summary, row.names = FALSE)

# RA-vs-RA row
stance_summary <- bind_rows(
  data.frame(ra = "Coder1_vs_Coder2", n = res_rara$n,
             pct_agreement = res_rara$pct,
             cohens_kappa = res_rara$kappa,
             krippendorff_alpha = res_rara$alpha,
             macro_f1 = NA_real_),
  stance_summary
)
write.csv(stance_summary, file.path(TABLE_DIR, "stance_reliability_summary.csv"), row.names = FALSE)

# Per-class PRF
stance_prf <- bind_rows(lapply(stance_rows, function(r) {
  data.frame(ra = r$ra, r$prf)
}))
write.csv(stance_prf, file.path(TABLE_DIR, "stance_per_class_prf.csv"), row.names = FALSE)

# Confusion matrices to a single text file
cm_path <- file.path(TABLE_DIR, "stance_confusion_matrices.txt")
sink(cm_path)
cat("Stance confusion matrices (rows = Coder1/Coder2, cols = comparator)\n\n")
cat("Coder1 vs Coder2  (N =", res_rara$n, ")\n"); print(cm_rara); cat("\n")
for (ra in names(stance_rows)) {
  cat(ra, "vs Model  (N =", stance_rows[[ra]]$n, ")\n"); print(stance_rows[[ra]]$cm); cat("\n")
}
sink()

# ---------------------------------------------------------------------------
# (C) AGREEMENT
# ---------------------------------------------------------------------------
log("\n=== (C) AGREEMENT (dyad) reliability ===")
ag <- read.csv(file.path(DATA_DIR, "merged_agreement.csv"), stringsAsFactors = FALSE)
ag_so <- ag %>% filter(ra == "Coder1",  !is.na(agreement_RA))
ag_sh <- ag %>% filter(ra == "Coder2", !is.na(agreement_RA))
log(sprintf("N coded -- Coder1: %d ; Coder2: %d (overlap on dyad: %d)",
            nrow(ag_so), nrow(ag_sh),
            length(intersect(ag_so$child_comment_id, ag_sh$child_comment_id))))

# RA vs RA
ag_pair <- inner_join(
  ag_so %>% select(child_comment_id, agree_coder1 = agreement_RA),
  ag_sh %>% select(child_comment_id, agree_coder2 = agreement_RA),
  by = "child_comment_id"
)
log(sprintf("RA-vs-RA agreement overlap: N = %d", nrow(ag_pair)))
res_rara <- nominal_agreement(ag_pair$agree_coder1, ag_pair$agree_coder2)
log(sprintf("  pct agree=%.3f  kappa=%.3f  alpha=%.3f",
            res_rara$pct, res_rara$kappa, res_rara$alpha))
agree_levels <- c("agree","disagree","mixed","neither","unknown")
cm_rara <- confusion_table(ag_pair$agree_coder1, ag_pair$agree_coder2, agree_levels)

summarize_agreement_task <- function(task_label, allowed_levels) {
  # RA vs RA
  pair_sub <- ag_pair
  if (!is.null(allowed_levels)) {
    pair_sub <- pair_sub %>%
      filter(agree_coder1 %in% allowed_levels,
             agree_coder2 %in% allowed_levels)
  }
  res_pair <- nominal_agreement(pair_sub$agree_coder1, pair_sub$agree_coder2)
  prf_pair <- per_class_prf(pair_sub$agree_coder1, pair_sub$agree_coder2,
                            if (is.null(allowed_levels)) agree_levels else allowed_levels)
  rows <- list(data.frame(
    comparison = "Coder1_vs_Coder2",
    task = task_label,
    n = res_pair$n,
    pct_agreement = res_pair$pct,
    cohens_kappa = res_pair$kappa,
    krippendorff_alpha = res_pair$alpha,
    macro_f1 = prf_pair$macro_f1[1],
    stringsAsFactors = FALSE
  ))

  # RA vs model
  for (ra_label in c("Coder1","Coder2")) {
    sub <- ag %>% filter(ra == !!ra_label, !is.na(agreement_RA), !is.na(agreement_ML))
    if (!is.null(allowed_levels)) {
      sub <- sub %>%
        filter(agreement_RA %in% allowed_levels,
               agreement_ML %in% allowed_levels)
    }
    if (nrow(sub) == 0) next
    use_levels <- if (is.null(allowed_levels)) agree_levels else allowed_levels
    res <- nominal_agreement(sub$agreement_RA, sub$agreement_ML)
    prf <- per_class_prf(sub$agreement_RA, sub$agreement_ML, use_levels)
    rows[[length(rows) + 1]] <- data.frame(
      comparison = paste0(ra_label, "_vs_Model"),
      task = task_label,
      n = res$n,
      pct_agreement = res$pct,
      cohens_kappa = res$kappa,
      krippendorff_alpha = res$alpha,
      macro_f1 = prf$macro_f1[1],
      stringsAsFactors = FALSE
    )
  }
  bind_rows(rows)
}

agree_summary <- bind_rows(
  summarize_agreement_task("all_4_categories", NULL),
  summarize_agreement_task("agree_disagree_only", c("agree","disagree"))
)
log("\n-- AGREEMENT reliability summary --")
print(agree_summary, row.names = FALSE)

write.csv(agree_summary, file.path(TABLE_DIR, "agreement_reliability_summary.csv"), row.names = FALSE)
agree_rows <- list()
for (ra in c("Coder1","Coder2")) {
  sub <- ag %>% filter(ra == !!ra, !is.na(agreement_RA), !is.na(agreement_ML))
  if (nrow(sub) == 0) next
  res <- nominal_agreement(sub$agreement_RA, sub$agreement_ML)
  prf <- per_class_prf(sub$agreement_RA, sub$agreement_ML, agree_levels)
  agree_rows[[ra]] <- list(ra = ra, n = res$n, pct = res$pct,
                           kappa = res$kappa, alpha = res$alpha,
                           prf = prf,
                           cm = confusion_table(sub$agreement_RA, sub$agreement_ML, agree_levels))
}
agree_prf <- bind_rows(lapply(agree_rows, function(r) {
  data.frame(ra = r$ra, r$prf)
}))
write.csv(agree_prf, file.path(TABLE_DIR, "agreement_per_class_prf.csv"), row.names = FALSE)

cm_path <- file.path(TABLE_DIR, "agreement_confusion_matrices.txt")
sink(cm_path)
cat("Agreement confusion matrices (rows = first coder, cols = second)\n\n")
cat("Coder1 vs Coder2  (N =", res_rara$n, ")\n"); print(cm_rara); cat("\n")
for (ra in names(agree_rows)) {
  cat(ra, "vs Model  (N =", agree_rows[[ra]]$n, ")\n"); print(agree_rows[[ra]]$cm); cat("\n")
}
sink()

# ---------------------------------------------------------------------------
# Session info + write log
# ---------------------------------------------------------------------------
sink(file.path(TABLE_DIR, "sessionInfo.txt")); print(sessionInfo()); sink()
writeLines(unlist(log_lines), LOG)
log("\nAll reliability tables written to: ", TABLE_DIR)
