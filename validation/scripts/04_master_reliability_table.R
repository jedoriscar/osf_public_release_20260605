# ---------------------------------------------------------------------------
# 04_master_reliability_table.R
#
# Build a single, side-by-side reliability table covering every metric for
# every feature, plus PABAK (Prevalence-Adjusted Bias-Adjusted Kappa).
# PABAK = 2 * percent_agreement - 1; it is the right complement to Cohen's
# kappa when the base rate is highly skewed.
#
# Produces:
#   - outputs/tables/feature_reliability_master.csv  (one row per feature)
#   - outputs/tables/composite_reliability_master.csv (one row per composite/pair)
#   - prints a markdown-rendered table to stdout
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(irr)
  library(pROC)
})

ROOT <- normalizePath(getwd(), mustWork = TRUE)
DATA_DIR  <- file.path(ROOT, "validation/data")
TABLE_DIR <- file.path(ROOT, "validation/tables")

FEATURES <- c("compassion","curiosity","nuance","personal_story","reasoning",
              "toxicity","identity_attack","threat","attack_on_author",
              "attack_on_commenter")
CONSTRUCTIVE <- FEATURES[1:5]
DESTRUCTIVE  <- FEATURES[6:10]

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
pct_agree <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) == 0) return(NA_real_)
  mean(a[ok] == b[ok])
}
cohen_k <- function(a, b) {
  ok <- !is.na(a) & !is.na(b); if (sum(ok) < 2) return(NA_real_)
  tryCatch(kappa2(cbind(a[ok], b[ok]))$value, error = function(e) NA_real_)
}
kripp_a <- function(a, b) {
  ok <- !is.na(a) & !is.na(b); if (sum(ok) < 2) return(NA_real_)
  suppressWarnings(tryCatch(kripp.alpha(t(cbind(a[ok], b[ok])), method="nominal")$value,
                            error = function(e) NA_real_))
}
pabak <- function(a, b) {
  # 2*Po - 1; range [-1, 1] like kappa, but unaffected by base rate
  p <- pct_agree(a, b); if (is.na(p)) NA_real_ else 2 * p - 1
}
pbr_r <- function(prob, bin) {
  ok <- !is.na(prob) & !is.na(bin); if (sum(ok) < 5) return(NA_real_)
  suppressWarnings(cor(prob[ok], bin[ok]))
}
auc_of <- function(prob, bin) {
  ok <- !is.na(prob) & !is.na(bin); if (sum(ok) < 5) return(NA_real_)
  if (length(unique(bin[ok])) < 2) return(NA_real_)
  tryCatch(as.numeric(auc(roc(bin[ok], prob[ok], quiet = TRUE,
                              levels = c(0,1), direction = "<"))),
           error = function(e) NA_real_)
}
n_pair <- function(a, b) sum(!is.na(a) & !is.na(b))

# ---------------------------------------------------------------------------
# Load merged feature data
# ---------------------------------------------------------------------------
feat <- read.csv(file.path(DATA_DIR, "merged_features.csv"), stringsAsFactors = FALSE)

# canonical & key probs are identical; use the .key probs
prob_col <- function(f) {
  for (cand in c(paste0("prob_", f, ".key"), paste0("prob_", f))) {
    if (cand %in% names(feat)) return(cand)
  }
  stop("no prob col for ", f)
}

# ---------------------------------------------------------------------------
# Master feature table
# ---------------------------------------------------------------------------
rows <- lapply(FEATURES, function(f) {
  so <- feat[[paste0(f, "_coder1")]]
  sh <- feat[[paste0(f, "_coder2")]]
  ml <- feat[[paste0(f, "_ML")]]
  p  <- feat[[prob_col(f)]]
  data.frame(
    feature = f,
    n_pair         = n_pair(so, sh),
    prev_coder1    = mean(so, na.rm = TRUE),
    prev_coder2   = mean(sh, na.rm = TRUE),
    prev_model     = mean(ml, na.rm = TRUE),
    # RA vs RA
    pct_RAxRA      = pct_agree(so, sh),
    kappa_RAxRA    = cohen_k(so, sh),
    pabak_RAxRA    = pabak(so, sh),
    alpha_RAxRA    = kripp_a(so, sh),
    # Coder1 vs Model (binary)
    pct_So_ML      = pct_agree(so, ml),
    kappa_So_ML    = cohen_k(so, ml),
    pabak_So_ML    = pabak(so, ml),
    # Coder2 vs Model (binary)
    pct_Sh_ML      = pct_agree(sh, ml),
    kappa_Sh_ML    = cohen_k(sh, ml),
    pabak_Sh_ML    = pabak(sh, ml),
    # RA binary vs Model probability
    r_So_prob      = pbr_r(p, so),
    auc_So_prob    = auc_of(p, so),
    r_Sh_prob      = pbr_r(p, sh),
    auc_Sh_prob    = auc_of(p, sh),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

write.csv(rows, file.path(TABLE_DIR, "feature_reliability_master.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# Pretty print as markdown
# ---------------------------------------------------------------------------
fmt2 <- function(x) ifelse(is.na(x), "—", sprintf("%.2f", x))
fmt_pct <- function(x) ifelse(is.na(x), "—", sprintf("%.0f%%", 100 * x))

cat("\n\n## Feature-level reliability (N pair = both RAs coded)\n\n")
cat("All comments are double-coded (N ≈ 298 per feature). prev = positive base rate.\n\n")
cat("| feature | prev_So | prev_Sh | prev_ML | RA×RA % | RA×RA κ | RA×RA PABAK | Coder1–ML κ | Coder1–ML PABAK | Coder2–ML κ | Coder2–ML PABAK | Coder1 r→prob | Coder1 AUC | Coder2 r→prob | Coder2 AUC |\n")
cat("|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|\n")
for (i in seq_len(nrow(rows))) {
  r <- rows[i, ]
  cat(sprintf(
    "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n",
    r$feature,
    fmt_pct(r$prev_coder1), fmt_pct(r$prev_coder2), fmt_pct(r$prev_model),
    fmt_pct(r$pct_RAxRA),  fmt2(r$kappa_RAxRA),  fmt2(r$pabak_RAxRA),
    fmt2(r$kappa_So_ML), fmt2(r$pabak_So_ML),
    fmt2(r$kappa_Sh_ML), fmt2(r$pabak_Sh_ML),
    fmt2(r$r_So_prob), fmt2(r$auc_So_prob),
    fmt2(r$r_Sh_prob), fmt2(r$auc_Sh_prob)
  ))
}

# ---------------------------------------------------------------------------
# Composite table (constructiveness / destructiveness)
# ---------------------------------------------------------------------------
mk_mean <- function(df, who, set) {
  cols <- paste0(set, "_", who)
  rowMeans(as.matrix(df[, cols]), na.rm = TRUE)
}
mk_ml_bin <- function(df, set) {
  rowMeans(sapply(set, function(f) df[[paste0(f,"_ML")]]), na.rm = TRUE)
}
mk_ml_prob <- function(df, set) {
  rowMeans(sapply(set, function(f) df[[prob_col(f)]]), na.rm = TRUE)
}

cs <- mk_mean(feat, "coder1",  CONSTRUCTIVE)
ch <- mk_mean(feat, "coder2", CONSTRUCTIVE)
ds <- mk_mean(feat, "coder1",  DESTRUCTIVE)
dh <- mk_mean(feat, "coder2", DESTRUCTIVE)
cm <- mk_ml_bin(feat, CONSTRUCTIVE); cp <- mk_ml_prob(feat, CONSTRUCTIVE)
dm <- mk_ml_bin(feat, DESTRUCTIVE);  dp <- mk_ml_prob(feat, DESTRUCTIVE)

icc_pair <- function(x, y) {
  ok <- complete.cases(x, y); if (sum(ok) < 5) return(NA_real_)
  tryCatch(icc(cbind(x[ok], y[ok]), model = "twoway", type = "agreement",
               unit = "single")$value, error = function(e) NA_real_)
}
r_pair <- function(x, y) {
  ok <- complete.cases(x, y); if (sum(ok) < 5) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok]))
}

comp <- bind_rows(
  data.frame(composite = "constructiveness", pair = "Coder1 × Coder2",
             n = sum(complete.cases(cs, ch)), ICC = icc_pair(cs, ch), r = r_pair(cs, ch)),
  data.frame(composite = "constructiveness", pair = "Coder1 × Model binary mean",
             n = sum(complete.cases(cs, cm)), ICC = icc_pair(cs, cm), r = r_pair(cs, cm)),
  data.frame(composite = "constructiveness", pair = "Coder2 × Model binary mean",
             n = sum(complete.cases(ch, cm)), ICC = icc_pair(ch, cm), r = r_pair(ch, cm)),
  data.frame(composite = "constructiveness", pair = "Coder1 × Model probability mean",
             n = sum(complete.cases(cs, cp)), ICC = icc_pair(cs, cp), r = r_pair(cs, cp)),
  data.frame(composite = "constructiveness", pair = "Coder2 × Model probability mean",
             n = sum(complete.cases(ch, cp)), ICC = icc_pair(ch, cp), r = r_pair(ch, cp)),
  data.frame(composite = "destructiveness", pair = "Coder1 × Coder2",
             n = sum(complete.cases(ds, dh)), ICC = icc_pair(ds, dh), r = r_pair(ds, dh)),
  data.frame(composite = "destructiveness", pair = "Coder1 × Model binary mean",
             n = sum(complete.cases(ds, dm)), ICC = icc_pair(ds, dm), r = r_pair(ds, dm)),
  data.frame(composite = "destructiveness", pair = "Coder2 × Model binary mean",
             n = sum(complete.cases(dh, dm)), ICC = icc_pair(dh, dm), r = r_pair(dh, dm)),
  data.frame(composite = "destructiveness", pair = "Coder1 × Model probability mean",
             n = sum(complete.cases(ds, dp)), ICC = icc_pair(ds, dp), r = r_pair(ds, dp)),
  data.frame(composite = "destructiveness", pair = "Coder2 × Model probability mean",
             n = sum(complete.cases(dh, dp)), ICC = icc_pair(dh, dp), r = r_pair(dh, dp))
)

write.csv(comp, file.path(TABLE_DIR, "composite_reliability_master.csv"),
          row.names = FALSE)

cat("\n\n## Composite-index reliability\n\n")
cat("| composite | pair | N | ICC(3,1) | Pearson r |\n")
cat("|---|---|---|---|---|\n")
for (i in seq_len(nrow(comp))) {
  cat(sprintf("| %s | %s | %d | %s | %s |\n",
              comp$composite[i], comp$pair[i], comp$n[i],
              fmt2(comp$ICC[i]), fmt2(comp$r[i])))
}

# ---------------------------------------------------------------------------
# Stance + Agreement: also show pct + kappa + pabak + macro-F1 side by side
# ---------------------------------------------------------------------------
cat("\n\n## Stance and Agreement (nominal tasks)\n\n")

st <- read.csv(file.path(DATA_DIR, "merged_stance.csv"), stringsAsFactors = FALSE)
st_so <- st %>% filter(ra == "Coder1",  !is.na(stance_RA))
st_sh <- st %>% filter(ra == "Coder2", !is.na(stance_RA))
st_pair <- inner_join(
  st_so %>% select(comment_id, a = stance_RA),
  st_sh %>% select(comment_id, b = stance_RA),
  by = "comment_id")

ag <- read.csv(file.path(DATA_DIR, "merged_agreement.csv"), stringsAsFactors = FALSE)
ag_so <- ag %>% filter(ra == "Coder1",  !is.na(agreement_RA))
ag_sh <- ag %>% filter(ra == "Coder2", !is.na(agreement_RA))
ag_pair <- inner_join(
  ag_so %>% select(child_comment_id, a = agreement_RA),
  ag_sh %>% select(child_comment_id, b = agreement_RA),
  by = "child_comment_id")

nominal_row <- function(label, a, b) {
  data.frame(comparison = label,
             n = n_pair(a, b),
             pct = pct_agree(a, b),
             kappa = cohen_k(a, b),
             pabak = pabak(a, b),
             alpha = kripp_a(a, b))
}

nom <- bind_rows(
  nominal_row("Stance — Coder1 × Coder2",  st_pair$a, st_pair$b),
  nominal_row("Stance — Coder1 × Model",    st_so$stance_RA, st_so$stance_ML),
  nominal_row("Stance — Coder2 × Model",   st_sh$stance_RA, st_sh$stance_ML),
  nominal_row("Agreement all 4 — Coder1 × Coder2", ag_pair$a, ag_pair$b),
  nominal_row("Agreement all 4 — Coder1 × Model", (ag_so %>% filter(!is.na(agreement_ML)))$agreement_RA,
                                                  (ag_so %>% filter(!is.na(agreement_ML)))$agreement_ML),
  nominal_row("Agreement all 4 — Coder2 × Model",(ag_sh %>% filter(!is.na(agreement_ML)))$agreement_RA,
                                                  (ag_sh %>% filter(!is.na(agreement_ML)))$agreement_ML),
  nominal_row("Agreement agree/disagree — Coder1 × Coder2",
              (ag_pair %>% filter(a %in% c("agree","disagree"), b %in% c("agree","disagree")))$a,
              (ag_pair %>% filter(a %in% c("agree","disagree"), b %in% c("agree","disagree")))$b),
  nominal_row("Agreement agree/disagree — Coder1 × Model",
              (ag_so %>% filter(!is.na(agreement_ML), agreement_RA %in% c("agree","disagree"), agreement_ML %in% c("agree","disagree")))$agreement_RA,
              (ag_so %>% filter(!is.na(agreement_ML), agreement_RA %in% c("agree","disagree"), agreement_ML %in% c("agree","disagree")))$agreement_ML),
  nominal_row("Agreement agree/disagree — Coder2 × Model",
              (ag_sh %>% filter(!is.na(agreement_ML), agreement_RA %in% c("agree","disagree"), agreement_ML %in% c("agree","disagree")))$agreement_RA,
              (ag_sh %>% filter(!is.na(agreement_ML), agreement_RA %in% c("agree","disagree"), agreement_ML %in% c("agree","disagree")))$agreement_ML)
)
write.csv(nom, file.path(TABLE_DIR, "nominal_reliability_master.csv"), row.names = FALSE)

cat("| comparison | N | % agree | κ | PABAK | Krippendorff α |\n")
cat("|---|---|---|---|---|---|\n")
for (i in seq_len(nrow(nom))) {
  cat(sprintf("| %s | %d | %s | %s | %s | %s |\n",
              nom$comparison[i], nom$n[i],
              fmt_pct(nom$pct[i]), fmt2(nom$kappa[i]),
              fmt2(nom$pabak[i]), fmt2(nom$alpha[i])))
}

cat("\nMaster tables written to:\n",
    "  ", file.path(TABLE_DIR, "feature_reliability_master.csv"), "\n",
    "  ", file.path(TABLE_DIR, "composite_reliability_master.csv"), "\n",
    "  ", file.path(TABLE_DIR, "nominal_reliability_master.csv"), "\n", sep = "")
