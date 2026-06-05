# Build a single unified table: 5 constructive features + constructive index
# + 5 destructive features + destructive index, with Coder1-vs-Coder2 and
# Coder1-vs-Model metrics side by side.
# For features (binary): % agreement and Cohen's kappa.
# For composites (continuous proportion-of-5): ICC and Pearson r.
# Coder1-vs-Model is reported against both the binary at 0.6 (matching the
# manuscript's index construction) and the underlying probability.

suppressPackageStartupMessages({
  library(dplyr); library(irr); library(pROC)
})

ROOT <- normalizePath(getwd(), mustWork = TRUE)
DATA_DIR  <- file.path(ROOT, "validation/data")
TABLE_DIR <- file.path(ROOT, "validation/tables")

CONSTRUCTIVE <- c("compassion","curiosity","nuance","personal_story","reasoning")
DESTRUCTIVE  <- c("toxicity","identity_attack","threat","attack_on_author","attack_on_commenter")

feat <- read.csv(file.path(DATA_DIR, "merged_features.csv"), stringsAsFactors = FALSE)
prob_col <- function(f) paste0("prob_", f, ".key")

# Helpers
pa <- function(a, b) { ok <- !is.na(a) & !is.na(b); if (!any(ok)) NA else mean(a[ok]==b[ok]) }
ck <- function(a, b) { ok <- !is.na(a) & !is.na(b); if (sum(ok)<2) NA else
  tryCatch(kappa2(cbind(a[ok], b[ok]))$value, error=function(e) NA) }
ic <- function(a, b) { ok <- complete.cases(a, b); if (sum(ok)<5) NA else
  tryCatch(icc(cbind(a[ok], b[ok]), model="twoway", type="agreement", unit="single")$value,
           error=function(e) NA) }
rp <- function(a, b) { ok <- complete.cases(a, b); if (sum(ok)<5) NA else
  suppressWarnings(cor(a[ok], b[ok])) }
au <- function(p, y) { ok <- !is.na(p)&!is.na(y); if (sum(ok)<5 || length(unique(y[ok]))<2) NA else
  tryCatch(as.numeric(auc(roc(y[ok], p[ok], quiet=TRUE, levels=c(0,1), direction="<"))),
           error=function(e) NA) }

# Per-feature rows
feature_row <- function(f) {
  so <- feat[[paste0(f,"_coder1")]]
  sh <- feat[[paste0(f,"_coder2")]]
  ml <- feat[[paste0(f,"_ML")]]
  p  <- feat[[prob_col(f)]]
  data.frame(
    row = f,
    N = sum(!is.na(so) & !is.na(sh)),
    prev_So = mean(so, na.rm=TRUE),
    prev_Sh = mean(sh, na.rm=TRUE),
    prev_ML = mean(ml, na.rm=TRUE),
    SoSh_pct  = pa(so, sh),
    SoSh_stat = ck(so, sh),         # kappa (binary)
    SoMLb_pct = pa(so, ml),
    SoMLb_stat= ck(so, ml),         # kappa (binary)
    SoMLp_r   = rp(p, so),          # point-biserial r
    SoMLp_AUC = au(p, so),          # AUC
    kind = "feature",
    stringsAsFactors = FALSE
  )
}

# Composite rows (continuous proportion-of-5)
composite_row <- function(label, set) {
  cs <- rowMeans(as.matrix(feat[, paste0(set, "_coder1")]),  na.rm=TRUE)
  ch <- rowMeans(as.matrix(feat[, paste0(set, "_coder2")]), na.rm=TRUE)
  cm <- rowMeans(sapply(set, function(f) feat[[paste0(f,"_ML")]]), na.rm=TRUE)
  cp <- rowMeans(sapply(set, function(f) feat[[prob_col(f)]]),   na.rm=TRUE)
  data.frame(
    row = label,
    N = sum(complete.cases(cs, ch)),
    prev_So = mean(cs, na.rm=TRUE),
    prev_Sh = mean(ch, na.rm=TRUE),
    prev_ML = mean(cm, na.rm=TRUE),
    SoSh_pct  = rp(cs, ch),         # use Pearson r in "pct" slot for composites
    SoSh_stat = ic(cs, ch),         # ICC
    SoMLb_pct = rp(cs, cm),
    SoMLb_stat= ic(cs, cm),
    SoMLp_r   = rp(cs, cp),
    SoMLp_AUC = ic(cs, cp),         # ICC for composite vs probability
    kind = "composite",
    stringsAsFactors = FALSE
  )
}

tbl <- bind_rows(
  lapply(CONSTRUCTIVE, feature_row),
  list(composite_row("Constructive index (0-1)", CONSTRUCTIVE)),
  lapply(DESTRUCTIVE,  feature_row),
  list(composite_row("Destructive index (0-1)",  DESTRUCTIVE))
)

write.csv(tbl, file.path(TABLE_DIR, "unified_feature_table.csv"), row.names = FALSE)

# Pretty print
fmt2 <- function(x) ifelse(is.na(x), "—", sprintf("%.2f", x))
pct  <- function(x) ifelse(is.na(x), "—", sprintf("%.0f%%", 100*x))
cat("\n| feature / index | N | prev. So | prev. Sh | prev. ML | Coder1–Coder2 % agree | Coder1–Coder2 κ | Coder1–Model(0.6) % agree | Coder1–Model(0.6) κ | Coder1 r → ML prob | Coder1 AUC |\n")
cat("|---|---|---|---|---|---|---|---|---|---|---|\n")
for (i in seq_len(nrow(tbl))) {
  r <- tbl[i, ]
  if (r$kind == "feature") {
    cat(sprintf("| %s | %d | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n",
                r$row, r$N, pct(r$prev_So), pct(r$prev_Sh), pct(r$prev_ML),
                pct(r$SoSh_pct),  fmt2(r$SoSh_stat),
                pct(r$SoMLb_pct), fmt2(r$SoMLb_stat),
                fmt2(r$SoMLp_r),  fmt2(r$SoMLp_AUC)))
  } else {
    # composite: % cells contain Pearson r; κ cells contain ICC; AUC cell contains ICC
    cat(sprintf("| **%s** | %d | %s | %s | %s | r=%s | ICC=%s | r=%s | ICC=%s | r=%s | ICC=%s |\n",
                r$row, r$N, fmt2(r$prev_So), fmt2(r$prev_Sh), fmt2(r$prev_ML),
                fmt2(r$SoSh_pct),  fmt2(r$SoSh_stat),
                fmt2(r$SoMLb_pct), fmt2(r$SoMLb_stat),
                fmt2(r$SoMLp_r),   fmt2(r$SoMLp_AUC)))
  }
}
cat("\nWrote:", file.path(TABLE_DIR, "unified_feature_table.csv"), "\n")
