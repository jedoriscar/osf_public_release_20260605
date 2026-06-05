# Purpose
# Correlations between constructiveness/destructiveness and politeness for
# discriminant validity. Politeness from R package `politeness` (Danescu-Niculescu-Mizil et al.).
#
# Reference: Main text line 52
# Data: Canonical joined_data. Politeness should be in .rda after running
#       prepare_canonical_data.R once; otherwise computed here with politeness package.

# Setup
rm(list = ls())

# Load data
# Canonical load; harmoniousness_raw/divisiveness_raw are label-based from load_data.R.
source("analysis/setup/load_data.R")

# Politeness: prefer column saved in canonical .rda (from prepare_canonical_data.R)
if ("politeness" %in% colnames(joined_data)) {
  cat("Using existing 'politeness' column from canonical data.\n")
} else if ("politeness_score" %in% colnames(joined_data)) {
  joined_data$politeness <- joined_data$politeness_score
  cat("Using existing 'politeness_score' column.\n")
} else {
  if (!requireNamespace("politeness", quietly = TRUE)) {
    stop("Politeness not in data. Run from the public release folder root:\n  Rscript analysis/setup/prepare_canonical_data.R\nThen re-run this script. Or install.packages(\"politeness\") and ensure comment text column exists.")
  }
  library(politeness)
  comment_col <- if ("comment" %in% colnames(joined_data)) "comment" else
    if ("comment_text" %in% colnames(joined_data)) "comment_text" else NULL
  if (is.null(comment_col)) stop("No comment text column for politeness.")
  texts <- as.character(joined_data[[comment_col]])
  texts[is.na(texts) | texts == ""] <- " "
  texts <- iconv(texts, from = "", to = "ASCII", sub = " ")
  texts[is.na(texts)] <- " "
  cat("Computing politeness with politeness package (parser='none', metric='average')...\n")
  pf <- politeness::politeness(texts, parser = "none", metric = "average", drop_blank = FALSE)
  joined_data$politeness <- rowMeans(pf, na.rm = TRUE)
  joined_data$politeness[is.na(joined_data$politeness)] <- 0
  cat("Done.\n")
}

# Calculate correlations
cat("\n=== DISCRIMINANT VALIDITY: POLITENESS CORRELATIONS ===\n\n")

cor_C_pol <- cor(joined_data$harmoniousness_raw, joined_data$politeness, use = "complete.obs")
test_C_pol <- cor.test(joined_data$harmoniousness_raw, joined_data$politeness, use = "complete.obs")

cat("Constructiveness × Politeness:\n")
cat(sprintf("  r = %.3f, p = %.3f\n", cor_C_pol, test_C_pol$p.value))

cor_D_pol <- cor(joined_data$divisiveness_raw, joined_data$politeness, use = "complete.obs")
test_D_pol <- cor.test(joined_data$divisiveness_raw, joined_data$politeness, use = "complete.obs")

cat("\nDestructiveness × Politeness:\n")
cat(sprintf("  r = %.3f, p = %.3f\n", cor_D_pol, test_D_pol$p.value))

# Save results
results <- list(
  C_politeness = list(r = cor_C_pol, p = test_C_pol$p.value),
  D_politeness = list(r = cor_D_pol, p = test_D_pol$p.value)
)

saveRDS(results, "analysis/discriminant_validity/RQ0_politeness_correlations_results.rds")

cat("\n=== RESULTS SAVED ===\n")
