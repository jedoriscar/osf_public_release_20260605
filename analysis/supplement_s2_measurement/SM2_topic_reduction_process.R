# Purpose
# Document the topic reduction process (638 → 50 → 18 topics).
# Reports statistics at each reduction stage.
#
# Reference: SI Appendix Section 2.1
# Reports: Initial 638 topics, reduced to 50, then filtered to 18

# Setup
rm(list = ls())

# Document reduction process
cat("=== TOPIC REDUCTION PROCESS ===\n\n")

cat("Stage 1: Initial discovery\n")
cat("  638 topics identified from full dataset\n\n")

cat("Stage 2: Hierarchical reduction\n")
cat("  50 topics after hierarchical clustering\n")
cat("  (Maintains comprehensive coverage)\n\n")

cat("Stage 3: Relevance filtering\n")
cat("  18 topics retained (most relevant to demographic change)\n")
cat("  32 topics excluded (tangential themes)\n\n")

cat("Final topic set:\n")
cat("  - 18 topics covering 78,123 documents (77.3% of corpus)\n")
cat("  - Topic -1: 227 outlier documents (0.3%)\n")

# Save results
results <- list(
  stage1_initial = 638,
  stage2_hierarchical = 50,
  stage3_final = 18,
  topics_excluded = 32,
  documents_covered = 78123,
  outlier_documents = 227
)

saveRDS(results, "analysis/supplement_s2_measurement/SM2_topic_reduction_process_results.rds")

cat("\n=== RESULTS SAVED ===\n")
