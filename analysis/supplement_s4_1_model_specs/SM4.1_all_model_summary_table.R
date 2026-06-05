# Purpose
# Generate summary table of all model specifications.
# Creates Table SX with all models, formulas, N's, and key coefficients.
#
# Reference: SI Appendix Section 4.1

# Setup
rm(list = ls())

# Create model summary table
cat("=== ALL MODEL SPECIFICATIONS SUMMARY ===\n\n")

model_summary <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", 
            "Model 6", "Model 7", "Model 8a", "Model 8b"),
  Research_Question = c("RQ1: Prevalence", "RQ1: Prevalence", "RQ2: Rewards", 
                       "RQ2: Rewards", "RQ2: Rewards", "RQ3: Propagation",
                       "RQ3: Propagation", "RQ3: Propagation", "RQ3: Propagation"),
  Model_Type = c("Paired t-test", "LMM", "Logistic MLM", "Neg Bin MLM", 
                "Neg Bin MLM", "Beta MLM", "Beta MLM", "Beta MLM", "Beta MLM"),
  Outcome = c("C vs D", "C vs D", "Top Comment", "Likes", "Replies",
             "Child C", "Child D", "Child D", "Child C"),
  Predictor = c("Within-comment", "Discourse type", "C + D", "C + D", "C + D",
               "Parent C", "Parent D", "Parent C", "Parent D"),
  N = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),  # Fill from actual results
  Key_Coefficient = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),  # Fill from actual results
  stringsAsFactors = FALSE
)

cat("Model Summary Table Structure:\n")
print(model_summary)

# Save results
saveRDS(model_summary, "analysis/supplement_s4_1_model_specs/SM4.1_all_model_summary_table_results.rds")
write.csv(model_summary, "analysis/supplement_s4_1_model_specs/Table_SX_all_model_specifications.csv", row.names = FALSE)

cat("\n=== RESULTS SAVED ===\n")
cat("Note: Fill in N's and coefficients from individual model scripts.\n")
