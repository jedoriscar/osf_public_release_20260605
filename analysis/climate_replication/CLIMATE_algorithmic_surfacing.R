# Purpose
# Test whether constructiveness and destructiveness predict algorithmic surfacing
# in climate change dataset
#
# Reference: Main text lines 197-198
# Model: Logistic multilevel model with random intercepts for video

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

# Load climate data
climate_data_path <- "data/analysis_objects/climate_comments.rds"

if (!file.exists(climate_data_path)) {
  stop("Climate data file not found.")
}

joined_data <- readRDS(climate_data_path)

# Filter to YouTube only, TOP-LEVEL comments only
analysis_data <- joined_data %>%
  filter(tolower(platform) == "youtube") %>%
  mutate(
    is_reply = !is.na(parent_comment_id) & parent_comment_id != "",
    # Create top_comment from comment_type (1 = relevance-sorted = Top Comment)
    top_comment_binary = ifelse(!is.na(comment_type) & comment_type == 1, 1, 0),
    video_id = as.factor(video_id)
  ) %>%
  filter(!is_reply, !is.na(harmoniousness_raw), !is.na(divisiveness_raw))

cat("=== CLIMATE REPLICATION: ALGORITHMIC SURFACING ===\n")
cat("N top-level comments:", nrow(analysis_data), "\n")
cat("N videos:", length(unique(analysis_data$video_id)), "\n")
cat("Top comment rate:", mean(analysis_data$top_comment_binary, na.rm = TRUE), "\n\n")

# Model: Algorithmic Surfacing (Logistic MLM)
mod1 <- glmer(
  top_comment_binary ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = analysis_data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# Extract results
coefs <- fixef(mod1)
ses <- sqrt(diag(vcov(mod1)))
z_vals <- coefs / ses
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
or_vals <- exp(coefs)

# Wald CI
ci_vals <- rbind(
  exp(coefs[1] + c(-1, 1) * 1.96 * ses[1]),
  exp(coefs[2] + c(-1, 1) * 1.96 * ses[2]),
  exp(coefs[3] + c(-1, 1) * 1.96 * ses[3])
)
rownames(ci_vals) <- names(coefs)

cat("Constructiveness:\n")
cat(sprintf("  OR = %.2f [%.2f, %.2f], p = %.3f\n\n", 
            or_vals[2], ci_vals[2, 1], ci_vals[2, 2], p_vals[2]))

cat("Destructiveness:\n")
cat(sprintf("  OR = %.2f [%.2f, %.2f], p = %.3f\n\n", 
            or_vals[3], ci_vals[3, 1], ci_vals[3, 2], p_vals[3]))

cat("=== RESULTS SAVED ===\n")
