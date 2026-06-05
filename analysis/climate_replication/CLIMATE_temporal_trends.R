# Purpose
# Replicate temporal trends analysis in climate change dataset.
# Uses comment year (when comments were posted), not video year.
#
# Reference: Main text 10_results_replication

# Setup
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)
library(lubridate)

# Load climate data
climate_data_path <- "data/analysis_objects/climate_comments.rds"

if (!file.exists(climate_data_path)) stop("Climate data not found.")

joined_data <- readRDS(climate_data_path)

# Extract year from comment date (comment_published_at may be Unix timestamp)
parse_year <- function(x) {
  num <- suppressWarnings(as.numeric(x))
  valid <- !is.na(num) & num > 1e9 & num < 2e9  # Unix timestamp range ~2001-2033
  out <- rep(NA_real_, length(x))
  out[valid] <- lubridate::year(as.POSIXct(num[valid], origin = "1970-01-01"))
  out
}
joined_data <- joined_data %>%
  mutate(year = parse_year(comment_published_at)) %>%
  filter(!is.na(year), !is.na(harmoniousness_raw), !is.na(divisiveness_raw))

# Temporal models
cat("=== CLIMATE REPLICATION: TEMPORAL TRENDS (comment year) ===\n\n")

mod_C <- lmer(
  harmoniousness_raw ~ year + (1|video_id),
  data = joined_data
)

mod_D <- lmer(
  divisiveness_raw ~ year + (1|video_id),
  data = joined_data
)

coefs_C <- fixef(mod_C)
coefs_D <- fixef(mod_D)
ses_C <- sqrt(diag(vcov(mod_C)))
ses_D <- sqrt(diag(vcov(mod_D)))

cat("Constructiveness over time:\n")
cat(sprintf("  b = %.4f (SE = %.4f)\n", coefs_C[2], ses_C[2]))

cat("\nDestructiveness over time:\n")
cat(sprintf("  b = %.4f (SE = %.4f)\n", coefs_D[2], ses_D[2]))

# Harmony advantage (constructiveness - destructiveness) over time
joined_data$harmony_advantage <- joined_data$harmoniousness_raw - joined_data$divisiveness_raw
mod_adv <- lmer(harmony_advantage ~ year + (1|video_id), data = joined_data)
coef_adv <- fixef(mod_adv)
se_adv <- sqrt(diag(vcov(mod_adv)))[2]
t_adv <- coef_adv[2] / se_adv
p_adv <- 2 * (1 - pt(abs(t_adv), df = summary(mod_adv)$devcomp$dims["n"] - 2))

cat("\nHarmony advantage over time:\n")
cat(sprintf("  b = %.4f (SE = %.4f), t = %.2f, p = %.4f\n", coef_adv[2], se_adv, t_adv, p_adv))

cat("\nN =", nrow(joined_data), "\n")
cat("Year range:", min(joined_data$year), "-", max(joined_data$year), "\n")

# Save results
results <- list(
  constructiveness_model = mod_C,
  destructiveness_model = mod_D,
  advantage_model = mod_adv,
  constructiveness_b = coefs_C[2],
  destructiveness_b = coefs_D[2],
  advantage_b = coef_adv[2],
  advantage_p = p_adv,
  n = nrow(joined_data)
)

saveRDS(results, "analysis/climate_replication/CLIMATE_temporal_trends_results.rds")

cat("\n=== RESULTS SAVED ===\n")
