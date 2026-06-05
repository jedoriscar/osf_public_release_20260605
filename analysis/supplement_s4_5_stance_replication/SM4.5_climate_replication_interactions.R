# Purpose
# Replicate stance x constructiveness interaction models in the climate dataset.

# Setup
rm(list = ls())
library(dplyr)
library(lme4)
library(lmerTest)

# Load climate data
climate_data_path <- "data/analysis_objects/climate_comments.rds"
if (!file.exists(climate_data_path)) {
  stop("Climate data file not found. Expected at: ", climate_data_path)
}
joined_data <- readRDS(climate_data_path)

climate_stance_file <- "data/model_labels/climate_stance_labels.csv"
if (!file.exists(climate_stance_file)) {
  stop("Climate stance data not found. Expected at: ", climate_stance_file)
}

climate_stance_data <- read.csv(climate_stance_file) %>%
  select(comment_id, stance_label) %>%
  filter(!is.na(comment_id), comment_id != "") %>%
  distinct(comment_id, .keep_all = TRUE)

analysis_data <- joined_data %>%
  inner_join(climate_stance_data, by = "comment_id", relationship = "many-to-one") %>%
  filter(tolower(platform) == "youtube" | is.na(platform)) %>%
  filter(stance_label %in% c("Climate Believer", "Climate Skeptic")) %>%
  mutate(
    stance_binary = ifelse(stance_label == "Climate Skeptic", 1, 0),
    comment_likes = as.numeric(likes),
    comment_replies = as.numeric(replies)
  ) %>%
  filter(!is.na(comment_likes), !is.na(comment_replies))

cat("=== CLIMATE STANCE X CONSTRUCTIVENESS INTERACTIONS ===\n")
cat("N comments:", nrow(analysis_data), "\n")
cat("N videos:", length(unique(analysis_data$video_id)), "\n")
print(table(analysis_data$stance_label, useNA = "ifany"))

mod_likes <- glmer.nb(
  comment_likes ~ stance_binary + harmoniousness_raw +
    stance_binary:harmoniousness_raw + (1 | video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

mod_replies <- glmer.nb(
  comment_replies ~ stance_binary + harmoniousness_raw +
    stance_binary:harmoniousness_raw + (1 | video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

extract_interaction <- function(model) {
  coefs <- fixef(model)
  ses <- sqrt(diag(vcov(model)))
  z_vals <- coefs / ses
  p_vals <- 2 * (1 - pnorm(abs(z_vals)))
  irr_vals <- exp(coefs)
  int_name <- names(coefs)[length(coefs)]
  list(
    term = int_name,
    b = coefs[int_name],
    se = ses[int_name],
    z = z_vals[int_name],
    p = p_vals[int_name],
    irr = irr_vals[int_name]
  )
}

likes_interaction <- extract_interaction(mod_likes)
replies_interaction <- extract_interaction(mod_replies)

cat("\nLikes interaction:\n")
print(likes_interaction)
cat("\nReplies interaction:\n")
print(replies_interaction)

results <- list(
  likes_model = mod_likes,
  replies_model = mod_replies,
  likes_interaction = likes_interaction,
  replies_interaction = replies_interaction,
  n = nrow(analysis_data),
  n_videos = length(unique(analysis_data$video_id))
)

saveRDS(results, "analysis/supplement_s4_5_stance_replication/SM4.5_climate_interactions_results.rds")

cat("\n=== CLIMATE REPLICATION COMPLETE ===\n")
