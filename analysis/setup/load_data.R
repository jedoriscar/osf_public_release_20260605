# Shared data-loading utility for the public analysis scripts.
# Run scripts from the public release folder root so relative paths resolve.

rm(list = ls())
library(dplyr)
library(tidyr)

data_path <- "data/analysis_objects/racial_comments.rda"

if (!file.exists(data_path)) {
  stop(paste("Data file not found at:", data_path, "\n",
             "Current working directory:", getwd(), "\n",
             "Please run scripts from the public release folder root."))
}

load(data_path)

if (!exists("joined_data")) {
  stop("Data object 'joined_data' not found after loading. Check the .rda file contents.")
}

# Standardize a few names expected by downstream scripts.
if (!"platform" %in% colnames(joined_data) && "platform_source.y" %in% colnames(joined_data)) {
  joined_data$platform <- joined_data$platform_source.y
}

if ("platform_source.y" %in% colnames(joined_data)) {
  yt_idx <- joined_data$platform_source.y == "YouTube"
  if ("likes" %in% colnames(joined_data) && sum(yt_idx) > 0) {
    joined_data$like_count[yt_idx] <- as.numeric(joined_data$likes[yt_idx])
  }
  if ("replies" %in% colnames(joined_data) && sum(yt_idx) > 0) {
    joined_data$reply_count[yt_idx] <- as.numeric(joined_data$replies[yt_idx])
  }
  # Relevance sorting applies only to top-level comments.
  if ("comment_type" %in% colnames(joined_data) && "parent_comment_id" %in% colnames(joined_data) && sum(yt_idx) > 0) {
    joined_data$top_comment <- NA_integer_
    is_reply <- !is.na(joined_data$parent_comment_id[yt_idx]) & joined_data$parent_comment_id[yt_idx] != ""
    joined_data$top_comment[yt_idx] <- ifelse(is_reply, 0L, as.integer(joined_data$comment_type[yt_idx]))
  }
}

# Use a stable row ID for analyses that need one row per comment.
row_id <- as.character(joined_data$comment_id)
miss <- is.na(row_id) | trimws(row_id) == ""
if ("unique_comment_identifier" %in% colnames(joined_data)) {
  ucid <- as.character(joined_data$unique_comment_identifier)
  row_id[miss] <- ucid[miss]
}
miss <- is.na(row_id) | trimws(row_id) == ""
if (any(miss)) {
  row_id[miss] <- paste0("row_", which(miss))
}
joined_data$row_id <- row_id

# Recreate the label-based indices used in the analyses.
constructive_cols <- c("prob_compassion", "prob_curiosity", "prob_nuance", "prob_personal_story", "prob_reasoning")
destructive_cols <- c("prob_toxic", "prob_identity_attack", "prob_threat", "prob_attack_on_author", "prob_attack_on_commenter")
if (all(constructive_cols %in% colnames(joined_data)) && all(destructive_cols %in% colnames(joined_data))) {
  joined_data$harmoniousness_raw <- rowMeans(joined_data[, constructive_cols] >= 0.6, na.rm = TRUE)
  joined_data$divisiveness_raw   <- rowMeans(joined_data[, destructive_cols] >= 0.6, na.rm = TRUE)
}

cat("Data loaded successfully.\n")
cat("Number of rows:", nrow(joined_data), "\n")
cat("Number of columns:", ncol(joined_data), "\n")
cat("Column names:\n")
print(colnames(joined_data)[1:20])

expected_vars <- c("harmoniousness_raw", "divisiveness_raw", "video_id", "comment_id")
missing_vars <- expected_vars[!expected_vars %in% colnames(joined_data)]
if (length(missing_vars) > 0) {
  warning(paste("Expected variables not found:", paste(missing_vars, collapse = ", ")))
}

cat("\nData structure check complete.\n")
cat("Use 'joined_data' as your data object in analysis scripts.\n")
