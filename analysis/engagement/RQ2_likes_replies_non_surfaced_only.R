# Goal (Nour §2.1)
# (1) Cross-correlations: top_comment (surfacing) vs likes, vs replies.
# (2) Same reward models (likes, replies negbinom) RESTRICTED to non–Top Comment
#     comments only. Compare IRRs to full sample to see if reward effects hold
#     when visibility is not driving (i.e. among comments that were NOT surfaced).
#
# SUPPLEMENTAL_ANALYSES_TO_RUN.md §2.1
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)

source("analysis/setup/load_data.R")

# Top-level YouTube only (surfacing defined only for top-level)
yt_top <- joined_data %>%
  filter(platform == "YouTube" | is.na(platform)) %>%
  mutate(
    is_reply = !is.na(parent_comment_id) & parent_comment_id != "",
    top_comment_binary = as.integer(top_comment == 1 | top_comment == TRUE),
    comment_replies = as.numeric(replies),
    comment_likes = as.numeric(like_count)
  ) %>%
  filter(!is_reply) %>%
  filter(!is.na(comment_replies), !is.na(comment_likes))

# ---- (1) Cross-correlations ----
cat("=== NOUR §2.1: SURFACING vs LIKES/REPLIES ===\n\n")
cat("N top-level YouTube comments:", nrow(yt_top), "\n")
cat("Top Comment rate (surfaced):", mean(yt_top$top_comment_binary, na.rm = TRUE), "\n\n")

cat("Correlation top_comment vs log(likes+1):", cor(yt_top$top_comment_binary, log1p(yt_top$comment_likes), use = "pairwise.complete.obs"), "\n")
cat("Correlation top_comment vs log(replies+1):", cor(yt_top$top_comment_binary, log1p(yt_top$comment_replies), use = "pairwise.complete.obs"), "\n\n")

# ---- (2) Non-surfaced only: same negbinom ----
non_surfaced <- yt_top %>% filter(top_comment_binary == 0)
cat("N non–Top Comment (non-surfaced):", nrow(non_surfaced), "\n\n")

cat("=== LIKES: non-surfaced only ===\n")
mod_likes_ns <- glmer.nb(
  comment_likes ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = non_surfaced,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
print(summary(mod_likes_ns))
coefs_l <- fixef(mod_likes_ns)
ses_l <- sqrt(diag(vcov(mod_likes_ns)))
irr_l <- exp(coefs_l)
ci_l <- exp(coefs_l + outer(ses_l, c(-1.96, 1.96)))
cat("Constructiveness IRR (non-surfaced):", round(irr_l[2], 3), "95% CI [", round(ci_l[2,1], 3), ",", round(ci_l[2,2], 3), "]\n")
cat("Destructiveness IRR (non-surfaced):", round(irr_l[3], 3), "95% CI [", round(ci_l[3,1], 3), ",", round(ci_l[3,2], 3), "]\n\n")

cat("=== REPLIES: non-surfaced only ===\n")
mod_replies_ns <- glmer.nb(
  comment_replies ~ harmoniousness_raw + divisiveness_raw + (1|video_id),
  data = non_surfaced,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
print(summary(mod_replies_ns))
coefs_r <- fixef(mod_replies_ns)
ses_r <- sqrt(diag(vcov(mod_replies_ns)))
irr_r <- exp(coefs_r)
ci_r <- exp(coefs_r + outer(ses_r, c(-1.96, 1.96)))
cat("Constructiveness IRR (non-surfaced):", round(irr_r[2], 3), "95% CI [", round(ci_r[2,1], 3), ",", round(ci_r[2,2], 3), "]\n")
cat("Destructiveness IRR (non-surfaced):", round(irr_r[3], 3), "95% CI [", round(ci_r[3,1], 3), ",", round(ci_r[3,2], 3), "]\n\n")

cat("Compare to main RQ2: full sample IRRs ~ 3.13 (likes), 5.28 (replies) for constructiveness. If non-surfaced IRRs are similar, reward is not only due to visibility.\n")

results <- list(
  n_top_level = nrow(yt_top),
  n_non_surfaced = nrow(non_surfaced),
  cor_surfacing_likes = cor(yt_top$top_comment_binary, log1p(yt_top$comment_likes), use = "pairwise.complete.obs"),
  cor_surfacing_replies = cor(yt_top$top_comment_binary, log1p(yt_top$comment_replies), use = "pairwise.complete.obs"),
  mod_likes_non_surfaced = mod_likes_ns,
  mod_replies_non_surfaced = mod_replies_ns,
  irr_constructiveness_likes_ns = irr_l[2],
  irr_constructiveness_replies_ns = irr_r[2]
)
saveRDS(results, "analysis/engagement/RQ2_non_surfaced_only_results.rds")
cat("Results saved to RQ2_non_surfaced_only_results.rds\n")
