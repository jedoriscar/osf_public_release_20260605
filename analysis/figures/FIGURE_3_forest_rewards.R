# Purpose
# Generate Figure 4: Forest plot showing constructiveness and destructiveness
# predicting Top Comment surfacing, likes, and replies (effect ratios on x-axis).
# Uses the SAME joint models as the manuscript text (both C and D in each model).
#
# Reference: Main text Figure 4
# Output: Forest plot with OR/IRR on x-axis, surfacing/likes/replies as facets

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Load RQ2 results (joint models: C + D in same model)
cat("=== FIGURE 4: FOREST PLOT FOR ENGAGEMENT AND SURFACING ===\n")
cat("Loading IRRs from RQ2 joint models (matches manuscript text)...\n\n")

results_surfacing <- readRDS("analysis/engagement/RQ2_algorithmic_surfacing_results.rds")
results_likes <- readRDS("analysis/engagement/RQ2_likes_negbinom_results.rds")
results_replies <- readRDS("analysis/engagement/RQ2_replies_negbinom_results.rds")

# Extract IRRs and CIs from joint models (ci is 2-element: lower, upper)
fig3_data <- data.frame(
  discourse_type = rep(c("Constructive-Feature Index", "Destructive-Feature Index"), each = 3),
  outcome = rep(c("Top Comment", "Likes", "Replies"), times = 2),
  irr = c(
    results_surfacing$constructiveness$or,
    results_likes$constructiveness$irr,
    results_replies$constructiveness$irr,
    results_surfacing$destructiveness$or,
    results_likes$destructiveness$irr,
    results_replies$destructiveness$irr
  ),
  ci_lower = c(
    results_surfacing$constructiveness$ci[1],
    results_likes$constructiveness$ci[1],
    results_replies$constructiveness$ci[1],
    results_surfacing$destructiveness$ci[1],
    results_likes$destructiveness$ci[1],
    results_replies$destructiveness$ci[1]
  ),
  ci_upper = c(
    results_surfacing$constructiveness$ci[2],
    results_likes$constructiveness$ci[2],
    results_replies$constructiveness$ci[2],
    results_surfacing$destructiveness$ci[2],
    results_likes$destructiveness$ci[2],
    results_replies$destructiveness$ci[2]
  )
) %>%
  mutate(
    discourse_type = factor(discourse_type, levels = c("Destructive-Feature Index", "Constructive-Feature Index")),
    outcome = factor(outcome, levels = c("Top Comment", "Likes", "Replies"))
  )

# Create plot
constructiveness_color <- "#1f77b4"
destructiveness_color <- "#ff7f0e"

fig3 <- ggplot(fig3_data, aes(x = irr, y = discourse_type, color = discourse_type)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, linewidth = 0.8) +
  geom_point(size = 4) +
  facet_wrap(~ outcome, ncol = 3) +
  scale_color_manual(values = c("Constructive-Feature Index" = constructiveness_color,
                                 "Destructive-Feature Index" = destructiveness_color)) +
  scale_y_discrete(limits = rev(levels(fig3_data$discourse_type))) +
  labs(
    title = "Constructive Features Are Associated With More Engagement and Surfacing",
    subtitle = sprintf("OR for Top Comment (N = %s); IRRs for likes/replies (N = %s)",
                      format(results_surfacing$n, big.mark = ","),
                      format(results_likes$n, big.mark = ",")),
    x = "Effect Ratio (OR for Top Comment; IRR for Likes and Replies)",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold")
  )

# Save figure
output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(output_dir, "Figure3_Reward_Patterns.png"),
       fig3, width = 10, height = 7.5, dpi = 300)

cat("✓ Figure 4 saved!\n")
cat("ORs (Top Comment): C =", round(results_surfacing$constructiveness$or, 2),
    ", D =", round(results_surfacing$destructiveness$or, 2), "\n")
cat("IRRs (Likes):  C =", round(results_likes$constructiveness$irr, 2),
    ", D =", round(results_likes$destructiveness$irr, 2), "\n")
cat("IRRs (Replies): C =", round(results_replies$constructiveness$irr, 2),
    ", D =", round(results_replies$destructiveness$irr, 2), "\n")
