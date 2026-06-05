# Purpose
# Generate a compact Figure 4 option for engagement and surfacing models.
# The plot combines Top Comment surfacing (odds ratio) with likes/replies
# (incidence rate ratios) on a shared log effect-ratio axis.

rm(list = ls())
library(tidyverse)
library(ggplot2)

cat("=== FIGURE 4 OPTION: COMPACT EFFECT-RATIO PLOT ===\n")

results_surfacing <- readRDS("analysis/engagement/RQ2_algorithmic_surfacing_results.rds")
results_likes <- readRDS("analysis/engagement/RQ2_likes_negbinom_results.rds")
results_replies <- readRDS("analysis/engagement/RQ2_replies_negbinom_results.rds")

figure_data <- tibble(
  feature = rep(c("Constructive-Feature Index", "Destructive-Feature Index"), each = 3),
  outcome = rep(c("Top Comment", "Likes", "Replies"), times = 2),
  ratio_type = rep(c("OR", "IRR", "IRR"), times = 2),
  estimate = c(
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
    outcome_label = factor(
      paste0(outcome, " (", ratio_type, ")"),
      levels = c("Replies (IRR)", "Likes (IRR)", "Top Comment (OR)")
    ),
    feature = factor(
      feature,
      levels = c("Constructive-Feature Index", "Destructive-Feature Index")
    )
  )

feature_colors <- c(
  "Constructive-Feature Index" = "#1f77b4",
  "Destructive-Feature Index" = "#ff7f0e"
)

feature_shapes <- c(
  "Constructive-Feature Index" = 16,
  "Destructive-Feature Index" = 15
)

fig4_compact <- ggplot(
  figure_data,
  aes(x = estimate, y = outcome_label, color = feature, shape = feature)
) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray45", linewidth = 0.6) +
  geom_errorbar(
    aes(xmin = ci_lower, xmax = ci_upper),
    position = position_dodge(width = 0.45),
    width = 0.18,
    linewidth = 0.8,
    orientation = "y"
  ) +
  geom_point(position = position_dodge(width = 0.45), size = 3.3) +
  scale_x_log10(
    breaks = c(0.07, 0.1, 0.2, 0.5, 1, 2, 5),
    labels = c("0.07", "0.10", "0.20", "0.50", "1", "2", "5"),
    limits = c(0.06, 6.5)
  ) +
  scale_color_manual(values = feature_colors) +
  scale_shape_manual(values = feature_shapes) +
  labs(
    x = "Effect ratio on log scale (OR for Top Comment; IRR for Likes and Replies)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(size = 10, color = "gray20"),
    axis.title.x = element_text(size = 10, margin = margin(t = 8)),
    plot.margin = margin(8, 16, 8, 8)
  )

output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

png_path <- file.path(output_dir, "Figure4_Effect_Ratio_Compact.png")
pdf_path <- file.path(output_dir, "Figure4_Effect_Ratio_Compact.pdf")

ggsave(png_path, fig4_compact, width = 7.2, height = 3.8, dpi = 300)
ggsave(pdf_path, fig4_compact, width = 7.2, height = 3.8)

cat("Saved:", png_path, "\n")
cat("Saved:", pdf_path, "\n")
print(figure_data)
