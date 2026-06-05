# Purpose
# Generate a presentation-style RQ2 reward figure as a horizontal bar chart
# (paired bars per outcome) showing IRRs for Likes and Replies.
# Style matches existing `09_figures/` exports (theme_minimal base_size=14, gray40
# dashed reference line at 1.0, consistent axis/legend sizing).
#
# Output:
# - analysis/figures/outputs/Figure_RQ2_Reward_Bars_Presentation.png
#
# Values (provided):
# - Likes:   Constructive IRR = 3.13, Destructive IRR = 0.43
# - Replies: Constructive IRR = 5.28, Destructive IRR = 0.51

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Prepare data
constructive_color <- "#2D6A4F"  # green
destructive_color  <- "#C55A11"  # orange

rq2_bars <- tibble::tribble(
  ~outcome,   ~discourse_type, ~irr,
  "Likes",    "Constructive",  3.13,
  "Likes",    "Destructive",   0.43,
  "Replies",  "Constructive",  5.28,
  "Replies",  "Destructive",   0.51
) %>%
  mutate(
    # Put Likes on TOP (ggplot draws first factor level at bottom).
    outcome = factor(outcome, levels = c("Replies", "Likes")),
    discourse_type = factor(discourse_type, levels = c("Constructive", "Destructive"))
  )

# Create plot
fig_rq2 <- ggplot(rq2_bars, aes(x = irr, y = outcome, fill = discourse_type)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(label = sprintf("%.2f", irr)),
    position = position_dodge(width = 0.75),
    hjust = -0.1,
    size = 4.2,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(
    values = c("Constructive" = constructive_color, "Destructive" = destructive_color)
  ) +
  guides(
    fill = guide_legend(reverse = TRUE, override.aes = list(alpha = 1))
  ) +
  scale_x_log10(
    limits = c(0.1, 10),
    breaks = c(0.4, 0.5, 1, 2, 5, 10),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Incidence Rate Ratio (IRR)",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.9, "lines"),
    legend.spacing.x = unit(0.5, "lines"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 22, 10, 10)
  )

# Save
output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

out_path <- file.path(output_dir, "Figure_RQ2_Reward_Bars_Presentation.png")

# Match 2400×1400px at dpi=300 -> 8×4.6667 inches
ggsave(out_path, fig_rq2, width = 8, height = 1400 / 300, dpi = 300)

# Verification
cat("✓ RQ2 reward bars saved:", out_path, "\n")
print(rq2_bars)

