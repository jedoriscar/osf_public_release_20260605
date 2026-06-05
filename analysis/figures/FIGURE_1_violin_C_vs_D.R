# Purpose
# Generate Figure 1: Violin plot comparing constructiveness vs. destructiveness overall.
# Shows distribution of discourse features across all comments.
#
# Reference: Main text Figure 1, lines 58-62
# Output: Violin plot with overlaid boxplots, mean values marked with white diamonds

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Load data
source("analysis/setup/load_data.R")

# Prepare data
fig1_data <- joined_data %>%
  select(harmoniousness_raw, divisiveness_raw) %>%
  pivot_longer(
    cols = everything(),
    names_to = "discourse_type",
    values_to = "score"
  ) %>%
  mutate(
    discourse_type = factor(
      discourse_type,
      levels = c("harmoniousness_raw", "divisiveness_raw"),
      labels = c("Constructive-Feature Index", "Destructive-Feature Index")
    )
  )

# Calculate means for labels
means <- fig1_data %>%
  group_by(discourse_type) %>%
  summarize(mean_score = mean(score, na.rm = TRUE))

cat("=== FIGURE 1: OVERALL C VS D VIOLIN PLOT ===\n")
cat("N =", nrow(joined_data), "comments\n")
cat("Mean constructive-feature index:", round(means$mean_score[means$discourse_type == "Constructive-Feature Index"], 3), "\n")
cat("Mean destructive-feature index:", round(means$mean_score[means$discourse_type == "Destructive-Feature Index"], 3), "\n\n")

# Create plot
constructiveness_color <- "#1f77b4"  # Blue
destructiveness_color <- "#ff7f0e"  # Orange

fig1 <- ggplot(fig1_data, aes(x = discourse_type, y = score, fill = discourse_type)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.1, alpha = 0.8, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  scale_fill_manual(values = c("Constructive-Feature Index" = constructiveness_color,
                                 "Destructive-Feature Index" = destructiveness_color), name = NULL) +
  labs(
    title = "The Constructive-Feature Index Exceeds the Destructive-Feature Index",
    subtitle = "Distribution of index scores across 101,103 comments",
    x = NULL,
    y = "Proportion of Features Present (0-1)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 13, color = "gray30"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Save figure
output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(output_dir, "Figure1_Overall_C_vs_D_violin.png"),
       fig1, width = 8, height = 6, dpi = 300)

cat("✓ Figure 1 saved to:", file.path(output_dir, "Figure1_Overall_C_vs_D_violin.png"), "\n")

# ALTERNATIVE 1: Bar chart of discrete distribution (0, 0.2, ..., 1.0)
# Index is proportion of 5 features, so discrete levels 0, 1/5, 2/5, 3/5, 4/5, 5/5
fig1_data_discrete <- fig1_data %>%
  mutate(level = round(score * 5) / 5) %>%
  group_by(discourse_type, level) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(discourse_type) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(level = factor(level, levels = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)))

fig1_bars <- ggplot(fig1_data_discrete, aes(x = level, y = pct, fill = discourse_type)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.75, alpha = 0.85) +
  scale_fill_manual(values = c("Constructive-Feature Index" = constructiveness_color,
                              "Destructive-Feature Index" = destructiveness_color), name = NULL) +
  labs(
    title = "The Constructive-Feature Index Exceeds the Destructive-Feature Index",
    subtitle = "Distribution over discrete levels (proportion of 5 features present). N = 101,103 comments.",
    x = "Proportion of Features Present (0, 0.2, …, 1.0)",
    y = "Percent of Comments"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 13, color = "gray30"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 13)
  )

ggsave(file.path(output_dir, "Figure1_Overall_C_vs_D_discrete_bars.png"),
       fig1_bars, width = 8, height = 6, dpi = 300)
cat("✓ Alternative 1 (discrete bars) saved to:", file.path(output_dir, "Figure1_Overall_C_vs_D_discrete_bars.png"), "\n")

# ALTERNATIVE 2: Box plot only (no violin)
fig1_box <- ggplot(fig1_data, aes(x = discourse_type, y = score, fill = discourse_type)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.alpha = 0.25) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white") +
  scale_fill_manual(values = c("Constructive-Feature Index" = constructiveness_color,
                               "Destructive-Feature Index" = destructiveness_color)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "The Constructive-Feature Index Exceeds the Destructive-Feature Index",
    subtitle = "Box plots (median, IQR, range). White diamonds = means. N = 101,103 comments.",
    x = NULL,
    y = "Proportion of Features Present (0-1)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 13, color = "gray30"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  )

ggsave(file.path(output_dir, "Figure1_Overall_C_vs_D_box_only.png"),
       fig1_box, width = 8, height = 6, dpi = 300)
cat("✓ Alternative 2 (box only) saved to:", file.path(output_dir, "Figure1_Overall_C_vs_D_box_only.png"), "\n")

# Figure note (for manuscript)
cat("\n=== FIGURE NOTE ===\n")
cat("Violin plots with overlaid boxplots showing the distribution of the constructive-\n")
cat("feature index (blue) and destructive-feature index (orange) across all 101,103 comments. Mean values are\n")
cat("marked with white diamonds (constructive-feature index: M = ", 
    round(means$mean_score[means$discourse_type == "Constructive-Feature Index"], 3),
    ", destructive-feature index: M = ",
    round(means$mean_score[means$discourse_type == "Destructive-Feature Index"], 3),
    "). The wider portions of the violin plots indicate where more comments fall;\n")
cat("the constructive-feature index shows consistently higher values than the destructive-feature index across\n")
cat("the full distribution.\n")
