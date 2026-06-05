# Purpose
# Topic-level figure: horizontal diverging bar chart.
# Raw weighted scores only (no z-scores, no range lines). Constructiveness (blue) right from 0,
# destructiveness (orange) left from 0. Values at bar ends; readable topic names; larger fonts.

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Load data
source("analysis/setup/load_data.R")

# Topic columns (match RQ1: topic_N_name, exclude _z)
topic_cols <- grep("^topic_-?\\d+_", names(joined_data), value = TRUE)
topic_cols <- topic_cols[!grepl("_z$", topic_cols)]
if (length(topic_cols) == 0) {
  topic_cols <- grep("^topic_[0-9]+_prob$", names(joined_data), value = TRUE)
}
if (length(topic_cols) == 0) {
  stop("Topic probability columns not found.")
}

# Topic-level weighted means (raw 0-1 scale)
topic_data_long <- joined_data %>%
  select(comment_id, harmoniousness_raw, divisiveness_raw, all_of(topic_cols)) %>%
  pivot_longer(
    cols = all_of(topic_cols),
    names_to = "topic_col",
    values_to = "topic_probability"
  ) %>%
  filter(topic_probability >= 0.25) %>%
  mutate(
    weighted_C = harmoniousness_raw * topic_probability,
    weighted_D = divisiveness_raw * topic_probability
  )

topic_weighted <- topic_data_long %>%
  group_by(topic_col) %>%
  summarize(
    constructiveness = sum(weighted_C, na.rm = TRUE) / sum(topic_probability, na.rm = TRUE),
    destructiveness = sum(weighted_D, na.rm = TRUE) / sum(topic_probability, na.rm = TRUE),
    n_comments = n(),
    .groups = "drop"
  ) %>%
  mutate(
    topic_num = as.numeric(sub("^topic_(-?\\d+).*", "\\1", topic_col)),
    topic_name_raw = gsub("^topic_-?\\d+_", "", gsub("_prob$", "", topic_col)),
    topic_display = gsub("_", " ", topic_name_raw) %>%
      gsub("\\b(.)", "\\U\\1", ., perl = TRUE)
  ) %>%
  arrange(topic_num)

# Order for plot: by constructiveness (highest at top) so layout matches reference
topic_order <- topic_weighted %>%
  arrange(desc(constructiveness)) %>%
  pull(topic_display)
topic_weighted <- topic_weighted %>%
  mutate(topic_display = factor(topic_display, levels = rev(topic_order)))

# Long format for geom_col: one row per topic per type, with signed x for diverging bars
plot_data <- topic_weighted %>%
  pivot_longer(
    cols = c(constructiveness, destructiveness),
    names_to = "discourse_type",
    values_to = "score"
  ) %>%
  mutate(
    discourse_type = factor(
      discourse_type,
      levels = c("destructiveness", "constructiveness"),
      labels = c("Destructive-Feature Index", "Constructive-Feature Index")
    ),
    score_signed = ifelse(discourse_type == "Constructive-Feature Index", score, -score)
  )

# Plot: diverging horizontal bars, raw values, value labels
# Talk color scheme (match other presentation plots)
constructiveness_color <- "#2D6A4F"  # green
destructiveness_color <- "#C55A11"   # orange

fig2 <- ggplot(plot_data, aes(x = score_signed, y = topic_display, fill = discourse_type)) +
  geom_col(position = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray20", linewidth = 0.6) +
  geom_text(
    aes(label = sprintf("%.3f", score), x = score_signed),
    hjust = ifelse(plot_data$score_signed >= 0, -0.12, 1.12),
    size = 4,
    fontface = "plain"
  ) +
  scale_fill_manual(
    values = c("Constructive-Feature Index" = constructiveness_color, "Destructive-Feature Index" = destructiveness_color),
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(NA, NA),
    expand = expansion(mult = c(0.12, 0.12)),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  labs(
    title = "Constructive Features Are Prevalent Across 18 Discussion Topics",
    subtitle = "Probability-weighted constructive- vs. destructive-feature index by BERTopic cluster",
    x = "Probability-weighted index (0–1)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12, color = "gray35"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    axis.text.y = element_text(face = "plain", size = 12),
    legend.text = element_text(size = 13)
  ) +
  coord_cartesian(clip = "off")

# Save
output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(
  file.path(output_dir, "Figure2_Topic_Level_Prevalence_GreenOrange.png"),
  fig2, width = 11, height = 8, dpi = 300
)

cat("✓ Topic-level figure saved (raw weighted scores, no z-scores or range lines).\n")
cat("  N topics:", nrow(topic_weighted), "\n")
