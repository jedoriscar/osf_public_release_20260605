# Purpose
# Generate Figure 6: Grouped bar chart showing mean constructiveness and destructiveness
# by diversity stance (Pro-Diversity, Anti-Diversity, Neutral/Unclear).
#
# Reference: Main text Figure X (around line 151), lines 151-157
# Output: Grouped bar chart with all three categories

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Load data
source("analysis/setup/load_data.R")

# Load stance classification data
stance_file <- "data/model_labels/racial_stance_labels.csv"
if (!file.exists(stance_file)) {
  stop("Stance classification file not found.")
}

stance_ids <- read.csv(stance_file, stringsAsFactors = FALSE) %>%
  select(comment_id, stance_label) %>%
  mutate(
    sl = trimws(coalesce(as.character(stance_label), "")),
    stance_label = if_else(sl != "" & !toupper(sl) %in% c("NA", "N/A"), sl, NA_character_)
  ) %>%
  select(comment_id, stance_label) %>%
  filter(!is.na(stance_label))

# Merge with main data (canonical harmoniousness_raw/divisiveness_raw)
analysis_data <- stance_ids %>%
  left_join(
    joined_data %>%
      select(comment_id, harmoniousness_raw, divisiveness_raw),
    by = "comment_id"
  ) %>%
  filter(!is.na(harmoniousness_raw), !is.na(divisiveness_raw))

# Calculate means by category
stance_summary <- analysis_data %>%
  group_by(stance_label) %>%
  summarize(
    mean_C = mean(harmoniousness_raw, na.rm = TRUE),
    mean_D = mean(divisiveness_raw, na.rm = TRUE),
    se_C = sd(harmoniousness_raw, na.rm = TRUE) / sqrt(n()),
    se_D = sd(divisiveness_raw, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(stance_label %in% c("Pro-Diversity", "Neutral/Unclear", "Anti-Diversity")) %>%
  mutate(
    stance_label = factor(stance_label,
                          levels = c("Pro-Diversity", "Neutral/Unclear", "Anti-Diversity"))
  )

cat("=== FIGURE 6: STANCE ANALYSIS ===\n")
print(stance_summary)

# Reshape for plotting
plot_data <- stance_summary %>%
  pivot_longer(
    cols = c(mean_C, mean_D),
    names_to = "discourse_type",
    values_to = "mean_value"
  ) %>%
  mutate(
    discourse_type = recode(discourse_type, "mean_C" = "Constructive-Feature Index", "mean_D" = "Destructive-Feature Index"),
    se = ifelse(discourse_type == "Constructive-Feature Index", se_C, se_D)
  ) %>%
  select(stance_label, discourse_type, mean_value, se)

# Create plot
constructiveness_color <- "#1f77b4"
destructiveness_color <- "#ff7f0e"

fig6 <- ggplot(plot_data, aes(x = stance_label, y = mean_value, fill = discourse_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(
    aes(y = mean_value + se + 0.022, label = sprintf("%.1f%%", mean_value * 100)),
    position = position_dodge(width = 0.9),
    vjust = 0,
    size = 3.5,
    fontface = "plain"
  ) +
  scale_fill_manual(values = c("Constructive-Feature Index" = constructiveness_color,
                               "Destructive-Feature Index" = destructiveness_color)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.24))) +
  labs(
    title = "Constructive Features Across Ideological Positions",
    subtitle = sprintf("Mean constructive- vs. destructive-feature index by stance (N = %s comments)", format(nrow(analysis_data), big.mark = ",")),
    x = "Diversity Stance",
    y = "Proportion of Features Present (0-1)",
    fill = NULL
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

# Save figure
output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(output_dir, "Figure6_Stance_Analysis.png"),
       fig6, width = 8, height = 6, dpi = 300)

cat("\n✓ Figure 6 saved!\n")
