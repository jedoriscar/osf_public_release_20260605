# Purpose
# Generate Figure 5: Grouped bar chart showing mean constructiveness and destructiveness
# by agreement category (Agree, Disagree, Mixed, Neither).
#
# Reference: Main text Figure X (around line 133), lines 133-139
# Output: Grouped bar chart with all four categories

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Load data
source("analysis/setup/load_data.R")

# Use canonical data when agreement_label is present (matches ROB1)
if ("agreement_label" %in% colnames(joined_data)) {
  analysis_data <- joined_data %>%
    filter(!is.na(agreement_label), !is.na(harmoniousness_raw), !is.na(divisiveness_raw)) %>%
    select(comment_id, agreement_label, harmoniousness_raw, divisiveness_raw)
} else {
  agreement_file <- "supplemental_materials/04_extracted_data/agreement_data.csv"
  if (!file.exists(agreement_file)) {
    stop("Agreement classification not in joined_data. Run prepare_canonical_data.R or provide agreement_data.csv.")
  }
  agreement_data <- read.csv(agreement_file) %>% select(comment_id, agreement_label)
  analysis_data <- agreement_data %>%
    left_join(
      joined_data %>% select(comment_id, harmoniousness_raw, divisiveness_raw),
      by = "comment_id"
    ) %>%
    filter(!is.na(harmoniousness_raw), !is.na(divisiveness_raw))
}

# Calculate means by category
agreement_summary <- analysis_data %>%
  group_by(agreement_label) %>%
  summarize(
    mean_C = mean(harmoniousness_raw, na.rm = TRUE),
    mean_D = mean(divisiveness_raw, na.rm = TRUE),
    se_C = sd(harmoniousness_raw, na.rm = TRUE) / sqrt(n()),
    se_D = sd(divisiveness_raw, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    agreement_label = factor(agreement_label, 
                            levels = c("Agree", "Disagree", "Mixed", "Neither"))
  )

cat("=== FIGURE 5: AGREEMENT ANALYSIS ===\n")
print(agreement_summary)

# Reshape for plotting
plot_data <- agreement_summary %>%
  pivot_longer(
    cols = c(mean_C, mean_D),
    names_to = "discourse_type",
    values_to = "mean_value"
  ) %>%
  mutate(
    discourse_type = recode(discourse_type, "mean_C" = "Constructive-Feature Index", "mean_D" = "Destructive-Feature Index"),
    se = ifelse(discourse_type == "Constructive-Feature Index", se_C, se_D)
  ) %>%
  select(agreement_label, discourse_type, mean_value, se)

# Create plot
constructiveness_color <- "#1f77b4"
destructiveness_color <- "#ff7f0e"

fig5 <- ggplot(plot_data, aes(x = agreement_label, y = mean_value, fill = discourse_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(aes(label = sprintf("%.1f%%", mean_value * 100)), position = position_dodge(width = 0.9),
            vjust = -1.2, size = 3.5, fontface = "plain") +
  scale_fill_manual(values = c("Constructive-Feature Index" = constructiveness_color,
                               "Destructive-Feature Index" = destructiveness_color)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Constructive Features Appear Across Agreement Categories",
    subtitle = sprintf("Mean constructive- vs. destructive-feature index by child reply stance (N = %s comments)", format(nrow(analysis_data), big.mark = ",")),
    x = "Agreement Category",
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

ggsave(file.path(output_dir, "Figure5_Agreement_Analysis.png"),
       fig5, width = 8, height = 6, dpi = 300)

cat("\n✓ Figure 5 saved!\n")
