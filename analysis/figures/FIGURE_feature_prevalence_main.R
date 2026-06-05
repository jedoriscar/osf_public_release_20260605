# Purpose
# Main figure: Feature-level prevalence collapsed across platforms.
# Horizontal bar chart, one bar per feature, ordered by prevalence (highest to lowest).
# Color shows constructive (blue) vs destructive (orange).
# Core message: constructive > destructive.
#
# Reference: Main text RQ1
# Output: Simple bar chart for main text

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Load data
source("analysis/setup/load_data.R")

analysis_data <- joined_data

# Feature definitions
feature_spec <- tribble(
  ~prob_col,              ~display_label,              ~category,
  "prob_compassion",      "Compassion",                "Constructive",
  "prob_curiosity",       "Curiosity",                 "Constructive",
  "prob_nuance",         "Nuance",                    "Constructive",
  "prob_personal_story",  "Personal story",            "Constructive",
  "prob_reasoning",      "Reasoning",                 "Constructive",
  "prob_toxic",          "Toxicity",                  "Destructive",
  "prob_identity_attack", "Identity attack",           "Destructive",
  "prob_threat",         "Threat",                    "Destructive",
  "prob_attack_on_author", "Attack on author",        "Destructive",
  "prob_attack_on_commenter", "Attack on commenter",  "Destructive"
)

# Calculate prevalence (collapsed across platforms)
prev_main <- map_dfr(1:nrow(feature_spec), function(i) {
  col <- feature_spec$prob_col[i]
  label <- feature_spec$display_label[i]
  cat <- feature_spec$category[i]
  if (!col %in% names(analysis_data)) return(NULL)
  
  analysis_data %>%
    summarise(
      n_above = sum(.data[[col]] >= 0.6, na.rm = TRUE),
      n_total = sum(!is.na(.data[[col]])),
      prevalence_pct = 100 * mean(.data[[col]] >= 0.6, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      feature = label,
      category = cat
    )
}) %>%
  filter(!is.na(feature))

# Order by prevalence only (highest to lowest); color shows constructive vs destructive
feature_levels <- prev_main %>%
  distinct(feature, category, prevalence_pct) %>%
  arrange(desc(prevalence_pct)) %>%
  pull(feature) %>%
  rev()  # first level = bottom in ggplot, so rev for highest at top

prev_main <- prev_main %>%
  mutate(
    feature = factor(feature, levels = feature_levels),
    category = factor(category, levels = c("Constructive", "Destructive"))
  )

# Create plot
constructive_color <- "#1f77b4"
destructive_color <- "#ff7f0e"

fig <- ggplot(prev_main, aes(x = prevalence_pct, y = feature, fill = category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", prevalence_pct)), hjust = -0.15, size = 3.8, fontface = "plain") +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray90", linewidth = 0.5) +
  scale_fill_manual(
    values = c("Constructive" = constructive_color, "Destructive" = destructive_color),
    name = NULL
  ) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Constructive Features Substantially Exceed Destructive Features",
    subtitle = "Proportion of comments with each feature (Perspective API probability ≥ 0.6). Ordered by prevalence. N = 101,103 comments.",
    x = "Prevalence (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13),
    axis.text.y = element_text(face = "plain"),
    legend.text = element_text(size = 13)
  )

# Save figure
output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(
  file.path(output_dir, "Figure_Feature_Prevalence_Main.png"),
  fig, width = 8, height = 7, dpi = 300
)

write.csv(
  prev_main %>% select(feature, category, prevalence_pct, n_above, n_total),
  file.path(output_dir, "Figure_Feature_Prevalence_Main_data.csv"),
  row.names = FALSE
)

cat("✓ Main figure saved to:", file.path(output_dir, "Figure_Feature_Prevalence_Main.png"), "\n")
cat("\nFeature prevalence (ordered by prevalence):\n")
print(prev_main %>% select(feature, category, prevalence_pct))
