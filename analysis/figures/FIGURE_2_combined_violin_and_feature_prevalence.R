# Purpose
# Combine Figure 2 (violin C vs D) and feature prevalence into ONE figure:
#   Panel A: Violin plot of constructiveness vs destructiveness, side by side by platform (YouTube | TikTok).
#   Panel B: Feature prevalence bars with numbers on top, YouTube and TikTok shown side by side (facets).
# Saves one image for main text; caption should describe both panels and note N, platforms, and key stats.

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)
library(patchwork)

# Load data
source("analysis/setup/load_data.R")

constructiveness_color <- "#1f77b4"
destructiveness_color <- "#ff7f0e"

theme_fig <- function() {
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "top"
  )
}

# Restrict to comments with platform for both panels (so YouTube vs TikTok is comparable)
analysis_data <- joined_data %>%
  filter(!is.na(platform))

# Panel A: Violin C vs D by platform (YouTube | TikTok)
fig2a_data <- analysis_data %>%
  select(platform, harmoniousness_raw, divisiveness_raw) %>%
  pivot_longer(
    cols = c(harmoniousness_raw, divisiveness_raw),
    names_to = "discourse_type",
    values_to = "score"
  ) %>%
  mutate(
    discourse_type = factor(
      discourse_type,
      levels = c("harmoniousness_raw", "divisiveness_raw"),
      labels = c("Constructive-Feature Index", "Destructive-Feature Index")
    ),
    platform = factor(platform, levels = c("YouTube", "TikTok"))
  )

pA <- ggplot(fig2a_data, aes(x = discourse_type, y = score, fill = discourse_type)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.1, alpha = 0.8, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5, fill = "white") +
  scale_fill_manual(values = c("Constructive-Feature Index" = constructiveness_color,
                               "Destructive-Feature Index" = destructiveness_color), name = NULL) +
  facet_wrap(~ platform, ncol = 2, strip.position = "top") +
  labs(x = NULL, y = "Proportion of Features Present (0-1)") +
  theme_fig() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Panel B: Feature prevalence by platform (YouTube | TikTok), numbers on bars
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

prev_by_platform <- map_dfr(1:nrow(feature_spec), function(i) {
  col <- feature_spec$prob_col[i]
  label <- feature_spec$display_label[i]
  cat <- feature_spec$category[i]
  if (!col %in% names(analysis_data)) return(NULL)
  analysis_data %>%
    group_by(platform) %>%
    summarise(
      n_above = sum(.data[[col]] >= 0.6, na.rm = TRUE),
      n_total = sum(!is.na(.data[[col]])),
      prevalence_pct = 100 * mean(.data[[col]] >= 0.6, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(feature = label, category = cat)
}) %>%
  filter(!is.na(feature))

# Order features by overall prevalence (average across platforms) so order is consistent across facets
feature_order <- prev_by_platform %>%
  group_by(feature) %>%
  summarise(avg_pct = mean(prevalence_pct), .groups = "drop") %>%
  arrange(desc(avg_pct)) %>%
  pull(feature) %>%
  rev()

prev_by_platform <- prev_by_platform %>%
  mutate(
    feature = factor(feature, levels = feature_order),
    category = factor(category, levels = c("Constructive", "Destructive")),
    platform = factor(platform, levels = c("YouTube", "TikTok"))
  )

pB <- ggplot(prev_by_platform, aes(x = prevalence_pct, y = feature, fill = category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", prevalence_pct)), hjust = -0.12, size = 3.2, fontface = "plain") +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray90", linewidth = 0.5) +
  scale_fill_manual(
    values = c("Constructive" = constructiveness_color, "Destructive" = destructiveness_color),
    name = NULL
  ) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~ platform, ncol = 2, strip.position = "top") +
  labs(x = "Prevalence (%)", y = NULL) +
  theme_fig() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "plain")
  )

# Combine and save (side by side, no titles, legends kept)
combined <- pA + pB

output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(
  file.path(output_dir, "Figure2_Combined_Violin_Feature_Prevalence.png"),
  combined, width = 16, height = 7, dpi = 300
)

# Also write by-platform prevalence data for reference
write.csv(
  prev_by_platform %>% select(feature, category, platform, prevalence_pct, n_above, n_total),
  file.path(output_dir, "Figure_Feature_Prevalence_By_Platform_data.csv"),
  row.names = FALSE
)

cat("✓ Figure 2 (combined violin + feature prevalence) saved to:\n")
cat("  ", file.path(output_dir, "Figure2_Combined_Violin_Feature_Prevalence.png"), "\n")
cat("✓ By-platform prevalence data saved to:\n")
cat("  ", file.path(output_dir, "Figure_Feature_Prevalence_By_Platform_data.csv"), "\n")
