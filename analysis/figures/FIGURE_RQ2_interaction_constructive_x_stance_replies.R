# Purpose
# Hidden backup slide: interaction plot showing Constructive-Feature Index ×
# Diversity stance predicting predicted replies.
#
# Output:
# - analysis/figures/outputs/Figure_RQ2_Interaction_Constructive_x_Stance_Replies.png
#
# Model: Negative binomial GLMM with random intercept by video_id:
#   replies ~ stance_binary + harmoniousness_raw + stance_binary:harmoniousness_raw + (1|video_id)

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)
library(lme4)

# Load data
source("analysis/setup/load_data.R")

stance_file <- "data/model_labels/racial_stance_labels.csv"
if (!file.exists(stance_file)) stop("Stance file not found at: ", stance_file)

stance_data <- read.csv(stance_file)
stance_lookup <- stance_data %>%
  select(comment_id, stance_label, any_of("topic_0_prob"))

analysis_data <- joined_data %>%
  inner_join(stance_lookup, by = "comment_id", relationship = "many-to-one") %>%
  filter(!is.na(stance_label))

if ("topic_0_prob" %in% colnames(analysis_data)) {
  analysis_data <- analysis_data %>% filter(topic_0_prob >= 0.6)
}

analysis_data <- analysis_data %>%
  filter(platform == "YouTube" | is.na(platform)) %>%
  mutate(
    comment_replies = as.numeric(replies),
    stance_label = as.character(stance_label),
    stance_binary = ifelse(stance_label == "Anti-Diversity", 1, 0)
  ) %>%
  filter(!is.na(comment_replies), stance_label %in% c("Pro-Diversity", "Anti-Diversity")) %>%
  filter(!is.na(harmoniousness_raw), !is.na(video_id))

# Fit interaction model
mod <- glmer.nb(
  comment_replies ~ stance_binary + harmoniousness_raw + stance_binary:harmoniousness_raw + (1 | video_id),
  data = analysis_data,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# Build prediction grid
grid <- expand.grid(
  harmoniousness_raw = seq(0, 1, by = 0.02),
  stance_binary = c(0, 1)
) %>%
  as_tibble() %>%
  mutate(
    stance_label = ifelse(stance_binary == 1, "Anti-Diversity", "Pro-Diversity")
  )

grid$pred_replies <- predict(mod, newdata = grid, type = "response", re.form = NA)

# Plot
pro_color <- "#2D6A4F"   # green
anti_color <- "#C55A11"  # orange

fig <- ggplot(grid, aes(x = harmoniousness_raw, y = pred_replies, color = stance_label)) +
  geom_line(linewidth = 1.4) +
  scale_color_manual(values = c("Pro-Diversity" = pro_color, "Anti-Diversity" = anti_color)) +
  labs(
    x = "Constructive-Feature Index (0-1)",
    y = "Predicted Replies",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    panel.grid.minor = element_blank()
  )

# Save
output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(output_dir, "Figure_RQ2_Interaction_Constructive_x_Stance_Replies.png")

# 2400×1400px at dpi=300 -> 8×4.6667 inches
ggsave(out_path, fig, width = 8, height = 1400 / 300, dpi = 300)

cat("✓ Saved:", out_path, "\n")
cat("N =", nrow(analysis_data), "comments\n")

