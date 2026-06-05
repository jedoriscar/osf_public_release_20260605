# Purpose
# Generate a presentation-style RQ3 propagation figure as horizontal bars
# (same-direction propagation only), exported as two PNG layers for a two-step reveal:
# 1) Orange only: Destructive → Destructive
# 2) Both bars:  Destructive → Destructive and Constructive → Constructive
#
# Style matches existing `09_figures/` exports (theme_minimal base_size=14).
#
# Output:
# - analysis/figures/outputs/Figure_RQ3_Propagation_Bars_Orange_Only.png
# - analysis/figures/outputs/Figure_RQ3_Propagation_Bars_Both.png
#
# Coefficients (from manuscript RQ3 results section):
# - Destructive parent → Destructive reply: β = 0.119
# - Constructive parent → Constructive reply: β = 0.155

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Prepare data
constructive_color <- "#2D6A4F"  # green
destructive_color  <- "#C55A11"  # orange

betas <- tibble::tribble(
  ~label,                      ~beta,  ~type,
  "Destructive → Destructive",  0.119,  "Destructive",
  "Constructive → Constructive",0.155,  "Constructive"
) %>%
  mutate(
    label = factor(label, levels = c("Destructive → Destructive", "Constructive → Constructive")),
    type = factor(type, levels = c("Constructive", "Destructive"))
  )

make_plot <- function(df) {
  xmax <- max(df$beta) * 1.25
  xmax <- max(xmax, 0.10)

  ggplot(df, aes(x = beta, y = label, fill = type)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = sprintf("β = %.3f", beta)),
      hjust = -0.08,
      size = 4.2,
      fontface = "bold",
      color = "black"
    ) +
    scale_fill_manual(values = c("Constructive" = constructive_color, "Destructive" = destructive_color)) +
    coord_cartesian(xlim = c(0, xmax), clip = "off") +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 13),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(10, 22, 10, 10)
    )
}

# Save
output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

orange_only <- betas %>% filter(type == "Destructive")
both <- betas

p_orange <- make_plot(orange_only)
p_both <- make_plot(both)

out_orange <- file.path(output_dir, "Figure_RQ3_Propagation_Bars_Orange_Only.png")
out_both <- file.path(output_dir, "Figure_RQ3_Propagation_Bars_Both.png")

# Match 2400×1400px at dpi=300 -> 8×4.6667 inches
ggsave(out_orange, p_orange, width = 8, height = 1400 / 300, dpi = 300)
ggsave(out_both, p_both, width = 8, height = 1400 / 300, dpi = 300)

# Verification
cat("✓ RQ3 propagation reveal exports saved:\n")
cat("  -", out_orange, "\n")
cat("  -", out_both, "\n")
print(betas)

