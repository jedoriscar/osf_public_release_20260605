# Purpose
# Generate Figure 4: Flow diagram showing parent-child propagation effects
# with regression coefficients overlaid on arrows.
#
# Reference: Main text Figure X (around line 107), lines 107-113
# Output: Flow diagram with parent C/D boxes, child C/D boxes, arrows with coefficients

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Load coefficients from RQ3 scripts (keeps the figure aligned with the model outputs)
cat("=== FIGURE 4: PROPAGATION FLOW DIAGRAM ===\n")
cat("Loading coefficients from RQ3 propagation results...\n\n")

rq3_dir <- "analysis/propagation"
rds_C_to_C   <- file.path(rq3_dir, "RQ3_parent_C_to_child_C_results.rds")
rds_C_to_D   <- file.path(rq3_dir, "RQ3_parent_C_to_child_D_results.rds")
rds_D_to_D   <- file.path(rq3_dir, "RQ3_parent_D_to_child_D_results.rds")
rds_D_to_C   <- file.path(rq3_dir, "RQ3_parent_D_to_child_C_chilling_results.rds")

if (!all(file.exists(c(rds_C_to_C, rds_C_to_D, rds_D_to_D, rds_D_to_C)))) {
  stop("Run the four RQ3 propagation scripts first to create the RDS files:\n",
       "  RQ3_parent_C_to_child_C.R, RQ3_parent_C_to_child_D.R,\n",
       "  RQ3_parent_D_to_child_D.R, RQ3_parent_D_to_child_C_chilling.R")
}

res_C_to_C <- readRDS(rds_C_to_C)
res_C_to_D <- readRDS(rds_C_to_D)
res_D_to_D <- readRDS(rds_D_to_D)
res_D_to_C <- readRDS(rds_D_to_C)

beta_C_to_C <- res_C_to_C$coefficient
beta_C_to_D <- res_C_to_D$coefficient
beta_D_to_D <- res_D_to_D$coefficient
beta_D_to_C <- res_D_to_C$coefficient

cat("Coefficients (from RQ3 scripts):\n")
cat(sprintf("  Parent C → Child C: β = %.3f\n", beta_C_to_C))
cat(sprintf("  Parent D → Child D: β = %.3f\n", beta_D_to_D))
cat(sprintf("  Parent C → Child D: β = %.3f (p = %.3f)\n", beta_C_to_D, res_C_to_D$p_value))
cat(sprintf("  Parent D → Child C: β = %.3f (p = %.3f)\n", beta_D_to_C, res_D_to_C$p_value))

# Create flow diagram
constructiveness_color <- "#1f77b4"
destructiveness_color <- "#ff7f0e"

fig4 <- ggplot() +
  # Parent boxes
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 3, ymax = 4),
            fill = constructiveness_color, alpha = 0.3, color = "black", linewidth = 1) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = 3, ymax = 4),
            fill = destructiveness_color, alpha = 0.3, color = "black", linewidth = 1) +
  
  # Child boxes
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 1),
            fill = constructiveness_color, alpha = 0.3, color = "black", linewidth = 1) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = 0, ymax = 1),
            fill = destructiveness_color, alpha = 0.3, color = "black", linewidth = 1) +
  
  # Labels
  annotate("text", x = 1, y = 3.5, label = "Parent:\nConstructive\nfeatures",
           size = 4.8, fontface = "bold") +
  annotate("text", x = 3, y = 3.5, label = "Parent:\nDestructive\nfeatures",
           size = 4.8, fontface = "bold") +
  annotate("text", x = 1, y = 0.5, label = "Child:\nConstructive\nfeatures",
           size = 4.8, fontface = "bold") +
  annotate("text", x = 3, y = 0.5, label = "Child:\nDestructive\nfeatures",
           size = 4.8, fontface = "bold") +
  
  # Direct arrows (same-direction) with coefficients
  geom_segment(aes(x = 1, y = 3, xend = 1, yend = 1),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               linewidth = 1.5, color = constructiveness_color) +
  annotate("text", x = 1.3, y = 2, label = sprintf("β = %.3f", beta_C_to_C),
           size = 4.2, fontface = "bold", color = constructiveness_color) +
  
  geom_segment(aes(x = 3, y = 3, xend = 3, yend = 1),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               linewidth = 1.5, color = destructiveness_color) +
  annotate("text", x = 2.7, y = 2, label = sprintf("β = %.3f", beta_D_to_D),
           size = 4.2, fontface = "bold", color = destructiveness_color) +
  
  # Cross arrows with coefficients
  geom_curve(aes(x = 1, y = 3, xend = 3, yend = 1),
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             linewidth = 1, color = "gray50", curvature = 0.3) +
  annotate("text", x = 2, y = 1.8, label = sprintf("β = %.3f (n.s.)", beta_C_to_D),
           size = 4.2, fontface = "bold", color = "gray30") +
  
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             linewidth = 1, color = "gray50", curvature = 0.3) +
  annotate("text", x = 2, y = 2.2, label = sprintf("β = %.3f (n.s.)", beta_D_to_C),
           size = 4.2, fontface = "bold", color = "gray30") +
  
  labs(
    title = "Discourse Features Propagate Through Reply Chains",
    subtitle = "Beta regression: parent and child constructive- vs. destructive-feature indices (0–1)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5)
  ) +
  xlim(-0.5, 4) +
  ylim(-0.5, 4.5)

# Save figure
output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(output_dir, "Figure4_Propagation_Flow.png"),
       fig4, width = 8, height = 8, dpi = 300)

cat("\n✓ Figure 4 saved!\n")
