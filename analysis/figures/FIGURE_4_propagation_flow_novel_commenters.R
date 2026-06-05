# Purpose
# Generate Figure 4 (manuscript Figure 5): propagation flow diagram using the
# PRIMARY different-person dyad analysis (N = 17,370). Parallel to
# FIGURE_4_propagation_flow.R, which uses the all-dyad sensitivity numbers.
#
# Source: analysis/propagation/
#         RQ3_propagation_novel_commenters_only_results.rds
# Output: outputs/Figure4_Propagation_Flow_NovelCommenters.png
rm(list = ls())
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(glmmTMB)
  library(lme4)
  library(showtext)
})
# Enable Greek-glyph rendering (the default macOS PDF/PNG device falls back to
# a font without beta, rendering 'beta' as '..' in ggsave outputs).
showtext_auto()
font_add_google("Open Sans", "OpenSans")

cat("=== FIGURE 4 (NOVEL COMMENTERS): PROPAGATION FLOW DIAGRAM ===\n")

rds_path <- "analysis/propagation/RQ3_propagation_novel_commenters_only_results.rds"
if (!file.exists(rds_path)) {
  stop("Run RQ3_propagation_novel_commenters_only.R first to create:\n  ", rds_path)
}
res <- readRDS(rds_path)

# Helper: pull beta + p from a stored model object
extract_bp <- function(entry) {
  mod   <- entry$model
  coefs <- fixef(mod)$cond
  ses   <- sqrt(diag(vcov(mod)$cond))
  z     <- coefs[2] / ses[2]
  p     <- 2 * (1 - pnorm(abs(z)))
  list(beta = unname(coefs[2]), p = unname(p))
}

CC <- extract_bp(res$C_to_C)
DD <- extract_bp(res$D_to_D)
CD <- extract_bp(res$C_to_D)
DC <- extract_bp(res$D_to_C)

beta_C_to_C <- CC$beta
beta_D_to_D <- DD$beta
beta_C_to_D <- CD$beta
beta_D_to_C <- DC$beta
p_C_to_D    <- CD$p
p_D_to_C    <- DC$p
n_dyads     <- res$dyads_novel_n

cat(sprintf("N novel-commenter dyads: %d\n", n_dyads))
cat(sprintf("Parent C -> Child C: beta = %.3f\n", beta_C_to_C))
cat(sprintf("Parent D -> Child D: beta = %.3f\n", beta_D_to_D))
cat(sprintf("Parent C -> Child D: beta = %.3f (p = %.3f)\n", beta_C_to_D, p_C_to_D))
cat(sprintf("Parent D -> Child C: beta = %.3f (p = %.3f)\n", beta_D_to_C, p_D_to_C))

# Flow diagram
constructiveness_color <- "#1f77b4"
destructiveness_color  <- "#ff7f0e"

# Use plotmath expressions so beta renders via R's built-in symbol font,
# independent of system-font Greek support.
label_cc <- bquote(beta == .(sprintf("%.3f", beta_C_to_C)))
label_dd <- bquote(beta == .(sprintf("%.3f", beta_D_to_D)))
label_cd <- if (p_C_to_D < .05) bquote(beta == .(sprintf("%.3f", beta_C_to_D))) else
  bquote(beta == .(sprintf("%.3f", beta_C_to_D)) ~ "(n.s.)")
label_dc <- if (p_D_to_C < .05) bquote(beta == .(sprintf("%.3f", beta_D_to_C))) else
  bquote(beta == .(sprintf("%.3f", beta_D_to_C)) ~ "(n.s.)")

fig <- ggplot() +
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 3, ymax = 4),
            fill = constructiveness_color, alpha = 0.3, color = "black", linewidth = 1) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = 3, ymax = 4),
            fill = destructiveness_color, alpha = 0.3, color = "black", linewidth = 1) +
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 1),
            fill = constructiveness_color, alpha = 0.3, color = "black", linewidth = 1) +
  geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = 0, ymax = 1),
            fill = destructiveness_color, alpha = 0.3, color = "black", linewidth = 1) +

  annotate("text", x = 1, y = 3.5, label = "Parent\nConstructive features",
           size = 11, fontface = "bold", lineheight = 0.95) +
  annotate("text", x = 3, y = 3.5, label = "Parent\nDestructive features",
           size = 11, fontface = "bold", lineheight = 0.95) +
  annotate("text", x = 1, y = 0.5, label = "Child\nConstructive features",
           size = 11, fontface = "bold", lineheight = 0.95) +
  annotate("text", x = 3, y = 0.5, label = "Child\nDestructive features",
           size = 11, fontface = "bold", lineheight = 0.95) +

  geom_segment(aes(x = 1, y = 3, xend = 1, yend = 1),
               arrow = arrow(length = unit(0.45, "cm"), type = "closed"),
               linewidth = 2, color = constructiveness_color) +
  annotate("text", x = 1.4, y = 2, label = list(label_cc), parse = TRUE,
           size = 12, fontface = "bold", color = constructiveness_color) +

  geom_segment(aes(x = 3, y = 3, xend = 3, yend = 1),
               arrow = arrow(length = unit(0.45, "cm"), type = "closed"),
               linewidth = 2, color = destructiveness_color) +
  annotate("text", x = 2.6, y = 2, label = list(label_dd), parse = TRUE,
           size = 12, fontface = "bold", color = destructiveness_color) +

  geom_curve(aes(x = 1, y = 3, xend = 3, yend = 1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             linewidth = 1.3, color = "gray50", curvature = 0.3, linetype = "dashed") +
  annotate("text", x = 2, y = 1.4, label = list(label_cd), parse = TRUE,
           size = 9, fontface = "bold", color = "gray30") +

  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             linewidth = 1.3, color = "gray50", curvature = 0.3, linetype = "dashed") +
  annotate("text", x = 2, y = 2.6, label = list(label_dc), parse = TRUE,
           size = 9, fontface = "bold", color = "gray30") +

  theme_void(base_family = "OpenSans") +
  theme(plot.margin = margin(t = 8, r = 12, b = 8, l = 12)) +
  coord_cartesian(xlim = c(0.25, 3.75), ylim = c(-0.1, 4.1), expand = FALSE)

# Save
output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
ggsave(file.path(output_dir, "Figure4_Propagation_Flow_NovelCommenters.png"),
       fig, width = 9, height = 6.2, dpi = 300)

cat("\n=== FIGURE SAVED ===\n")
cat("Output: ", file.path(output_dir, "Figure4_Propagation_Flow_NovelCommenters.png"), "\n")
