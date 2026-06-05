# Purpose
# Single main-text figure (Figure 6): three sensitivity checks in one layout.
# Panel A: agreement (constructive features vs. mere agreement).
# Panel B: ideological stance (constructive features vs. pro-diversity stance).
# Panel C: temporal persistence (constructive features stable in relative dominance).
# Layout: journal-style — Panels A and B side by side; Panel C full width below.
#
# Font sizes are kept at 14+ for readability.

# Setup
rm(list = ls())
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(lubridate)
library(patchwork)

source("analysis/setup/load_data.R")

constructiveness_color <- "#1f77b4"
destructiveness_color <- "#ff7f0e"

LAB_C <- "Constructive-Feature Index"
LAB_D <- "Destructive-Feature Index"

# Minimal chrome: no titles, no legends (colors defined in figure caption).
theme_fig <- function() {
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.text     = element_text(size = 13),
    plot.title    = element_blank(),
    plot.subtitle = element_blank(),
    plot.tag      = element_text(face = "bold", size = 16),
    axis.title    = element_text(size = 12),
    axis.text     = element_text(size = 11)
  )
}

# --- Panel A: Agreement ---
if ("agreement_label" %in% colnames(joined_data)) {
  agreement_data <- joined_data %>%
    filter(!is.na(agreement_label), !is.na(harmoniousness_raw), !is.na(divisiveness_raw))
} else {
  stop("agreement_label not in joined_data. Run prepare_canonical_data.R.")
}
agreement_summary <- agreement_data %>%
  group_by(agreement_label) %>%
  summarize(
    mean_C = mean(harmoniousness_raw, na.rm = TRUE),
    mean_D = mean(divisiveness_raw, na.rm = TRUE),
    se_C = sd(harmoniousness_raw, na.rm = TRUE) / sqrt(n()),
    se_D = sd(divisiveness_raw, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(agreement_label = factor(agreement_label, levels = c("Agree", "Disagree", "Mixed", "Neither")))

plot_agreement <- agreement_summary %>%
  pivot_longer(cols = c(mean_C, mean_D), names_to = "discourse_type", values_to = "mean_value") %>%
  mutate(
    discourse_type = recode(discourse_type, mean_C = LAB_C, mean_D = LAB_D),
    se = ifelse(discourse_type == LAB_C, se_C, se_D)
  ) %>%
  select(agreement_label, discourse_type, mean_value, se)

pA <- ggplot(plot_agreement, aes(x = agreement_label, y = mean_value, fill = discourse_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(
    aes(y = mean_value + se + 0.022, label = sprintf("%.1f%%", mean_value * 100)),
    position = position_dodge(width = 0.9),
    vjust = 0,
    size = 2.85
  ) +
  scale_fill_manual(name = NULL,
                    values = setNames(c(constructiveness_color, destructiveness_color), c(LAB_C, LAB_D)),
                    guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.24))) +
  labs(
    x = "Child reply vs. parent (GPT-4o-mini)",
    y = "Index (0–1)"
  ) +
  theme_fig() +
  theme(legend.position = "none")

# --- Panel B: Stance ---
stance_file <- "data/model_labels/racial_stance_labels.csv"
if (!file.exists(stance_file)) stop("Stance file not found.")
stance_ids <- read.csv(stance_file, stringsAsFactors = FALSE) %>%
  select(comment_id, stance_label) %>%
  mutate(
    sl = trimws(coalesce(as.character(stance_label), "")),
    stance_label = if_else(sl != "" & !toupper(sl) %in% c("NA", "N/A"), sl, NA_character_)
  ) %>%
  select(comment_id, stance_label) %>%
  filter(!is.na(stance_label))

stance_data <- stance_ids %>%
  left_join(joined_data %>% select(comment_id, harmoniousness_raw, divisiveness_raw), by = "comment_id") %>%
  filter(!is.na(harmoniousness_raw), !is.na(divisiveness_raw))

stance_summary <- stance_data %>%
  group_by(stance_label) %>%
  summarize(
    mean_C = mean(harmoniousness_raw, na.rm = TRUE),
    mean_D = mean(divisiveness_raw, na.rm = TRUE),
    se_C = sd(harmoniousness_raw, na.rm = TRUE) / sqrt(n()),
    se_D = sd(divisiveness_raw, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(
    stance_label %in% c("Pro-Diversity", "Neutral/Unclear", "Anti-Diversity")
  ) %>%
  mutate(stance_label = factor(stance_label, levels = c("Pro-Diversity", "Neutral/Unclear", "Anti-Diversity")))

plot_stance <- stance_summary %>%
  pivot_longer(cols = c(mean_C, mean_D), names_to = "discourse_type", values_to = "mean_value") %>%
  mutate(
    discourse_type = recode(discourse_type, mean_C = LAB_C, mean_D = LAB_D),
    se = ifelse(discourse_type == LAB_C, se_C, se_D)
  ) %>%
  select(stance_label, discourse_type, mean_value, se)

pB <- ggplot(plot_stance, aes(x = stance_label, y = mean_value, fill = discourse_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(
    aes(y = mean_value + se + 0.022, label = sprintf("%.1f%%", mean_value * 100)),
    position = position_dodge(width = 0.9),
    vjust = 0,
    size = 2.85
  ) +
  scale_fill_manual(name = NULL,
                    values = setNames(c(constructiveness_color, destructiveness_color), c(LAB_C, LAB_D)),
                    guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.24))) +
  labs(
    x = "Stance on diversity (GPT-4o-mini)",
    y = NULL
  ) +
  theme_fig() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

# --- Panel C: Temporal ---
# Displayed points are VIDEO-ADJUSTED annual means from a factor-year LMM
# (year as factor, random intercept for video); the line is the within-video
# linear slope. Years with n < 100 comments (2011, 2012, 2014, 2015) are
# excluded from the displayed points to avoid unstable single- or
# near-single-comment annual estimates. The within-video slope is essentially
# unchanged whether those years are included or excluded.
if ("comment_published_at" %in% colnames(joined_data)) {
  joined_data$year <- year(joined_data$comment_published_at)
} else if ("comment_date" %in% colnames(joined_data)) {
  joined_data$year <- year(as.Date(joined_data$comment_date))
} else {
  stop("Comment date column not found.")
}
temporal_data <- joined_data %>%
  filter(!is.na(year), !is.na(harmoniousness_raw), !is.na(divisiveness_raw))

# Filter to years with n >= 100 for displayed annual means
year_n  <- temporal_data %>% count(year, name = "n_year")
keep_yr <- year_n %>% filter(n_year >= 100) %>% pull(year)
ad      <- temporal_data %>% filter(year %in% keep_yr) %>%
  mutate(year_c = year - mean(year), year_f = factor(year))

# Linear LMM (within-video slope) for the line
mod_C_lin <- lmer(harmoniousness_raw ~ year_c + (1 | video_id), data = ad)
mod_D_lin <- lmer(divisiveness_raw   ~ year_c + (1 | video_id), data = ad)

# Factor LMM (year-specific video-adjusted means) for the points
mod_C_fac <- lmer(harmoniousness_raw ~ year_f + (1 | video_id), data = ad)
mod_D_fac <- lmer(divisiveness_raw   ~ year_f + (1 | video_id), data = ad)

get_adjusted <- function(mod) {
  s <- summary(mod)$coefficients
  yrs <- levels(ad$year_f)
  int <- s["(Intercept)", "Estimate"]; int_se <- s["(Intercept)", "Std. Error"]
  out <- data.frame(year = as.numeric(yrs), estimate = NA_real_, se = NA_real_)
  for (i in seq_along(yrs)) {
    if (i == 1) { out$estimate[i] <- int; out$se[i] <- int_se } else {
      nm <- paste0("year_f", yrs[i])
      out$estimate[i] <- int + s[nm, "Estimate"]
      out$se[i]       <- sqrt(int_se^2 + s[nm, "Std. Error"]^2)
    }
  }
  out
}

adj_C <- get_adjusted(mod_C_fac) %>% mutate(discourse = "C")
adj_D <- get_adjusted(mod_D_fac) %>% mutate(discourse = "D")
points_data <- bind_rows(adj_C, adj_D) %>%
  mutate(discourse_type = factor(discourse, levels = c("D", "C"),
                                 labels = c(LAB_D, LAB_C)))

# Linear-fit line predictions across the displayed year range
line_yrs <- sort(unique(ad$year))
yr_mean  <- mean(ad$year)
line_data <- bind_rows(
  data.frame(year = line_yrs, discourse = "C",
             pred = fixef(mod_C_lin)["(Intercept)"] +
                    fixef(mod_C_lin)["year_c"] * (line_yrs - yr_mean)),
  data.frame(year = line_yrs, discourse = "D",
             pred = fixef(mod_D_lin)["(Intercept)"] +
                    fixef(mod_D_lin)["year_c"] * (line_yrs - yr_mean))
) %>%
  mutate(discourse_type = factor(discourse, levels = c("D", "C"),
                                 labels = c(LAB_D, LAB_C)))

pC <- ggplot() +
  geom_line(data = line_data,
            aes(x = year, y = pred, color = discourse_type),
            linewidth = 1.1, alpha = 0.75) +
  geom_errorbar(data = points_data,
                aes(x = year, ymin = estimate - se, ymax = estimate + se,
                    color = discourse_type),
                width = 0.28, alpha = 0.55) +
  geom_point(data = points_data,
             aes(x = year, y = estimate, color = discourse_type), size = 2.8) +
  scale_color_manual(name = NULL,
                     values = setNames(c(constructiveness_color, destructiveness_color), c(LAB_C, LAB_D))) +
  scale_x_continuous(breaks = sort(unique(points_data$year))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Year comment posted",
    y = "Index (0–1)"
  ) +
  theme_fig() +
  theme(legend.position = "top")

# Combine and save
combined <- ((pA | pB) / pC) +
  plot_layout(heights = c(1, 1.05)) +
  plot_annotation(tag_levels = "A")

output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(output_dir, "Figure6_Sensitivity_Analyses_Three_Panels.png"),
       combined, width = 12, height = 9, dpi = 300)
ggsave(file.path(output_dir, "Figure_Combined_Robustness_Three_Panels.png"),
       combined, width = 12, height = 9, dpi = 300)

cat("✓ Figure 6 (three-panel sensitivity) saved to:\n")
cat("  ", file.path(output_dir, "Figure6_Sensitivity_Analyses_Three_Panels.png"), "\n")
cat("  ", file.path(output_dir, "Figure_Combined_Robustness_Three_Panels.png"), "\n")
