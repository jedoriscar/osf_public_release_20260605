# Purpose
# Generate Figure 6 (manuscript): temporal trends in constructive- and
# destructive-feature indices over comment year, on the YouTube subset.
#
# Important specification choice:
#   The displayed points are VIDEO-ADJUSTED annual means computed from a
#   factor-year LMM (year as factor, random intercept for video). These are
#   the right quantity to plot alongside a within-video linear trend, because
#   raw annual means confound the year effect with which videos contribute
#   comments in which years. Both quantities now live on the same scale, so
#   the model line and the displayed points are commensurable.
#
# Years with fewer than 100 comments (2011, 2012, 2014, 2015) are excluded
# from the displayed points to avoid unstable single-comment or near-empty
# annual estimates. The linear model is fit on the same n>=100 subset.
rm(list = ls())
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(lme4)
  library(lmerTest)
  library(lubridate)
})

source("analysis/setup/load_data.R")

if (!"comment_published_at" %in% colnames(joined_data)) {
  stop("comment_published_at not in joined_data.")
}
joined_data$year <- year(joined_data$comment_published_at)

analysis_data <- joined_data %>%
  filter(!is.na(year), !is.na(harmoniousness_raw), !is.na(divisiveness_raw))

# Year-by-year sample sizes
year_n <- analysis_data %>% count(year, name = "n_year") %>% arrange(year)
cat("=== Comments per year ===\n")
print(year_n)

# Filter to years with at least 100 comments for stable annual estimates
min_n <- 100
keep_years <- year_n %>% filter(n_year >= min_n) %>% pull(year)
ad <- analysis_data %>% filter(year %in% keep_years)
cat(sprintf("\nDisplayed years (n >= %d): %s\n",
            min_n, paste(sort(keep_years), collapse = ", ")))
cat(sprintf("N comments included: %d\n", nrow(ad)))

# Linear LMM — within-video slope (this is the slope reported in main text)
ad <- ad %>% mutate(year_c = year - mean(year))

mod_C_lin <- lmer(harmoniousness_raw ~ year_c + (1 | video_id), data = ad)
mod_D_lin <- lmer(divisiveness_raw   ~ year_c + (1 | video_id), data = ad)

cat("\n=== Linear LMM slopes (within-video; n >= 100 years) ===\n")
cat(sprintf("Constructive: b = %.4f per year, SE = %.4f, p < .001\n",
            fixef(mod_C_lin)["year_c"],
            summary(mod_C_lin)$coefficients["year_c","Std. Error"]))
cat(sprintf("Destructive:  b = %.4f per year, SE = %.4f, p < .001\n",
            fixef(mod_D_lin)["year_c"],
            summary(mod_D_lin)$coefficients["year_c","Std. Error"]))

# Factor LMM — video-adjusted annual means
ad$year_f <- factor(ad$year)

mod_C_fac <- lmer(harmoniousness_raw ~ year_f + (1 | video_id), data = ad)
mod_D_fac <- lmer(divisiveness_raw   ~ year_f + (1 | video_id), data = ad)

# Predicted year-adjusted means: intercept + year-factor coefficient
get_adjusted <- function(mod) {
  s   <- summary(mod)$coefficients
  yrs <- levels(ad$year_f)
  intercept <- s["(Intercept)", "Estimate"]
  int_se    <- s["(Intercept)", "Std. Error"]
  out <- data.frame(
    year      = as.numeric(yrs),
    estimate  = NA_real_,
    se        = NA_real_
  )
  for (i in seq_along(yrs)) {
    if (i == 1) {
      out$estimate[i] <- intercept
      out$se[i]       <- int_se
    } else {
      coef_name        <- paste0("year_f", yrs[i])
      out$estimate[i]  <- intercept + s[coef_name, "Estimate"]
      out$se[i]        <- sqrt(int_se^2 + s[coef_name, "Std. Error"]^2)
    }
  }
  out
}

adj_C <- get_adjusted(mod_C_fac) %>% mutate(discourse = "C")
adj_D <- get_adjusted(mod_D_fac) %>% mutate(discourse = "D")

cat("\n=== Video-adjusted annual means (Constructive) ===\n")
print(adj_C %>% mutate(across(c(estimate, se), ~ round(., 4))))
cat("\n=== Video-adjusted annual means (Destructive) ===\n")
print(adj_D %>% mutate(across(c(estimate, se), ~ round(., 4))))

# Predicted line from linear LMM (across the displayed year range)
line_C <- data.frame(year = sort(unique(ad$year)))
line_C$year_c <- line_C$year - mean(ad$year)
line_C$pred   <- fixef(mod_C_lin)["(Intercept)"] +
                 fixef(mod_C_lin)["year_c"] * line_C$year_c
line_C$discourse <- "C"

line_D <- data.frame(year = sort(unique(ad$year)))
line_D$year_c <- line_D$year - mean(ad$year)
line_D$pred   <- fixef(mod_D_lin)["(Intercept)"] +
                 fixef(mod_D_lin)["year_c"] * line_D$year_c
line_D$discourse <- "D"

# Combine for plotting
points <- bind_rows(adj_C, adj_D) %>%
  mutate(discourse_type = factor(discourse, levels = c("D", "C"),
                                 labels = c("Destructive-Feature Index",
                                            "Constructive-Feature Index")))
lines  <- bind_rows(line_C, line_D) %>%
  mutate(discourse_type = factor(discourse, levels = c("D", "C"),
                                 labels = c("Destructive-Feature Index",
                                            "Constructive-Feature Index")))

# Build figure
constructiveness_color <- "#1f77b4"
destructiveness_color  <- "#ff7f0e"

fig <- ggplot() +
  geom_line(data = lines,
            aes(x = year, y = pred, color = discourse_type),
            linewidth = 1.2, alpha = 0.7) +
  geom_errorbar(data = points,
                aes(x = year, ymin = estimate - se, ymax = estimate + se,
                    color = discourse_type),
                width = 0.3, alpha = 0.6) +
  geom_point(data = points,
             aes(x = year, y = estimate, color = discourse_type),
             size = 3) +
  scale_color_manual(values = c(
    "Constructive-Feature Index" = constructiveness_color,
    "Destructive-Feature Index"  = destructiveness_color
  )) +
  scale_x_continuous(breaks = sort(unique(points$year))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = NULL,
    subtitle = NULL,
    x = "Comment Year",
    y = "Proportion of Features Present (0–1)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.title      = element_text(size = 14),
    axis.text       = element_text(size = 13),
    legend.text     = element_text(size = 13),
    plot.margin     = margin(t = 10, r = 14, b = 10, l = 14)
  )

output_dir <- "analysis/figures/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
ggsave(file.path(output_dir, "Figure7_Temporal_Trends.png"),
       fig, width = 10, height = 5.5, dpi = 300)

cat("\n=== FIGURE SAVED ===\n")
