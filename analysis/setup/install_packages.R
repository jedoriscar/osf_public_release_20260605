# Purpose
# Install required packages for all manuscript analyses.
# This script installs only the packages actually needed,
# following the principle of minimal dependencies.

# Required packages
required_packages <- c(
  "lme4",        # Multilevel models
  "lmerTest",    # p-values for lmer models
  "glmmTMB",     # Beta regression and other GLMMs
  "tidyverse",   # Data manipulation (dplyr, ggplot2, etc.)
  "broom",       # Tidy model outputs
  "psych",       # Descriptive statistics
  "corrplot",    # Correlation matrices
  "viridis",     # Color scales for figures
  "scales",      # Axis formatting
  "patchwork"    # Combining plots
)

# Install missing packages
cat("Checking for required packages...\n\n")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, dependencies = FALSE)
  } else {
    cat(pkg, "already installed.\n")
  }
}

cat("\n=== PACKAGE INSTALLATION COMPLETE ===\n")
cat("All required packages are now available.\n")
