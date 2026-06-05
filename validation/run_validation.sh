#!/usr/bin/env bash
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR" || exit 1

for script in \
  validation/scripts/03_reliability_metrics.R \
  validation/scripts/04_master_reliability_table.R \
  validation/scripts/05_unified_feature_table.R \
  validation/scripts/06_ra_calibrated_thresholds.R \
  validation/scripts/07_calibration_three_anchors.R
do
  printf 'Running %s\n' "$script"
  LC_ALL=C Rscript "$script" || exit 1
done

printf 'Validation outputs written to validation/tables/\n'
