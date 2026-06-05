#!/usr/bin/env bash
set -u

# Run analyses from the public release folder.
# Source-stage scripts that require raw text or raw collection files are not
# included in this public release.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR" || exit 1

LOG_DIR="analysis/logs/run_all_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$LOG_DIR"
STATUS_FILE="$LOG_DIR/status.tsv"
printf "script\tstatus\tseconds\n" > "$STATUS_FILE"

mapfile -t SCRIPTS < <(find analysis \
  -path '*/logs/*' -prune -o \
  -type f -name '*.R' -not -name '._*' -print | sort)

for script in "${SCRIPTS[@]}"; do
  safe_name="$(printf "%s" "$script" | tr '/ ' '__')"
  log_file="$LOG_DIR/${safe_name}.log"
  start="$(date +%s)"
  printf "Running %s\n" "$script"

  if LC_ALL=C Rscript "$script" > "$log_file" 2>&1; then
    run_status="SUCCESS"
  else
    run_status="ERROR"
  fi

  end="$(date +%s)"
  printf "%s\t%s\t%s\n" "$script" "$run_status" "$((end - start))" | tee -a "$STATUS_FILE"
done

printf "\nStatus written to %s\n" "$STATUS_FILE"
