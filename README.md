# Public Data and Analysis Code

This folder contains deidentified data, analysis code, validation materials, and figure outputs for the study of constructive and destructive features in online discourse about demographic and climate change.

Public GitHub repository: https://github.com/jedoriscar/osf_public_release_20260605

The materials are organized for public sharing through OSF or GitHub. The folder is intended to let readers inspect the analytic data, rerun the main statistical analyses, and understand which source materials are not public because they contain raw text, platform identifiers, user handles, or other restricted content.

## Folder Guide

- `data/`: deidentified public datasets. The compressed CSV files are easiest to inspect. The `analysis_objects/` files are the R objects used by the analysis scripts.
- `analysis/`: R scripts for the main analyses, robustness checks, supplemental analyses, and figures.
- `validation/`: redacted human-validation data, validation scripts, and reliability/calibration tables.
- `outputs/`: generated figures and supporting figure data files.
- `docs/`: codebook, analysis map, reproducibility notes, sharing notes, and AI-use disclosure.
- `file_manifest.tsv`: file inventory with sizes and SHA-256 hashes.

## Running the Code

Run commands from this folder root:

```bash
Rscript analysis/setup/install_packages.R
bash analysis/run_all.sh
bash validation/run_validation.sh
```

The full analysis run writes logs to `analysis/logs/`. Source-stage scripts that require restricted materials, such as raw comment text, raw collection files, or full topic-model objects, are not included in this public folder. The public analytic datasets retain the variables needed for the reported quantitative models.

## Data Protection

The public data remove full comment text, raw platform IDs, video URLs, usernames, channel names, and raw validation worksheets. Stable pseudonymous IDs are retained so comments, videos, dyads, deleted-comment checks, stance labels, agreement labels, and validation records can still be joined.

The public data include derived measures used in the analyses, including Perspective API feature probabilities/labels, constructiveness and destructiveness indices, sentiment measures, politeness, stance/agreement labels, engagement variables, and parent-child dyad variables.

## Reuse

No reuse license is assigned in this folder. Add a license before public posting if you want to define reuse terms.
