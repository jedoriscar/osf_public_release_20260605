# Analysis Scripts

Run scripts from the public release folder root. Most scripts load data through `analysis/setup/load_data.R`.

```bash
bash analysis/run_all.sh
```

`analysis/run_all.sh` runs available R scripts and writes logs to `analysis/logs/`. Source-stage scripts that require restricted materials, such as raw comment text, raw collection files, or full topic-model objects, are not included in this public release.

## Folder Map

- `setup/`: package installation, data loading, and basic structure checks.
- `discriminant_validity/`: correlations with sentiment, politeness, moral outrage, and constructiveness/destructiveness.
- `prevalence/`: constructiveness/destructiveness prevalence analyses.
- `engagement/`: surfacing, likes, replies, and engagement models.
- `propagation/`: parent-child reply propagation models.
- `agreement_robustness/`: agreement/disagreement robustness analyses.
- `stance_robustness/`: stance robustness analyses.
- `temporal_and_deleted_comments/`: temporal trends and deleted-comment checks.
- `climate_replication/`: replication analyses in climate-change discourse.
- `figures/`: figure-generation scripts.
- `supplement_*`: supplemental analyses grouped by supplement section.
