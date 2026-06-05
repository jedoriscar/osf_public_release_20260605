# Reproducing Results

Start from the public release folder root.

```bash
Rscript analysis/setup/install_packages.R
Rscript analysis/setup/verify_data_structure.R
Rscript analysis/prevalence/RQ1_overall_prevalence_ttest.R
Rscript analysis/temporal_and_deleted_comments/ROB3_deleted_comments_analysis.R
bash validation/run_validation.sh
```

To run the available analysis scripts as a batch:

```bash
bash analysis/run_all.sh
```

The batch script records logs and status files under `analysis/logs/`. Source-stage scripts requiring restricted raw materials are not included in this public release. This is expected for a deidentified public package and does not mean the public analytic data are incomplete for the main reported quantitative models.

## Expected Smoke-Check Values

The following checks should reproduce from the public release:

- Total racial/demographic-change comments: `101,103`.
- YouTube comments: `78,196`; TikTok comments: `22,907`.
- Overall constructiveness mean: about `0.186`.
- Overall destructiveness mean: about `0.046`.
- Paired prevalence comparison: `t(101102) = 168.97`.
- Comments with at least one constructive feature: `52.8%`.
- Comments with at least one destructive feature: `20.0%`.

Deleted-comment sensitivity checks should reproduce the reported constructive-feature advantage among available and deleted comments.
