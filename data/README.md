# Data Files

The compressed CSV files are the easiest files to inspect directly. The R objects in `analysis_objects/` contain the same public analytic data in the format expected by the R scripts.

## Public CSV Files

- `racial_comments.csv.gz`: deidentified comment-level data for the demographic-change sample.
- `racial_parent_child_dyads.csv.gz`: deidentified parent-child reply dyads for the demographic-change sample.
- `climate_comments.csv.gz`: deidentified comment-level data for the climate-change replication sample.
- `climate_parent_child_dyads.csv.gz`: deidentified parent-child reply dyads for the climate-change replication sample.
- `dataset_sizes.csv`: row counts for the public datasets.

## Analysis Objects

- `analysis_objects/racial_comments.rda`: R object used by the racial-discourse scripts. Loading this file creates `joined_data`.
- `analysis_objects/racial_parent_child_dyads.rda`: R object used by propagation and reply analyses. Loading this file creates `parent_child_data`.
- `analysis_objects/climate_comments.rds`: RDS file used by the climate replication scripts.
- `analysis_objects/climate_parent_child_dyads.rds`: RDS file used by the climate propagation scripts.
- `analysis_objects/*_failed_comment_ids.csv`: deidentified IDs used in deleted-comment sensitivity checks.
- `analysis_objects/racial_accessible_comment_ids.csv`: deidentified IDs for available YouTube comments.

## Model Labels

Files in `model_labels/` contain redacted stance, agreement, and deleted-comment labels used by robustness analyses. They retain pseudonymous IDs and analytic labels but not raw text or raw platform identifiers.
