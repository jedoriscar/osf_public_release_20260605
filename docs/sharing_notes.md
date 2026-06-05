# Sharing Notes

The public release is designed to share the data and code needed to inspect and reproduce the reported analyses while protecting raw platform content and user privacy.

Public GitHub repository: https://github.com/jedoriscar/osf_public_release_20260605

## Included

- Deidentified comment-level datasets.
- Deidentified parent-child dyad datasets.
- Derived feature scores and indices used in the analyses.
- Derived video upload year for temporal summaries, without exact platform timestamps.
- Redacted stance and agreement labels used in robustness checks.
- Deleted-comment status inputs used in sensitivity analyses.
- Human-validation data with raw text removed and coder names replaced by pseudonymous labels.
- R scripts for the main analyses, robustness checks, supplemental analyses, and figures.
- Generated public figures and supporting figure data files.

## Not Public

The following materials are not included because they contain raw text, direct platform identifiers, user handles, raw URLs, or source-only objects:

- Full comment text.
- Raw YouTube/TikTok comment IDs, video IDs, usernames, channel names, and URLs.
- Raw API collection outputs.
- Raw validation worksheets with comment text.
- Full topic-model object files and raw topic-model inputs.
- Raw video-framing worksheets.

Where possible, the public release includes deidentified or aggregate versions of these materials. For example, pseudonymous IDs preserve joins across files, and derived labels are retained when they are needed for reported analyses.

## Interpreting Source-Stage Scripts

Some source-stage scripts are not included because they require restricted materials. This should be read as a privacy/source-data limit of the public release, not as missing analysis code. The public analytic data contain the variables needed for the main reported statistical models.
