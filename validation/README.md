# Validation Materials

This folder contains redacted human-validation data and scripts used to produce reliability and calibration summaries.

Run from the public release folder root:

```bash
bash validation/run_validation.sh
```

The validation data use pseudonymous coder labels and remove raw comment text. The files retain the feature labels, model outputs, and pseudonymous IDs needed to reproduce the reliability and calibration tables.

Current stance validation uses 300 comments coded by two research assistants and compared with the model labels. Current agreement validation uses 300 parent-child dyads. Agreement reliability is reported for the full four-category task and, separately, for the clear agree/disagree subset emphasized in the manuscript. The agreement trace summaries in `validation/tables/` document that all 300 V5 dyads were found in the full dyad dataset, 61 already had existing model labels, and 239 were classified with the original agreement prompt to complete the validation set.
