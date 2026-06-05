# Codebook

This codebook summarizes the main variables used by the public analysis scripts. Some datasets contain additional derived variables used by individual robustness checks.

## Identifiers

- `comment_id`: pseudonymous comment identifier.
- `video_id`: pseudonymous video identifier.
- `parent_comment_id`: pseudonymous parent-comment identifier for replies.
- `row_id` or `unique_comment_identifier`: row-level identifier used when a platform did not provide a stable public comment ID.
- `username` or related user fields: pseudonymous user labels.

## Platform and Timing

- `platform`: YouTube or TikTok.
- `year`: year used for temporal analyses. Exact timestamps are not included in the public release when they are not needed for the reported models.

## Engagement and Surfacing

- `top_comment`: indicator for whether a YouTube top-level comment appeared in the relevance-sorted top-comment set.
- `like_count` / `likes`: comment-level likes where available.
- `reply_count` / `replies`: comment-level replies where available.

## Constructive Features

Constructive-feature probabilities are Perspective API-derived variables:

- `prob_compassion`
- `prob_curiosity`
- `prob_nuance`
- `prob_personal_story`
- `prob_reasoning`

The constructiveness index, usually `harmoniousness_raw` in the scripts, is the mean of the five constructive feature indicators after thresholding each probability at `0.6`.

## Destructive Features

Destructive-feature probabilities are Perspective API-derived variables:

- `prob_toxic` or `prob_toxicity`
- `prob_identity_attack`
- `prob_threat`
- `prob_attack_on_author`
- `prob_attack_on_commenter`

The destructiveness index, usually `divisiveness_raw` in the scripts, is the mean of the five destructive feature indicators after thresholding each probability at `0.6`.

## Related Measures

- VADER sentiment variables: lexicon-based sentiment scores.
- Politeness variables: politeness-marker measures used for discriminant-validity checks.
- `prob_moral_outrage` or related moral-outrage fields: moral-outrage classifier scores where available.
- `comment_length`: comment length after redaction-preserving processing.

## Stance and Agreement

Files in `data/model_labels/` contain redacted model labels for stance and agreement/disagreement analyses. These files preserve labels, confidence values where available, and pseudonymous IDs needed for joining with the public analytic data.

## Parent-Child Dyads

The dyad files contain child comments linked to parent comments. They are used for reply propagation, thread depth, unique-replier, stance, and agreement analyses.
