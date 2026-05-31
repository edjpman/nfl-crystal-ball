# Categorical Encoder Leakage Fix

This document explains the label-encoding data leakage that existed in the Chiefs win prediction pipeline, what changed to fix it, and how to use the persisted encoder at inference time.

## What Was Wrong

In `chief_wins.ipynb`, categorical columns (`season_type`, `city`, `opp_team`, `roof`, `surface`) were encoded **before** cross-validation:

```python
df = fteng.feature_addition(data=df)
df = fteng.cat_transform(data=df)   # fitted on the entire dataset
```

`featEng.cat_transform()` used a separate `LabelEncoder` per column and called `fit_transform()` on **all rows**. That means category mappings were learned from test-season data as well as training-season data.

### Why That Is Leakage

Each cross-validation fold holds out certain seasons for testing. When the encoder sees those test seasons during `fit`, it learns:

- Which category values exist in the test set
- Integer codes assigned to those values

Those codes are derived from information the model is not supposed to see at training time. The effect is usually small for tree models like XGBoost, but it makes CV metrics slightly optimistic and breaks the rule that preprocessing must be fit on training data only.

## What Changed

### 1. `featEng` in `nfl_cb_main/data_loader.py`

| Before | After |
|--------|-------|
| `LabelEncoder` per column, discarded after each call | Single `OrdinalEncoder` stored as `self.encoder` |
| No persistence | `save_encoder(path)` / `load_encoder(path)` via joblib |
| No transform-only path | `transform_new(data)` applies a fitted encoder without refitting |

`OrdinalEncoder` on the same columns produces the same style of integer mapping as the old per-column `LabelEncoder` loop. Columns and encoding logic are unchanged.

### 2. `chief_wins.ipynb` preprocessing flow

**Before CV (cell 2):** only `feature_addition()` runs. Categorical columns stay as raw strings until inside the fold loop.

**Inside each CV fold (cell 3):**

1. Split rows by `train_years` / `test_years`
2. `cat_transform(train_df)` — fit encoder on training seasons only
3. `transform_new(test_df)` — encode test seasons with the training-fitted encoder
4. Concatenate into `fold_df` and pass that to `ttd_year_splits()`

**After CV completes:**

1. Fit a final encoder on the full `df`
2. Save it to `models/encoder.joblib`

The final saved encoder is for **production / inference**, not for CV evaluation. CV folds each use their own fold-specific encoder fit on that fold's training seasons.

## New API Reference

```python
fteng = featEng(cat_cols=['season_type', 'city', 'opp_team', 'roof', 'surface'])

# Fit and encode (stores encoder on the instance)
encoded_train = fteng.cat_transform(train_df)

# Encode new data with the same mapping (no refit)
encoded_test = fteng.transform_new(test_df)

# Persist / restore
fteng.save_encoder('models/encoder.joblib')

fteng_inf = featEng(cat_cols=categorical_vars)
fteng_inf.load_encoder('models/encoder.joblib')
encoded_new = fteng_inf.transform_new(new_df)
```

## Inference Workflow

When scoring new games:

1. Run `feature_addition()` on raw play-by-play rows (same as training).
2. Load the saved encoder:

   ```python
   fteng = featEng(cat_cols=categorical_vars)
   fteng.load_encoder('models/encoder.joblib')
   df = fteng.transform_new(df)
   ```

3. Pass encoded features to the trained model.

If a category appears at inference that was never seen during final encoder fitting, `OrdinalEncoder.transform()` will raise an error (same practical constraint as the old `LabelEncoder` approach).

## Files Touched

- `nfl_cb_main/data_loader.py` — encoder implementation and persistence
- `notebooks/chief_wins.ipynb` — CV-safe encoding + final encoder save
- `requirements.txt` — added `joblib` for serialization

## Summary

| Stage | Encoder fit on | Purpose |
|-------|----------------|---------|
| CV fold | Training seasons in that fold | Unbiased validation metrics |
| After CV | Full dataset | Reusable artifact for inference (`models/encoder.joblib`) |

The fix ensures category integer codes in each CV fold depend only on training-season categories, eliminating the preprocessing leakage from the original notebook.
