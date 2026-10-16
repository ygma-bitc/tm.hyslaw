# Classify eDISH peak quadrants and concurrent Hy's Law

Peak eDISH uses each subject's **on-treatment** maximum ALT or AST / ULN
versus maximum bilirubin / ULN (not necessarily the same day).
Concurrent (biochemical potential) Hy's Law requires bilirubin meeting
`bili_cut` on or after an ALT/AST meeting `alt_cut`, within
`window_days`. ALP meeting `alp_cut` in the same window can optionally
exclude the subject from potential Hy's Law.

## Usage

``` r
classify_hyslaw(
  adsl = NULL,
  adlb = NULL,
  prepped = NULL,
  window_days = 14,
  alt_cut = 3,
  bili_cut = 2,
  alp_cut = 2,
  exclude_alp = TRUE,
  compare = c(">=", ">"),
  ...
)
```

## Arguments

- adsl:

  ADSL-like data. Ignored when `prepped` is supplied.

- adlb:

  ADLB-like data. Ignored when `prepped` is supplied.

- prepped:

  Optional list from
  [`prep_hyslaw_data()`](https://ygma-bitc.github.io/tm.hyslaw/reference/prep_hyslaw_data.md).

- window_days:

  Concurrent window in days after the ALT/AST elevation. Default `14`
  matches the pharmaverse admiral Hy's Law vignette.

- alt_cut:

  ALT/AST ULN multiple.

- bili_cut:

  Bilirubin ULN multiple.

- alp_cut:

  ALP ULN multiple used for cholestasis exclusion and the cholestatic
  screening plot.

- exclude_alp:

  If `TRUE`, concurrent cases with ALP meeting `alp_cut` in the window
  are not counted as potential Hy's Law.

- compare:

  `">="` (default) or `">"`. FDA / many CSR shells use strict
  greater-than; this package defaults to greater-or-equal.

- ...:

  Passed to
  [`prep_hyslaw_data()`](https://ygma-bitc.github.io/tm.hyslaw/reference/prep_hyslaw_data.md)
  when `prepped` is `NULL`.

## Value

The `prepped` list with a subject-level `subjects` data frame.

## Details

Baseline records are kept for mDISH (peak / baseline) ratios. They are
not used as eDISH maxima.
