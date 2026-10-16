# teal Module: eDISH / mDISH / cholestatic screening plot

teal Module: eDISH / mDISH / cholestatic screening plot

## Usage

``` r
tm_g_edish(
  label = "eDISH plot",
  adsl_name = "ADSL",
  adlb_name = "ADLB",
  group_vars = c(`Actual Treatment` = "TRT01A"),
  population_var = "SAFFL",
  population_value = "Y",
  population_choices = c(`Safety (SAFFL)` = "SAFFL", `ITT (ITTFL)` = "ITTFL"),
  window_days = 14,
  exclude_alp = TRUE,
  compare = c(">=", ">"),
  which = c("edish", "mdish", "cholestatic"),
  color_by = c("arm", "quadrant"),
  facet = FALSE,
  annotate_quadrants = TRUE,
  mark_potential = TRUE,
  log_axes = TRUE
)
```

## Arguments

- label:

  Module label.

- adsl_name:

  ADSL dataset name in `teal_data`.

- adlb_name:

  ADLB dataset name.

- group_vars:

  Named character vector of grouping variables.

- population_var:

  Default population flag.

- population_value:

  Population flag value to keep.

- population_choices:

  Named choices shown in the population selector.

- window_days:

  Default concurrent window.

- exclude_alp:

  Default ALP exclusion.

- compare:

  Default threshold operator, `">="` or `">"`.

- which:

  `"edish"`, `"mdish"`, or `"cholestatic"`.

- color_by:

  `"arm"` or `"quadrant"`.

- facet:

  Default faceting.

- annotate_quadrants:

  Default corner labels.

- mark_potential:

  Default red-circle overlay.

- log_axes:

  Default log-axis setting.

## Value

A `teal` module object.
