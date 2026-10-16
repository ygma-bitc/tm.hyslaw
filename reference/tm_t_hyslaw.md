# teal Module: Hy's Law summary table and notable listing

teal Module: Hy's Law summary table and notable listing

## Usage

``` r
tm_t_hyslaw(
  label = "Hy's Law table",
  adsl_name = "ADSL",
  adlb_name = "ADLB",
  group_vars = c(`Actual Treatment` = "TRT01A"),
  population_var = "SAFFL",
  population_value = "Y",
  population_choices = c(`Safety (SAFFL)` = "SAFFL", `ITT (ITTFL)` = "ITTFL"),
  window_days = 14,
  exclude_alp = TRUE,
  compare = c(">=", ">"),
  show_total = TRUE,
  table_which = c("both", "review", "elevation")
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

- show_total:

  Initial Total-column setting.

- table_which:

  `"review"`, `"elevation"`, or `"both"`.

## Value

A `teal` module object.
