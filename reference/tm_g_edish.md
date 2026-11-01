# teal Module: eDISH / mDISH / cholestatic screening plot

teal Module: eDISH / mDISH / cholestatic screening plot

## Usage

``` r
tm_g_edish(
  label = "eDISH plot",
  adsl_name = "ADSL",
  adlb_name = "ADLB",
  arm_var = teal.picks::variables(choices = c("TRT01A", "TRT01P", "ARM", "SEX", "RACE",
    "COUNTRY"), selected = "TRT01A", multiple = FALSE),
  population_var = teal.picks::variables(choices = c("SAFFL", "ITTFL"), selected =
    "SAFFL", multiple = FALSE),
  population_value = "Y",
  window_days = 14,
  exclude_alp = TRUE,
  compare = c(">=", ">"),
  which = c("edish", "mdish", "cholestatic"),
  color_by = c("arm", "quadrant"),
  facet = FALSE,
  annotate_quadrants = TRUE,
  mark_potential = TRUE,
  log_axes = TRUE,
  group_vars = NULL,
  population_choices = NULL
)
```

## Arguments

- label:

  Module label.

- adsl_name:

  ADSL dataset name in `teal_data`.

- adlb_name:

  ADLB dataset name.

- arm_var:

  ([`teal.picks::variables()`](https://rdrr.io/pkg/teal.picks/man/picks.html))  
  Treatment or grouping column on ADSL. `multiple` must be `FALSE`. The
  default selection is `TRT01A`. The control shows column labels from
  the data.

- population_var:

  ([`teal.picks::variables()`](https://rdrr.io/pkg/teal.picks/man/picks.html))  
  Population flag column on ADSL. `multiple` must be `FALSE`. The
  default selection is `SAFFL`. A length-1 character string is
  deprecated and still accepted as the selected flag.

- population_value:

  Population flag value to keep. Not shown in the UI.

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

- group_vars:

  **\[deprecated\]** Named character vector of treatment columns, such
  as `c("Actual Treatment" = "TRT01A")`. Still accepted. Converted with
  [`lifecycle::deprecate_warn()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)
  to `teal.picks::variables(choices, selected, multiple = FALSE)`.
  Prefer `arm_var`.

- population_choices:

  **\[deprecated\]** Named character vector of population flag columns.
  Still accepted and converted the same way. Prefer `population_var`.

## Value

A `teal` module object.

## Examples

``` r
synth <- make_synthetic_hyslaw_data(n_extra = 0L, seed = 1L)
data <- teal.data::teal_data(ADSL = synth$ADSL, ADLB = synth$ADLB)
app <- teal::init(
  data = data,
  modules = teal::modules(
    tm_g_edish(
      arm_var = teal.picks::variables(
        choices = c("TRT01A", "SEX", "RACE", "COUNTRY"),
        selected = "TRT01A",
        multiple = FALSE
      ),
      population_var = teal.picks::variables(
        choices = c("SAFFL", "ITTFL"),
        selected = "SAFFL",
        multiple = FALSE
      )
    )
  )
)
#> Registered S3 method overwritten by 'teal':
#>   method        from      
#>   c.teal_slices teal.slice
if (interactive()) {
  shiny::shinyApp(app$ui, app$server)
}
```
