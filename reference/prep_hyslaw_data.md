# Prepare ADSL / ADLB records for Hy's Law review

Filters liver-related laboratory parameters, computes ULN multiples, and
joins the analysis population. An empty result is returned as a
structured list (`empty = TRUE`) so teal modules can show a no-data
message.

## Usage

``` r
prep_hyslaw_data(
  adsl,
  adlb,
  arm_var = "TRT01A",
  subject_var = "USUBJID",
  paramcd_var = "PARAMCD",
  aval_var = "AVAL",
  anrhi_var = "ANRHI",
  adt_var = "ADT",
  ady_var = "ADY",
  population_var = "SAFFL",
  population_value = "Y",
  ontrt_only = TRUE,
  liver_paramcds = c("ALT", "AST", "BILI", "ALP", "ALKPH")
)
```

## Arguments

- adsl:

  ADSL-like subject-level data.

- adlb:

  ADLB-like BDS laboratory data.

- arm_var:

  Treatment / grouping variable.

- subject_var:

  Subject identifier.

- paramcd_var:

  Parameter code variable.

- aval_var:

  Analysis value variable.

- anrhi_var:

  ULN variable.

- adt_var:

  Analysis date variable.

- ady_var:

  Analysis day variable.

- population_var:

  Optional population flag variable.

- population_value:

  Population flag value to keep.

- ontrt_only:

  If `TRUE` and `ONTRTFL` exists, keep on-treatment records plus
  baseline (`ABLFL == "Y"`) when that flag is present.

- liver_paramcds:

  Parameter codes to keep. `ALKPH` is treated as ALP.

## Value

A list with long `labs`, `adsl`, `alt_counts_df`, metadata, and `empty`.
