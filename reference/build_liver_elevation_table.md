# Nested on-treatment liver-test elevation table

Counts subjects whose peak on-treatment ULN ratio meets 3/5/10/20x
(transaminases) or 1.5/2x (bilirubin, ALP), then the combination of
concurrent biochemical Hy's Law split by ALP in the window.

## Usage

``` r
build_liver_elevation_table(
  adsl = NULL,
  adlb = NULL,
  arm_var = "TRT01A",
  show_total = TRUE,
  title = "Maximal on-treatment liver test elevations (exploratory)",
  footnotes = NULL,
  classified = NULL,
  at_cuts = c(3, 5, 10, 20),
  bili_cuts = c(1.5, 2),
  alp_cuts = c(1.5, 2),
  ...
)
```

## Arguments

- adsl:

  ADSL-like data. Ignored when `classified` is supplied.

- adlb:

  ADLB-like data.

- arm_var:

  Treatment / grouping variable.

- show_total:

  Whether to add a Total column.

- title:

  Table title.

- footnotes:

  Optional footnotes.

- classified:

  Optional list from
  [`classify_hyslaw()`](https://ygma-bitc.github.io/tm.hyslaw/reference/classify_hyslaw.md).

- at_cuts:

  Transaminase ULN multiples.

- bili_cuts:

  Bilirubin ULN multiples.

- alp_cuts:

  ALP ULN multiples.

- ...:

  Passed to
  [`classify_hyslaw()`](https://ygma-bitc.github.io/tm.hyslaw/reference/classify_hyslaw.md)
  when `classified` is `NULL`.

## Value

An `rtables` `VTableTree`.
