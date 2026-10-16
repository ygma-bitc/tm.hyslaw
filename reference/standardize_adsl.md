# Standardize an ADSL-like data frame for Hy's Law review

Harmonizes common CDISC Pilot / `pharmaverseadam` / `random.cdisc.data`
column variants so
[`prep_hyslaw_data()`](https://ygma-bitc.github.io/tm.hyslaw/reference/prep_hyslaw_data.md)
can join treatment and population flags.

## Usage

``` r
standardize_adsl(adsl)
```

## Arguments

- adsl:

  A data frame with at least `USUBJID`.

## Value

A standardized ADSL data frame.
