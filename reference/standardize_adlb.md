# Standardize an ADLB / ADLBC-like data frame

Maps Pilot `A1HI` to `ANRHI`, `ALKPH` to `ALP`, and common value columns
(`AVAL`, `LBSTRESN`) so
[`prep_hyslaw_data()`](https://ygma-bitc.github.io/tm.hyslaw/reference/prep_hyslaw_data.md)
can compute ULN multiples.

## Usage

``` r
standardize_adlb(adlb, adsl = NULL)
```

## Arguments

- adlb:

  A laboratory BDS data frame.

- adsl:

  Optional standardized ADSL used to back-fill `TRT01A`.

## Value

A standardized ADLB data frame.
