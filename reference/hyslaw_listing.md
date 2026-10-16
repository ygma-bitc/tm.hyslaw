# Subject listing for notable Hy's Law / eDISH cases

Subject listing for notable Hy's Law / eDISH cases

## Usage

``` r
hyslaw_listing(
  adsl = NULL,
  adlb = NULL,
  classified = NULL,
  which = c("notable", "all"),
  ...
)
```

## Arguments

- adsl, adlb:

  Passed to
  [`classify_hyslaw()`](https://ygma-bitc.github.io/tm.hyslaw/reference/classify_hyslaw.md)
  when `classified` is `NULL`.

- classified:

  Optional list from
  [`classify_hyslaw()`](https://ygma-bitc.github.io/tm.hyslaw/reference/classify_hyslaw.md).

- which:

  `"notable"` keeps peak Hy's Law, concurrent, ALP-excluded, or
  concurrent cholestatic subjects; `"all"` returns every subject.

- ...:

  Passed to
  [`classify_hyslaw()`](https://ygma-bitc.github.io/tm.hyslaw/reference/classify_hyslaw.md).

## Value

A data frame. Symptoms, alternative causes, and confirmed Hy's Law are
not derived from ADLB; see footnotes on
[`build_hyslaw_table()`](https://ygma-bitc.github.io/tm.hyslaw/reference/build_hyslaw_table.md).
