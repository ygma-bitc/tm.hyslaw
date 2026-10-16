# Build Hy's Law / eDISH review tables

Default output is two stacked sections: maximal treatment-emergent (peak
eDISH) quadrant counts, then biochemical concurrent Hy's Law in the
selected window. Use
[`build_liver_elevation_table()`](https://ygma-bitc.github.io/tm.hyslaw/reference/build_liver_elevation_table.md)
for the nested ALT/AST/BILI/ALP elevation shell.

## Usage

``` r
build_hyslaw_table(
  adsl = NULL,
  adlb = NULL,
  arm_var = "TRT01A",
  show_total = TRUE,
  title = "Hy's Law monitoring (exploratory)",
  footnotes = NULL,
  classified = NULL,
  which = c("quadrant", "concurrent"),
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

- which:

  Character vector of sections: `"quadrant"`, `"concurrent"`.

- ...:

  Passed to
  [`classify_hyslaw()`](https://ygma-bitc.github.io/tm.hyslaw/reference/classify_hyslaw.md)
  when `classified` is `NULL`.

## Value

An `rtables` `VTableTree`.

## Details

Pass either raw `adsl` / `adlb` or a list returned by
[`classify_hyslaw()`](https://ygma-bitc.github.io/tm.hyslaw/reference/classify_hyslaw.md)
via `classified` so the teal server can prepare data once.

## Examples

``` r
data <- make_synthetic_hyslaw_data(n_extra = 0L, seed = 1L)
build_hyslaw_table(data$ADSL, data$ADLB)
#> Hy's Law monitoring (exploratory)
#> Peak eDISH cuts: ALT/AST >= 3x ULN, BILI >= 2x ULN (on-treatment maxima)
#> Concurrent window: 14 days after ALT/AST elevation; ALP exclusion on; operator >=
#> 
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#>                                                                                Placebo    Investigational    Total  
#>                                                                                 (N=4)          (N=6)         (N=10) 
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Maximal treatment-emergent liver tests (peak eDISH)                                                                 
#>   ALT or AST >= 3x ULN and BILI >= 2x ULN (peak Hy's Law quadrant)             1 (25.0)      3 (50.0)       4 (40.0)
#>   Temple's corollary                                                           1 (25.0)      1 (16.7)       2 (20.0)
#>   Cholestasis (isolated hyperbilirubinemia)                                    1 (25.0)      1 (16.7)       2 (20.0)
#>   Normal or near normal                                                        1 (25.0)      1 (16.7)       2 (20.0)
#> Biochemical concurrent Hy's Law (14-day window)                                                                     
#>   ALT or AST >= 3x ULN and BILI >= 2x ULN in window                            1 (25.0)      2 (33.3)       3 (30.0)
#>   Concurrent with ALP >= 2x ULN in window                                      0 (0.0)       1 (16.7)       1 (10.0)
#>   Potential Hy's Law after ALP exclusion                                       1 (25.0)      1 (16.7)       2 (20.0)
#>   Cholestatic concurrent (BILI >= 2x ULN within 14 days after ALP >= 2x ULN)   0 (0.0)       1 (16.7)       1 (10.0)
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> 
#> Exploratory review table; not a submission TFL.
#> Threshold operator is >= (FDA 2009 and many CSR shells use >; change classify_hyslaw(compare = ">") to match).
#> Peak eDISH / maximal abnormalities use each subject's on-treatment maximum ALT or AST / ULN versus maximum bilirubin / ULN. The two maxima are not required to occur on the same day. Baseline records (ABLFL = Y) are excluded from those maxima.
#> Biochemical concurrent Hy's Law: bilirubin >= 2x ULN on or after ALT/AST >= 3x ULN, within 14 days. This is not the peak-quadrant count and is not a confirmed Hy's Law case.
#> Potential Hy's Law applies optional ALP >= 2x ULN exclusion in the same window (currently on). Missing ALP is not imputed; subjects without ALP meeting the cut in the window are retained when exclusion is on.
#> R-ratio = (ALT/ULN)/(ALP/ULN) at the concurrent ALT date when both exist, otherwise peak ALT/ULN over peak ALP/ULN. Pattern: >=5 hepatocellular, >2 and <5 mixed, <=2 cholestatic.
#> Symptoms, alternative causes, and 'satisfies Hy's Law' are not assessed from ADLB.
```
