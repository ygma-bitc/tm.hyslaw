# Build synthetic ADSL / ADLB fixtures for Hy's Law demos and tests

Seeds subjects into the four eDISH peak quadrants and two extra patterns
that distinguish peak-only Hy's Law from concurrent-window monitoring: a
peak pair outside the bilirubin window, and a concurrent pair with
elevated ALP (cholestasis exclusion).

## Usage

``` r
make_synthetic_hyslaw_data(n_extra = 6L, seed = 20260829L)
```

## Arguments

- n_extra:

  `integer(1)` Extra normal-range subjects per arm.

- seed:

  `integer(1)` Random seed for extra subjects only.

## Value

A list with `ADSL` and `ADLB`.

## Examples

``` r
data <- make_synthetic_hyslaw_data(seed = 1L)
names(data)
#> [1] "ADSL" "ADLB"
unique(data$ADLB$PARAMCD)
#> [1] "ALT"  "AST"  "BILI" "ALP" 
```
