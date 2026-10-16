# Changelog

## tm.hyslaw 0.2.0

- Added R CMD check and pkgdown GitHub Actions workflows for
  release-ready open-source development.
- Added `_pkgdown.yml` reference grouping and a lightweight `.lintr`
  configuration.
- README now includes CI/pkgdown badges and local quality-gate commands.
- Fixed the R CMD check NOTE from unqualified
  [`runif()`](https://rdrr.io/r/stats/Uniform.html) by using
  [`stats::runif()`](https://rdrr.io/r/stats/Uniform.html).
- eDISH plot defaults to one panel colored by treatment, with quadrant
  names on the plot and red open circles for biochemical potential Hy’s
  Law. Caption states that axis maxima need not be the same day.
- Optional mDISH (`which = "mdish"`) and cholestatic ALP vs bilirubin
  screen (`which = "cholestatic"`). Faceting by group is opt-in.
- Peak maxima use on-treatment records; baseline is kept for mDISH
  ratios.
- Review table splits ST&F-style four peak quadrants from
  concurrent-window counts. New
  [`build_liver_elevation_table()`](https://ygma-bitc.github.io/tm.hyslaw/reference/build_liver_elevation_table.md)
  nested 3/5/10/20x and combination-plus-ALP rows.
- `compare = ">="` (default) or `">"`; footnotes document the operator,
  missing ALP, on-treatment vs baseline, and that ADLB does not support
  symptoms / alternative causes / confirmed Hy’s Law.
- Listing adds R-ratio and injury pattern.
- Teal modules expose plot type, threshold operator, and stacked review
  / elevation tables.

## tm.hyslaw 0.1.0

- Initial Hy’s Law / eDISH review builders that run without Shiny.
- Peak eDISH quadrants plus concurrent ALT/AST and bilirubin elevations
  (default 14-day window, aligned with the admiral Hy’s Law vignette).
- Optional ALP \>= 2x ULN exclusion in the same window.
- [`build_hyslaw_table()`](https://ygma-bitc.github.io/tm.hyslaw/reference/build_hyslaw_table.md),
  [`hyslaw_listing()`](https://ygma-bitc.github.io/tm.hyslaw/reference/hyslaw_listing.md),
  and
  [`plot_edish()`](https://ygma-bitc.github.io/tm.hyslaw/reference/plot_edish.md).
- Teal modules
  [`tm_g_edish()`](https://ygma-bitc.github.io/tm.hyslaw/reference/tm_g_edish.md)
  and
  [`tm_t_hyslaw()`](https://ygma-bitc.github.io/tm.hyslaw/reference/tm_t_hyslaw.md).
- Synthetic ADLB fixtures seed four eDISH quadrants, a peak-only pair
  outside the window, and a concurrent pair with elevated ALP.
- [`standardize_adsl()`](https://ygma-bitc.github.io/tm.hyslaw/reference/standardize_adsl.md)
  /
  [`standardize_adlb()`](https://ygma-bitc.github.io/tm.hyslaw/reference/standardize_adlb.md)
  adapters for pharmaverseadam and CDISC Pilot ADLBC (`A1HI`, `ALKPH`).
