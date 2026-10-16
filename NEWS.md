# tm.hyslaw 0.2.0

* Added R CMD check and pkgdown GitHub Actions workflows for release-ready open-source development.
* Added `_pkgdown.yml` reference grouping and a lightweight `.lintr` configuration.
* README now includes CI/pkgdown badges and local quality-gate commands.
* Fixed the R CMD check NOTE from unqualified `runif()` by using `stats::runif()`.
* eDISH plot defaults to one panel colored by treatment, with quadrant names on the plot and red open circles for biochemical potential Hy's Law. Caption states that axis maxima need not be the same day.
* Optional mDISH (`which = "mdish"`) and cholestatic ALP vs bilirubin screen (`which = "cholestatic"`). Faceting by group is opt-in.
* Peak maxima use on-treatment records; baseline is kept for mDISH ratios.
* Review table splits ST&F-style four peak quadrants from concurrent-window counts. New `build_liver_elevation_table()` nested 3/5/10/20x and combination-plus-ALP rows.
* `compare = ">="` (default) or `">"`; footnotes document the operator, missing ALP, on-treatment vs baseline, and that ADLB does not support symptoms / alternative causes / confirmed Hy's Law.
* Listing adds R-ratio and injury pattern.
* Teal modules expose plot type, threshold operator, and stacked review / elevation tables.

# tm.hyslaw 0.1.0

* Initial Hy's Law / eDISH review builders that run without Shiny.
* Peak eDISH quadrants plus concurrent ALT/AST and bilirubin elevations (default 14-day window, aligned with the admiral Hy's Law vignette).
* Optional ALP >= 2x ULN exclusion in the same window.
* `build_hyslaw_table()`, `hyslaw_listing()`, and `plot_edish()`.
* Teal modules `tm_g_edish()` and `tm_t_hyslaw()`.
* Synthetic ADLB fixtures seed four eDISH quadrants, a peak-only pair outside the window, and a concurrent pair with elevated ALP.
* `standardize_adsl()` / `standardize_adlb()` adapters for pharmaverseadam and CDISC Pilot ADLBC (`A1HI`, `ALKPH`).
