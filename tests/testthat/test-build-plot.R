test_that("build_hyslaw_table() splits peak quadrants from concurrent", {
  data <- synthetic_hyslaw_fixture()
  tbl <- build_hyslaw_table(data$ADSL, data$ADLB)
  text <- printed_table(tbl)

  expect_s4_class(tbl, "VTableTree")
  expect_match(text, "peak Hy's Law quadrant")
  expect_match(text, "Temple's corollary")
  expect_match(text, "Cholestasis")
  expect_match(text, "Normal or near normal")
  expect_match(text, "Biochemical concurrent")
  expect_match(text, "Potential Hy's Law after ALP exclusion")
  expect_match(text, "Placebo")
  expect_match(text, "Investigational")
  expect_match(text, "Threshold operator is >=")
  expect_match(text, "on-treatment maximum")
})

test_that("build_liver_elevation_table() has nested cuts and combination", {
  data <- synthetic_hyslaw_fixture()
  tbl <- build_liver_elevation_table(data$ADSL, data$ADLB)
  text <- printed_table(tbl)

  expect_s4_class(tbl, "VTableTree")
  expect_match(text, "ALT or AST")
  expect_match(text, "3x ULN")
  expect_match(text, "20x ULN")
  expect_match(text, "Combination")
  expect_match(text, "ALP")
})

test_that("hyslaw_listing() keeps peak-only and ALP-excluded subjects and R-ratio", {
  data <- synthetic_hyslaw_fixture()
  listing <- hyslaw_listing(data$ADSL, data$ADLB, which = "notable")
  expect_true("I-PEAK-01" %in% listing$USUBJID)
  expect_true("I-ALP-01" %in% listing$USUBJID)
  expect_false("P-NORM-01" %in% listing$USUBJID)
  expect_true("r_ratio" %in% names(listing))
  expect_true("injury_pattern" %in% names(listing))
  expect_true("potential_hyslaw" %in% names(listing))
})

test_that("plot_edish() defaults to one panel colored by group", {
  data <- synthetic_hyslaw_fixture()
  p_default <- plot_edish(data$ADSL, data$ADLB)
  p_facet <- plot_edish(data$ADSL, data$ADLB, color_by = "quadrant", facet = TRUE)
  p_sex <- plot_edish(data$ADSL, data$ADLB, arm_var = "SEX", facet = TRUE)

  expect_s3_class(p_default, "ggplot")
  expect_false(inherits(p_default$facet, "FacetWrap"))
  expect_s3_class(p_facet$facet, "FacetWrap")
  expect_equal(rlang::as_name(p_facet$facet$params$facets[[1L]]), "TRT01A")
  expect_equal(rlang::as_name(p_sex$facet$params$facets[[1L]]), "SEX")
  expect_match(p_default$labels$caption, "not required to occur on the same day")
  expect_match(p_default$labels$caption, "biochemical potential")
})

test_that("plot_edish() draws mDISH and cholestatic screens", {
  data <- synthetic_hyslaw_fixture()
  p_mdish <- plot_edish(data$ADSL, data$ADLB, which = "mdish")
  p_cdish <- plot_edish(data$ADSL, data$ADLB, which = "cholestatic")

  expect_s3_class(p_mdish, "ggplot")
  expect_s3_class(p_cdish, "ggplot")
  expect_match(p_mdish$labels$x, "baseline")
  expect_match(p_cdish$labels$x, "ALP")
  expect_match(p_cdish$labels$caption, "Cholestatic|ALP")
})
