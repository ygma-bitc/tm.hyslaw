test_that("standardize_adlb() maps A1HI and ALKPH", {
  raw <- data.frame(
    USUBJID = "A",
    PARAMCD = "ALKPH",
    AVAL = 240,
    A1HI = 120,
    ADT = as.Date("2024-01-10"),
    stringsAsFactors = FALSE
  )
  out <- standardize_adlb(raw)
  expect_equal(out$PARAMCD, "ALP")
  expect_equal(out$ANRHI, 120)
  expect_equal(out$AVAL, 240)
})

test_that("tm_g_edish() and tm_t_hyslaw() return teal modules", {
  g <- expect_silent(tm_g_edish(label = "eDISH"))
  t <- expect_silent(tm_t_hyslaw(label = "Hy's Law"))
  expect_s3_class(g, "teal_module")
  expect_s3_class(t, "teal_module")
  expect_true(all(c("ADSL", "ADLB") %in% g$datanames))
  expect_true(all(c("ADSL", "ADLB") %in% t$datanames))
})

test_that("tm_g_edish() validates named group_vars", {
  expect_error(tm_g_edish(group_vars = "TRT01A"), regexp = "named|Assertion")
})
