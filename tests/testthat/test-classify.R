test_that("synthetic data seeds four eDISH quadrants and window patterns", {
  data <- synthetic_hyslaw_fixture()
  expect_named(data, c("ADSL", "ADLB"))
  expect_true(all(c("ALT", "AST", "BILI", "ALP") %in% data$ADLB$PARAMCD))
  expect_false(any(grepl("INTERNAL_STUDY|INTERNAL_PRODUCT", c(data$ADSL$USUBJID, data$ADSL$TRT01A))))

  classified <- classify_hyslaw(data$ADSL, data$ADLB)

  expect_equal(subject_row(classified, "P-HY-01")$edish_quadrant, "hyslaw")
  expect_true(subject_row(classified, "P-HY-01")$concurrent_hyslaw)
  expect_true(subject_row(classified, "P-HY-01")$potential_hyslaw)

  expect_equal(subject_row(classified, "P-TEMP-01")$edish_quadrant, "temple")
  expect_equal(subject_row(classified, "P-BILI-01")$edish_quadrant, "isolated_bili")
  expect_equal(subject_row(classified, "P-BILI-01")$edish_quadrant_label, "Cholestasis")
  expect_equal(subject_row(classified, "P-NORM-01")$edish_quadrant, "normal")
  expect_false(is.na(subject_row(classified, "P-HY-01")$r_ratio))
  expect_true(subject_row(classified, "P-HY-01")$peak_altast_bl > 1)
  expect_true(subject_row(classified, "I-ALP-01")$concurrent_cholestatic)

  peak_only <- subject_row(classified, "I-PEAK-01")
  expect_equal(peak_only$edish_quadrant, "hyslaw")
  expect_false(peak_only$concurrent_hyslaw)

  alp_ex <- subject_row(classified, "I-ALP-01")
  expect_true(alp_ex$concurrent_hyslaw)
  expect_true(alp_ex$alp_elevated_in_window)
  expect_false(alp_ex$potential_hyslaw)
})

test_that("concurrent window boundary matches admiral-style 0 to window_days", {
  data <- synthetic_hyslaw_fixture()
  closed <- classify_hyslaw(data$ADSL, data$ADLB, window_days = 14)
  narrow <- classify_hyslaw(data$ADSL, data$ADLB, window_days = 5)

  expect_true(subject_row(closed, "P-HY-01")$concurrent_hyslaw)
  expect_false(subject_row(closed, "I-PEAK-01")$concurrent_hyslaw)
  expect_false(subject_row(narrow, "P-HY-01")$concurrent_hyslaw)
})

test_that("compare = '>' uses a strict threshold", {
  data <- synthetic_hyslaw_fixture()
  at_idx <- data$ADLB$USUBJID == "P-TEMP-01" &
    data$ADLB$PARAMCD %in% c("ALT", "AST") &
    data$ADLB$ADY == 14L
  data$ADLB$AVAL[at_idx] <- 120
  ge <- classify_hyslaw(data$ADSL, data$ADLB, compare = ">=")
  gt <- classify_hyslaw(data$ADSL, data$ADLB, compare = ">")
  expect_equal(subject_row(ge, "P-TEMP-01")$edish_quadrant, "temple")
  expect_equal(subject_row(gt, "P-TEMP-01")$edish_quadrant, "normal")
})

test_that("ALP exclusion can be turned off", {
  data <- synthetic_hyslaw_fixture()
  classified <- classify_hyslaw(data$ADSL, data$ADLB, exclude_alp = FALSE)
  expect_true(subject_row(classified, "I-ALP-01")$potential_hyslaw)
})

test_that("empty population returns empty = TRUE without error", {
  data <- synthetic_hyslaw_fixture()
  data$ADSL$SAFFL <- "N"
  prepped <- prep_hyslaw_data(data$ADSL, data$ADLB)
  expect_true(prepped$empty)
  classified <- classify_hyslaw(prepped = prepped)
  expect_true(classified$empty)
  expect_equal(nrow(classified$subjects), 0L)
})
