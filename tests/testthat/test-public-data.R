find_pilot_xpt <- function(filename) {
  candidates <- c(
    file.path("data-raw", filename),
    file.path("..", "..", "data-raw", filename)
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) {
    return(NULL)
  }
  normalizePath(existing[[1L]], winslash = "/", mustWork = TRUE)
}

test_that("pharmaverseadam ADLB classifies without error", {
  skip_if_not_installed("pharmaverseadam")
  adsl <- tryCatch(pharmaverseadam::adsl, error = function(e) NULL)
  adlb <- tryCatch(pharmaverseadam::adlb, error = function(e) NULL)
  skip_if(is.null(adsl) || is.null(adlb), "pharmaverseadam datasets are unavailable")

  classified <- expect_no_error(
    classify_hyslaw(standardize_adsl(adsl), standardize_adlb(adlb, adsl))
  )
  expect_false(classified$empty)
  expect_true(NROW(classified$subjects) > 0L)
  tbl <- expect_no_error(build_hyslaw_table(classified = classified))
  expect_s4_class(tbl, "VTableTree")
})

test_that("CDISC Pilot ADLBC standardizes and builds a table", {
  skip_if_not_installed("haven")
  adsl_path <- find_pilot_xpt("adsl.xpt")
  adlb_path <- find_pilot_xpt("adlbc.xpt")
  skip_if(is.null(adsl_path) || is.null(adlb_path), "CDISC Pilot XPTs are not in data-raw/")

  adsl <- standardize_adsl(haven::read_xpt(adsl_path))
  adlb <- standardize_adlb(haven::read_xpt(adlb_path), adsl)
  classified <- expect_no_error(classify_hyslaw(adsl, adlb))
  expect_s4_class(build_hyslaw_table(classified = classified), "VTableTree")
})
