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
  expect_s3_class(g$server_args$arm_var, "picks")
  expect_s3_class(t$server_args$population_var, "picks")
  expect_equal(g$server_args$arm_var$variables$selected, "TRT01A")
  expect_equal(t$server_args$arm_var$variables$selected, "TRT01A")
  expect_equal(g$server_args$population_var$variables$selected, "SAFFL")
  expect_equal(t$server_args$population_var$variables$selected, "SAFFL")
  expect_identical(attr(g$server_args$arm_var$variables, "multiple"), FALSE)
  expect_identical(attr(g$server_args$population_var$variables, "multiple"), FALSE)
  expect_identical(attr(t$server_args$arm_var$variables, "multiple"), FALSE)
  expect_identical(attr(t$server_args$population_var$variables, "multiple"), FALSE)

  open <- expect_silent(tm_g_edish(adsl_name = "ADSL_OPEN", adlb_name = "ADLB_OPEN"))
  expect_equal(open$server_args$arm_var$datasets$selected, "ADSL_OPEN")
  expect_true(all(c("ADSL_OPEN", "ADLB_OPEN") %in% open$datanames))
})

test_that("legacy named group_vars and population_choices are still accepted", {
  lifecycle::expect_deprecated(
    g <- tm_g_edish(group_vars = c("Actual Treatment" = "TRT01A", "Sex" = "SEX")),
    "group_vars"
  )
  expect_s3_class(g, "teal_module")
  expect_equal(unname(g$server_args$arm_var$variables$choices), c("TRT01A", "SEX"))
  expect_equal(unname(g$server_args$arm_var$variables$selected), "TRT01A")
  expect_null(names(g$server_args$arm_var$variables$choices))

  messages <- character()
  withCallingHandlers(
    tbl <- tm_t_hyslaw(
      group_vars = c("Actual Treatment" = "TRT01A"),
      population_choices = c("Safety (SAFFL)" = "SAFFL", "ITT (ITTFL)" = "ITTFL"),
      population_var = "ITTFL"
    ),
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("group_vars", messages, fixed = TRUE)))
  expect_true(any(grepl("population_choices", messages, fixed = TRUE)))
  expect_s3_class(tbl, "teal_module")
  expect_equal(unname(tbl$server_args$arm_var$variables$selected), "TRT01A")
  expect_equal(unname(tbl$server_args$population_var$variables$choices), c("SAFFL", "ITTFL"))
  expect_equal(unname(tbl$server_args$population_var$variables$selected), "ITTFL")
  expect_null(names(tbl$server_args$population_var$variables$choices))
})

test_that("a length-1 population_var string selects that flag", {
  lifecycle::expect_deprecated(
    g <- tm_g_edish(population_var = "ITTFL"),
    "population_var"
  )
  expect_equal(unname(g$server_args$population_var$variables$selected), "ITTFL")
  expect_true("SAFFL" %in% g$server_args$population_var$variables$choices)
})

test_that("multiple = TRUE is rejected", {
  expect_error(
    tm_g_edish(
      arm_var = teal.picks::variables(
        choices = c("TRT01A", "SEX"),
        selected = c("TRT01A", "SEX"),
        multiple = TRUE
      )
    ),
    "multiple = FALSE"
  )
})

test_that("servers pass the selected column names to classify_hyslaw()", {
  synth <- make_synthetic_hyslaw_data(n_extra = 0L, seed = 1L)
  app_data <- teal.data::teal_data(ADSL = synth$ADSL, ADLB = synth$ADLB)
  g <- tm_g_edish(
    arm_var = teal.picks::variables(choices = c("TRT01A", "SEX"), selected = "SEX", multiple = FALSE)
  )
  shiny::testServer(
    g$server,
    args = c(list(data = shiny::reactive(app_data)), g$server_args),
    {
      session$setInputs(
        window_days = 14,
        compare = ">=",
        which = "edish",
        color_by = "arm",
        facet = FALSE,
        exclude_alp = TRUE,
        annotate_quadrants = TRUE,
        mark_potential = TRUE,
        log_axes = TRUE
      )
      classified <- session$returned$classified()
      expect_equal(classified$arm_var, "SEX")
      expect_true("SEX" %in% names(classified$subjects))
    }
  )

  tbl <- tm_t_hyslaw()
  shiny::testServer(
    tbl$server,
    args = c(list(data = shiny::reactive(app_data)), tbl$server_args),
    {
      session$setInputs(
        window_days = 14,
        compare = ">=",
        exclude_alp = TRUE,
        show_total = TRUE,
        table_which = "both"
      )
      classified <- session$returned$classified()
      expect_equal(classified$arm_var, "TRT01A")
      expect_true(nrow(classified$subjects) > 0L)
    }
  )
})
