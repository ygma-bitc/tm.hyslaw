#' @keywords internal
build_hyslaw_code_text <- function(adsl_name,
                                   adlb_name,
                                   arm_var,
                                   population_var,
                                   window_days,
                                   exclude_alp,
                                   compare,
                                   show_total,
                                   table_which) {
  tbl_call <- if (identical(table_which, "elevation")) {
    paste(
      "tm.hyslaw::build_liver_elevation_table(",
      "  classified = classified,",
      sprintf("  show_total = %s", if (isTRUE(show_total)) "TRUE" else "FALSE"),
      ")",
      sep = "\n"
    )
  } else if (identical(table_which, "both")) {
    paste(
      "tm.hyslaw::build_hyslaw_table(classified = classified,",
      sprintf("  show_total = %s)", if (isTRUE(show_total)) "TRUE" else "FALSE"),
      "tm.hyslaw::build_liver_elevation_table(classified = classified,",
      sprintf("  show_total = %s)", if (isTRUE(show_total)) "TRUE" else "FALSE"),
      sep = "\n"
    )
  } else {
    paste(
      "tm.hyslaw::build_hyslaw_table(",
      "  classified = classified,",
      sprintf("  show_total = %s", if (isTRUE(show_total)) "TRUE" else "FALSE"),
      ")",
      sep = "\n"
    )
  }
  paste(
    sprintf("classified <- tm.hyslaw::classify_hyslaw("),
    sprintf("  adsl = %s,", adsl_name),
    sprintf("  adlb = %s,", adlb_name),
    sprintf("  arm_var = %s,", deparse(arm_var)),
    sprintf("  population_var = %s,", deparse(population_var)),
    sprintf("  window_days = %s,", deparse(window_days)),
    sprintf("  exclude_alp = %s,", deparse(exclude_alp)),
    sprintf("  compare = %s", deparse(compare)),
    ")",
    tbl_call,
    sep = "\n"
  )
}


#' @keywords internal
ui_t_hyslaw <- function(id, ...) {
  ns <- shiny::NS(id)
  a <- list(...)
  shiny::tagList(
    shinyjs::useShinyjs(),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        shiny::uiOutput(ns("table_or_message")),
        shiny::uiOutput(ns("elevation_or_message")),
        shiny::tags$hr(),
        shiny::h4("Notable subject listing"),
        shiny::tableOutput(ns("listing"))
      ),
      encoding = shiny::tags$div(
        shiny::tags$label("Encodings", class = "text-primary"),
        shiny::tags$div(
          shiny::tags$label("Treatment / group variable"),
          teal.picks::picks_ui(ns("arm_var"), a$arm_var)
        ),
        shiny::tags$div(
          shiny::tags$label("Population flag"),
          teal.picks::picks_ui(ns("population_var"), a$population_var)
        ),
        shiny::numericInput(
          inputId = ns("window_days"),
          label = "Concurrent window (days)",
          value = a$window_days,
          min = 0,
          step = 1
        ),
        shiny::radioButtons(
          inputId = ns("compare"),
          label = "Threshold operator",
          choices = c(">=" = ">=", ">" = ">"),
          selected = a$compare
        ),
        shiny::checkboxInput(
          inputId = ns("exclude_alp"),
          label = "Exclude concurrent cases with ALP >= cut in window",
          value = isTRUE(a$exclude_alp)
        ),
        shiny::radioButtons(
          inputId = ns("table_which"),
          label = "Tables",
          choices = c(
            "Peak quadrants + concurrent" = "review",
            "Nested elevations" = "elevation",
            "Both" = "both"
          ),
          selected = a$table_which
        ),
        shiny::checkboxInput(
          inputId = ns("show_total"),
          label = "Show Total column",
          value = isTRUE(a$show_total)
        )
      ),
      forms = shiny::tagList(
        teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R Code")
      )
    )
  )
}


#' @keywords internal
srv_t_hyslaw <- function(id,
                         data,
                         adsl_name = "ADSL",
                         adlb_name = "ADLB",
                         arm_var,
                         population_var,
                         population_value = "Y",
                         window_days = 14,
                         exclude_alp = TRUE,
                         compare = ">=",
                         show_total = TRUE,
                         table_which = "both") {
  checkmate::assert_class(data, "reactive")

  shiny::moduleServer(id, function(input, output, session) {
    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(arm_var = arm_var, population_var = population_var),
      data = data
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("window_days", function(value) {
        if (is.null(value)) {
          return()
        }
        if (length(value) != 1L || is.na(value) || value < 0) {
          "Window must be a non-negative number."
        }
      })
      iv$enable()
      iv
    })

    arm_name_r <- shiny::reactive({
      selected <- selected_column_name(selectors$arm_var())
      shiny::validate(shiny::need(
        length(selected) == 1L && nzchar(selected),
        "Select a treatment / group variable."
      ))
      selected
    })

    population_name_r <- shiny::reactive({
      selected <- selected_column_name(selectors$population_var())
      shiny::validate(shiny::need(
        length(selected) == 1L && nzchar(selected),
        "Select a population flag."
      ))
      selected
    })

    classified_r <- shiny::reactive({
      teal::validate_inputs(iv_r())
      req_data <- data()
      shiny::validate(
        shiny::need(adsl_name %in% names(req_data), sprintf("Dataset '%s' is missing.", adsl_name)),
        shiny::need(adlb_name %in% names(req_data), sprintf("Dataset '%s' is missing.", adlb_name))
      )
      classify_hyslaw(
        adsl = req_data[[adsl_name]],
        adlb = req_data[[adlb_name]],
        arm_var = arm_name_r(),
        population_var = population_name_r(),
        population_value = population_value,
        window_days = input$window_days %||% window_days,
        exclude_alp = isTRUE(input$exclude_alp),
        compare = input$compare %||% compare
      )
    })

    no_data_r <- shiny::reactive({
      classified <- classified_r()
      isTRUE(classified$empty) || NROW(classified$subjects) == 0L
    })

    show_review_r <- shiny::reactive({
      identical(input$table_which %||% table_which, "review") ||
        identical(input$table_which %||% table_which, "both")
    })
    show_elev_r <- shiny::reactive({
      identical(input$table_which %||% table_which, "elevation") ||
        identical(input$table_which %||% table_which, "both")
    })

    table_r <- shiny::reactive({
      shiny::validate(shiny::need(!no_data_r(), "No laboratory records available."))
      shiny::req(show_review_r())
      build_hyslaw_table(
        classified = classified_r(),
        show_total = isTRUE(input$show_total)
      )
    })

    elevation_r <- shiny::reactive({
      shiny::validate(shiny::need(!no_data_r(), "No laboratory records available."))
      shiny::req(show_elev_r())
      build_liver_elevation_table(
        classified = classified_r(),
        show_total = isTRUE(input$show_total)
      )
    })

    output$table_or_message <- shiny::renderUI({
      if (isTRUE(no_data_r())) {
        return(teal.widgets::white_small_well(
          shiny::div(shiny::tags$strong("No laboratory records available."))
        ))
      }
      if (!isTRUE(show_review_r())) {
        return(NULL)
      }
      teal.widgets::table_with_settings_ui(session$ns("table_out"))
    })

    output$elevation_or_message <- shiny::renderUI({
      if (isTRUE(no_data_r()) || !isTRUE(show_elev_r())) {
        return(NULL)
      }
      shiny::tagList(
        shiny::tags$hr(),
        teal.widgets::table_with_settings_ui(session$ns("elevation_out"))
      )
    })

    teal.widgets::table_with_settings_srv(id = "table_out", table_r = table_r)
    teal.widgets::table_with_settings_srv(id = "elevation_out", table_r = elevation_r)

    output$listing <- shiny::renderTable({
      if (isTRUE(no_data_r())) {
        return(NULL)
      }
      hyslaw_listing(classified = classified_r(), which = "notable")
    })

    source_code_r <- shiny::reactive({
      build_hyslaw_code_text(
        adsl_name = adsl_name,
        adlb_name = adlb_name,
        arm_var = arm_name_r(),
        population_var = population_name_r(),
        window_days = input$window_days %||% window_days,
        exclude_alp = isTRUE(input$exclude_alp),
        compare = input$compare %||% compare,
        show_total = isTRUE(input$show_total),
        table_which = input$table_which %||% table_which
      )
    })

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "Hy's Law table - R Code"
    )

    list(
      table = table_r,
      elevation = elevation_r,
      classified = classified_r,
      no_data = no_data_r
    )
  })
}


#' teal Module: Hy's Law summary table and notable listing
#'
#' @inheritParams tm_g_edish
#' @param show_total Initial Total-column setting.
#' @param table_which `"review"`, `"elevation"`, or `"both"`.
#'
#' @return A `teal` module object.
#'
#' @examples
#' synth <- make_synthetic_hyslaw_data(n_extra = 0L, seed = 1L)
#' data <- teal.data::teal_data(ADSL = synth$ADSL, ADLB = synth$ADLB)
#' app <- teal::init(
#'   data = data,
#'   modules = teal::modules(
#'     tm_t_hyslaw(
#'       arm_var = teal.picks::variables(
#'         choices = c("TRT01A", "SEX", "RACE", "COUNTRY"),
#'         selected = "TRT01A",
#'         multiple = FALSE
#'       ),
#'       population_var = teal.picks::variables(
#'         choices = c("SAFFL", "ITTFL"),
#'         selected = "SAFFL",
#'         multiple = FALSE
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shiny::shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_hyslaw <- function(label = "Hy's Law table",
                        adsl_name = "ADSL",
                        adlb_name = "ADLB",
                        arm_var = teal.picks::variables(
                          choices = c("TRT01A", "TRT01P", "ARM", "SEX", "RACE", "COUNTRY"),
                          selected = "TRT01A",
                          multiple = FALSE
                        ),
                        population_var = teal.picks::variables(
                          choices = c("SAFFL", "ITTFL"),
                          selected = "SAFFL",
                          multiple = FALSE
                        ),
                        population_value = "Y",
                        window_days = 14,
                        exclude_alp = TRUE,
                        compare = c(">=", ">"),
                        show_total = TRUE,
                        table_which = c("both", "review", "elevation"),
                        group_vars = NULL,
                        population_choices = NULL) {
  compare <- match.arg(compare)
  table_which <- match.arg(table_which)
  checkmate::assert_string(label)
  checkmate::assert_string(adsl_name)
  checkmate::assert_string(adlb_name)
  checkmate::assert_string(population_value)
  checkmate::assert_number(window_days, lower = 0, finite = TRUE)
  checkmate::assert_flag(exclude_alp)
  checkmate::assert_flag(show_total)

  selectors <- migrate_hyslaw_selectors(
    arm_var = arm_var,
    group_vars = group_vars,
    population_var = population_var,
    population_choices = population_choices,
    adsl_name = adsl_name,
    fn_name = "tm_t_hyslaw",
    user_env = rlang::caller_env()
  )
  arm_var <- selectors$arm_var
  population_var <- selectors$population_var

  teal::module(
    label = label,
    server = srv_t_hyslaw,
    ui = ui_t_hyslaw,
    ui_args = list(
      arm_var = arm_var,
      population_var = population_var,
      window_days = window_days,
      exclude_alp = exclude_alp,
      compare = compare,
      show_total = show_total,
      table_which = table_which
    ),
    server_args = list(
      adsl_name = adsl_name,
      adlb_name = adlb_name,
      arm_var = arm_var,
      population_var = population_var,
      population_value = population_value,
      window_days = window_days,
      exclude_alp = exclude_alp,
      compare = compare,
      show_total = show_total,
      table_which = table_which
    ),
    datanames = unique(c(adsl_name, adlb_name))
  )
}
