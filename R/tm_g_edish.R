#' @keywords internal
build_edish_code_text <- function(adsl_name,
                                  adlb_name,
                                  arm_var,
                                  population_var,
                                  window_days,
                                  exclude_alp,
                                  compare,
                                  which,
                                  color_by,
                                  facet,
                                  annotate_quadrants,
                                  mark_potential) {
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
    "tm.hyslaw::plot_edish(",
    "  classified = classified,",
    sprintf("  which = %s,", deparse(which)),
    sprintf("  color_by = %s,", deparse(color_by)),
    sprintf("  facet = %s,", deparse(facet)),
    sprintf("  annotate_quadrants = %s,", deparse(annotate_quadrants)),
    sprintf("  mark_potential = %s", deparse(mark_potential)),
    ")",
    sep = "\n"
  )
}


#' @keywords internal
ui_g_edish <- function(id, ...) {
  ns <- shiny::NS(id)
  a <- list(...)
  shiny::tagList(
    shinyjs::useShinyjs(),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        shiny::plotOutput(ns("edish_plot"), height = "560px")
      ),
      encoding = shiny::tags$div(
        shiny::tags$label("Encodings", class = "text-primary"),
        shiny::tags$div(
          shiny::tags$label("Treatment / group variable"),
          teal.picks::picks_ui(ns("arm_var"), a$arm_var)
        ),
        shiny::tags$p(
          class = "help-block",
          "Default: one panel colored by group (arm imbalance). Facet is optional."
        ),
        shiny::radioButtons(
          inputId = ns("which"),
          label = "Plot",
          choices = c(
            "eDISH (ALT or AST / ULN)" = "edish",
            "mDISH (peak / baseline)" = "mdish",
            "Cholestatic (ALP / ULN)" = "cholestatic"
          ),
          selected = a$which
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
          inputId = ns("color_by"),
          label = "Color points by",
          choices = c("Group variable" = "arm", "Screening quadrant" = "quadrant"),
          selected = a$color_by
        ),
        shiny::checkboxInput(
          inputId = ns("facet"),
          label = "Facet by group variable",
          value = isTRUE(a$facet)
        ),
        shiny::checkboxInput(
          inputId = ns("annotate_quadrants"),
          label = "Label quadrants on the plot",
          value = isTRUE(a$annotate_quadrants)
        ),
        shiny::checkboxInput(
          inputId = ns("mark_potential"),
          label = "Circle biochemical potential / concurrent cases",
          value = isTRUE(a$mark_potential)
        ),
        shiny::checkboxInput(
          inputId = ns("log_axes"),
          label = "Log10 axes",
          value = isTRUE(a$log_axes)
        )
      ),
      forms = shiny::tagList(
        teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R Code")
      )
    )
  )
}


#' @keywords internal
srv_g_edish <- function(id,
                        data,
                        adsl_name = "ADSL",
                        adlb_name = "ADLB",
                        arm_var,
                        population_var,
                        population_value = "Y",
                        window_days = 14,
                        exclude_alp = TRUE,
                        compare = ">=",
                        which = "edish",
                        color_by = "arm",
                        facet = FALSE,
                        annotate_quadrants = TRUE,
                        mark_potential = TRUE,
                        log_axes = TRUE) {
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

    plot_r <- shiny::reactive({
      shiny::validate(shiny::need(!no_data_r(), "No laboratory records available."))
      plot_edish(
        classified = classified_r(),
        which = input$which %||% which,
        color_by = input$color_by %||% color_by,
        log_axes = isTRUE(input$log_axes),
        facet = isTRUE(input$facet),
        annotate_quadrants = isTRUE(input$annotate_quadrants),
        mark_potential = isTRUE(input$mark_potential)
      )
    })

    output$edish_plot <- shiny::renderPlot({
      plot_r()
    })

    source_code_r <- shiny::reactive({
      build_edish_code_text(
        adsl_name = adsl_name,
        adlb_name = adlb_name,
        arm_var = arm_name_r(),
        population_var = population_name_r(),
        window_days = input$window_days %||% window_days,
        exclude_alp = isTRUE(input$exclude_alp),
        compare = input$compare %||% compare,
        which = input$which %||% which,
        color_by = input$color_by %||% color_by,
        facet = isTRUE(input$facet),
        annotate_quadrants = isTRUE(input$annotate_quadrants),
        mark_potential = isTRUE(input$mark_potential)
      )
    })

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "eDISH - R Code"
    )

    list(plot = plot_r, classified = classified_r, no_data = no_data_r)
  })
}


#' teal Module: eDISH / mDISH / cholestatic screening plot
#'
#' @param label Module label.
#' @param adsl_name ADSL dataset name in `teal_data`.
#' @param adlb_name ADLB dataset name.
#' @param arm_var (`teal.picks::variables()`)\cr
#'   Treatment or grouping column on ADSL. `multiple` must be `FALSE`.
#'   The default selection is `TRT01A`. The control shows column labels from the data.
#' @param population_var (`teal.picks::variables()`)\cr
#'   Population flag column on ADSL. `multiple` must be `FALSE`.
#'   The default selection is `SAFFL`. A length-1 character string is deprecated
#'   and still accepted as the selected flag.
#' @param population_value Population flag value to keep. Not shown in the UI.
#' @param group_vars `r lifecycle::badge("deprecated")` Named character vector of
#'   treatment columns, such as `c("Actual Treatment" = "TRT01A")`. Still accepted.
#'   Converted with [lifecycle::deprecate_warn()] to
#'   `teal.picks::variables(choices, selected, multiple = FALSE)`. Prefer `arm_var`.
#' @param population_choices `r lifecycle::badge("deprecated")` Named character vector
#'   of population flag columns. Still accepted and converted the same way.
#'   Prefer `population_var`.
#' @param window_days Default concurrent window.
#' @param exclude_alp Default ALP exclusion.
#' @param compare Default threshold operator, `">="` or `">"`.
#' @param which `"edish"`, `"mdish"`, or `"cholestatic"`.
#' @param color_by `"arm"` or `"quadrant"`.
#' @param facet Default faceting.
#' @param annotate_quadrants Default corner labels.
#' @param mark_potential Default red-circle overlay.
#' @param log_axes Default log-axis setting.
#'
#' @return A `teal` module object.
#'
#' @examples
#' synth <- make_synthetic_hyslaw_data(n_extra = 0L, seed = 1L)
#' data <- teal.data::teal_data(ADSL = synth$ADSL, ADLB = synth$ADLB)
#' app <- teal::init(
#'   data = data,
#'   modules = teal::modules(
#'     tm_g_edish(
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
tm_g_edish <- function(label = "eDISH plot",
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
                       which = c("edish", "mdish", "cholestatic"),
                       color_by = c("arm", "quadrant"),
                       facet = FALSE,
                       annotate_quadrants = TRUE,
                       mark_potential = TRUE,
                       log_axes = TRUE,
                       group_vars = NULL,
                       population_choices = NULL) {
  compare <- match.arg(compare)
  which <- match.arg(which)
  color_by <- match.arg(color_by)
  checkmate::assert_string(label)
  checkmate::assert_string(adsl_name)
  checkmate::assert_string(adlb_name)
  checkmate::assert_string(population_value)
  checkmate::assert_number(window_days, lower = 0, finite = TRUE)
  checkmate::assert_flag(exclude_alp)
  checkmate::assert_flag(facet)
  checkmate::assert_flag(annotate_quadrants)
  checkmate::assert_flag(mark_potential)
  checkmate::assert_flag(log_axes)

  selectors <- migrate_hyslaw_selectors(
    arm_var = arm_var,
    group_vars = group_vars,
    population_var = population_var,
    population_choices = population_choices,
    adsl_name = adsl_name,
    fn_name = "tm_g_edish",
    user_env = rlang::caller_env()
  )
  arm_var <- selectors$arm_var
  population_var <- selectors$population_var

  teal::module(
    label = label,
    server = srv_g_edish,
    ui = ui_g_edish,
    ui_args = list(
      arm_var = arm_var,
      population_var = population_var,
      window_days = window_days,
      exclude_alp = exclude_alp,
      compare = compare,
      which = which,
      color_by = color_by,
      facet = facet,
      annotate_quadrants = annotate_quadrants,
      mark_potential = mark_potential,
      log_axes = log_axes
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
      which = which,
      color_by = color_by,
      facet = facet,
      annotate_quadrants = annotate_quadrants,
      mark_potential = mark_potential,
      log_axes = log_axes
    ),
    datanames = unique(c(adsl_name, adlb_name))
  )
}
