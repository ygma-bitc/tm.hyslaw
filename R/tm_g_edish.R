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
        shiny::selectizeInput(
          inputId = ns("arm_var"),
          label = "Treatment / group variable",
          choices = a$group_vars,
          selected = unname(a$group_vars[[1L]]),
          multiple = FALSE,
          width = "100%"
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
        shiny::selectizeInput(
          inputId = ns("population_var"),
          label = "Population flag",
          choices = a$population_choices,
          selected = a$population_var,
          multiple = FALSE,
          width = "100%"
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
                        group_vars = c("Actual Treatment" = "TRT01A"),
                        population_var = "SAFFL",
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
    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("arm_var", ~ if (!nzchar(. %||% "")) "Select a treatment / group variable.")
      iv$add_rule("window_days", ~ if (is.na(.) || . < 0) "Window must be a non-negative number.")
      iv$enable()
      iv
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
        arm_var = input$arm_var,
        population_var = input$population_var %||% population_var,
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
        arm_var = input$arm_var,
        population_var = input$population_var %||% population_var,
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
#' @param group_vars Named character vector of grouping variables.
#' @param population_var Default population flag.
#' @param population_value Population flag value to keep.
#' @param population_choices Named choices shown in the population selector.
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
#' @export
tm_g_edish <- function(label = "eDISH plot",
                       adsl_name = "ADSL",
                       adlb_name = "ADLB",
                       group_vars = c("Actual Treatment" = "TRT01A"),
                       population_var = "SAFFL",
                       population_value = "Y",
                       population_choices = c(
                         "Safety (SAFFL)" = "SAFFL",
                         "ITT (ITTFL)" = "ITTFL"
                       ),
                       window_days = 14,
                       exclude_alp = TRUE,
                       compare = c(">=", ">"),
                       which = c("edish", "mdish", "cholestatic"),
                       color_by = c("arm", "quadrant"),
                       facet = FALSE,
                       annotate_quadrants = TRUE,
                       mark_potential = TRUE,
                       log_axes = TRUE) {
  compare <- match.arg(compare)
  which <- match.arg(which)
  color_by <- match.arg(color_by)
  checkmate::assert_string(label)
  checkmate::assert_string(adsl_name)
  checkmate::assert_string(adlb_name)
  checkmate::assert_character(group_vars, min.len = 1L, names = "named")
  checkmate::assert_string(population_var)
  checkmate::assert_string(population_value)
  checkmate::assert_character(population_choices, min.len = 1L, names = "named")
  checkmate::assert_number(window_days, lower = 0, finite = TRUE)
  checkmate::assert_flag(exclude_alp)
  checkmate::assert_flag(facet)
  checkmate::assert_flag(annotate_quadrants)
  checkmate::assert_flag(mark_potential)
  checkmate::assert_flag(log_axes)

  teal::module(
    label = label,
    server = srv_g_edish,
    ui = ui_g_edish,
    ui_args = list(
      group_vars = group_vars,
      population_var = population_var,
      population_choices = population_choices,
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
      group_vars = group_vars,
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
