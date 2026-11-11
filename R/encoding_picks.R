#' Badge label: selected column only.
#'
#' `teal.picks` prints the dataset and the column in one badge, and that string
#' is hard-coded in `picks_srv()`. These modules fix the dataset, so the badge
#' shows the column. The tooltip still names both, and the menu still lists
#' column labels from the data.
#'
#' @param pick A resolved or unresolved `picks` object.
#' @return A `shiny.tag`.
#' @keywords internal
#' @noRd
variable_badge <- function(pick) {
  variable <- selected_column_name(pick)
  if (length(variable) == 0L) {
    variable <- "~"
  } else {
    variable <- paste(variable, collapse = ", ")
  }
  dataset <- paste(as.character(pick$datasets$selected), collapse = ", ")
  htmltools::tags$span(
    title = sprintf("datasets: %s\nvariables: %s", dataset, variable),
    variable
  )
}

bind_variable_badges <- function(output, selectors) {
  output$arm_var_badge <- shiny::renderUI(variable_badge(selectors$arm_var()))
  output$population_var_badge <- shiny::renderUI(variable_badge(selectors$population_var()))
  invisible(NULL)
}

hyslaw_encoding_dependency <- function() {
  htmltools::htmlDependency(
    name = "tm-hyslaw-encoding",
    version = as.character(utils::packageVersion("tm.hyslaw")),
    src = "www",
    package = "tm.hyslaw",
    stylesheet = "encoding-picks.css"
  )
}

#' Badge dropdown with a caller-supplied label.
#'
#' The ids match what `teal.picks` script.js toggles. Assets are loaded from
#' that package so the menu behaviour stays upstream.
#'
#' @keywords internal
#' @noRd
hyslaw_badge_dropdown <- function(id, label, content) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::singleton(htmltools::tags$head(
      htmltools::includeCSS(
        system.file("badge-dropdown", "style.css", package = "teal.picks")
      ),
      htmltools::includeScript(
        system.file("badge-dropdown", "script.js", package = "teal.picks")
      )
    )),
    htmltools::tags$div(
      class = "badge-wrapper",
      htmltools::tags$span(
        id = ns("summary_badge"),
        class = "badge bg-primary rounded-pill badge-dropdown",
        style = "cursor: pointer;",
        htmltools::tags$span(class = "badge-dropdown-label", label),
        htmltools::tags$span(class = "badge-dropdown-icon", "\u25be"),
        onclick = sprintf(
          "toggleBadgeDropdown('%s', '%s')",
          ns("summary_badge"),
          ns("inputs_container")
        )
      ),
      htmltools::tags$div(
        # teal.picks moves this node to document.body while it is open.
        class = "tm-hyslaw-picks-menu",
        content,
        id = ns("inputs_container"),
        style = htmltools::css(
          visibility = "hidden",
          width = "0",
          position = "fixed",
          background = "white",
          border = "1px solid #ccc",
          `border-radius` = "4px",
          `box-shadow` = "0 2px 10px rgba(0,0,0,0.1)",
          padding = "10px",
          `z-index` = "1050",
          `min-width` = "400px",
          transition = "opacity 0.2s ease",
          opacity = 0
        )
      )
    )
  )
}

hyslaw_picks_ui <- function(ns, input_id, picks) {
  teal.picks::picks_ui(
    ns(input_id),
    picks,
    container = function(content, id, ...) {
      # `...` receives the upstream badge text (dataset and column). The
      # label rendered here is the column name only.
      hyslaw_badge_dropdown(
        id = id,
        label = shiny::uiOutput(ns(paste0(input_id, "_badge")), inline = TRUE),
        content = content
      )
    }
  )
}
