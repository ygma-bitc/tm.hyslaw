#' Visible badge text for one teal.picks selector.
#'
#' The upstream badge prints every slot, so the dataset name is prefixed
#' onto the column. There is no argument to drop that slot. The dataset
#' stays on the tooltip, in the same `datasets:` / `variables:` form.
#' The column text comes from `selected_column_name()`.
#'
#' @param pick A resolved or unresolved `picks` object.
#' @return A `shiny.tag`.
#' @keywords internal
#' @noRd
variable_badge <- function(pick) {
  variable <- drop_blank(selected_column_name(pick))
  dataset <- drop_blank(pick$datasets$selected)
  variable <- if (length(variable)) paste(variable, collapse = ", ") else "~"
  dataset <- if (length(dataset)) paste(dataset, collapse = ", ") else "~"
  shiny::tags$span(
    title = sprintf("datasets: %s\nvariables: %s", dataset, variable),
    variable
  )
}

#' Drop missing and empty strings. Shared by the badge column and dataset.
#'
#' @keywords internal
#' @noRd
drop_blank <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & nzchar(x)]
}

picks_are_fixed <- function(picks) {
  all(vapply(picks, teal.picks::is_pick_fixed, logical(1)))
}

#' Badge container that shows `variable_badge()` instead of the upstream summary.
#'
#' `badge_dropdown()` and `badge_fixed()` stay in teal.picks. This only
#' chooses which one to call and passes the column name as the label.
#'
#' @keywords internal
#' @noRd
picks_badge_container <- function(label_id, fixed) {
  force(label_id)
  force(fixed)
  badge_ui <- utils::getFromNamespace(
    if (isTRUE(fixed)) "badge_fixed" else "badge_dropdown",
    "teal.picks"
  )
  function(content, id, label) {
    # `label` is the upstream summary, which prefixes the dataset name.
    badge_ui(
      id = id,
      label = shiny::uiOutput(label_id, inline = TRUE),
      content = content
    )
  }
}

hyslaw_picks_ui <- function(ns, input_id, picks) {
  teal.picks::picks_ui(
    ns(input_id),
    picks,
    container = picks_badge_container(
      ns(paste0(input_id, "_badge")),
      picks_are_fixed(picks)
    )
  )
}

bind_variable_badges <- function(output, selectors) {
  for (input_id in c("arm_var", "population_var")) {
    local({
      id <- input_id
      output[[paste0(id, "_badge")]] <- shiny::renderUI(
        variable_badge(selectors[[id]]())
      )
    })
  }
  invisible(NULL)
}
