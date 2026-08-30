# Local teal demo for tm_g_edish() and tm_t_hyslaw()
#
# Data priority:
#   1. Existing ADSL / ADLB objects in the global environment
#   2. pharmaverseadam::adsl / adlb
#   3. Local CDISC Pilot XPTs in data-raw/ (adsl.xpt, adlbc.xpt)
#   4. Fully synthetic mock data via make_synthetic_hyslaw_data()
#
# CDISC Pilot source (attribute to CDISC; do not commit XPTs):
#   https://github.com/cdisc-org/sdtm-adam-pilot-project
#
# Exploratory review only; not a submission TFL.

suppressPackageStartupMessages({
  library(shiny)
  library(teal)
  library(teal.data)
  library(tm.hyslaw)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

pick_object <- function(...) {
  object_names <- c(...)
  for (nm in object_names) {
    if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
      return(get(nm, envir = .GlobalEnv))
    }
  }
  NULL
}

find_data_raw_file <- function(filename) {
  candidates <- c(
    file.path("data-raw", filename),
    file.path("..", "data-raw", filename),
    file.path("..", "..", "data-raw", filename)
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) {
    return(NULL)
  }
  normalizePath(existing[[1L]], winslash = "/", mustWork = TRUE)
}

load_pharmaverseadam <- function() {
  if (!requireNamespace("pharmaverseadam", quietly = TRUE)) {
    return(NULL)
  }
  adsl <- tryCatch(pharmaverseadam::adsl, error = function(e) NULL)
  adlb <- tryCatch(pharmaverseadam::adlb, error = function(e) NULL)
  if (is.null(adsl) || is.null(adlb)) {
    return(NULL)
  }
  message("Loading pharmaverseadam::adsl / adlb (exploratory demo, not a submission TFL).")
  list(
    ADSL = standardize_adsl(adsl),
    ADLB = standardize_adlb(adlb, adsl),
    source = "pharmaverseadam"
  )
}

load_cdisc_pilot_xpt <- function() {
  if (!requireNamespace("haven", quietly = TRUE)) {
    return(NULL)
  }
  adsl_path <- find_data_raw_file("adsl.xpt")
  adlb_path <- find_data_raw_file("adlbc.xpt")
  if (is.null(adsl_path) || is.null(adlb_path)) {
    return(NULL)
  }
  message(
    "Loading CDISC Pilot ADaM XPTs from data-raw/ ",
    "(source: cdisc-org/sdtm-adam-pilot-project; attribute to CDISC)."
  )
  adsl <- standardize_adsl(haven::read_xpt(adsl_path))
  adlb <- standardize_adlb(haven::read_xpt(adlb_path), adsl)
  list(ADSL = adsl, ADLB = adlb, source = "cdisc-pilot")
}

group_vars_from_adsl <- function(adsl) {
  candidate_group_vars <- c(
    "Actual Treatment" = "TRT01A",
    "Country" = "COUNTRY",
    "Race" = "RACE",
    "Sex" = "SEX"
  )
  keep <- candidate_group_vars[candidate_group_vars %in% names(adsl)]
  if (length(keep) == 0L) {
    keep <- c("Actual Treatment" = "TRT01A")
  }
  keep
}

synthetic <- make_synthetic_hyslaw_data(n_extra = 8L, seed = 20260829L)

real_adsl <- pick_object("ADSL", "adsl")
real_adlb <- pick_object("ADLB", "adlb", "ADLBC", "adlbc")
open_data <- NULL
data_source <- "synthetic"

if (!is.null(real_adsl) && !is.null(real_adlb)) {
  open_data <- list(
    ADSL = standardize_adsl(real_adsl),
    ADLB = standardize_adlb(real_adlb, real_adsl),
    source = "global-env"
  )
} else {
  open_data <- load_pharmaverseadam()
  if (is.null(open_data)) {
    open_data <- load_cdisc_pilot_xpt()
  }
}

data_args <- list(
  ADSL = synthetic$ADSL,
  ADLB = synthetic$ADLB
)
modules_list <- list(
  tm_g_edish(
    label = "eDISH (synthetic)",
    adsl_name = "ADSL",
    adlb_name = "ADLB",
    group_vars = group_vars_from_adsl(synthetic$ADSL)
  ),
  tm_t_hyslaw(
    label = "Hy's Law table (synthetic)",
    adsl_name = "ADSL",
    adlb_name = "ADLB",
    group_vars = group_vars_from_adsl(synthetic$ADSL)
  )
)

if (!is.null(open_data)) {
  data_source <- open_data$source %||% "external"
  data_args$ADSL_OPEN <- open_data$ADSL
  data_args$ADLB_OPEN <- open_data$ADLB
  open_label <- if (identical(data_source, "cdisc-pilot")) {
    "CDISC Pilot"
  } else if (identical(data_source, "pharmaverseadam")) {
    "pharmaverseadam"
  } else {
    data_source
  }
  modules_list <- c(
    modules_list,
    list(
      tm_g_edish(
        label = sprintf("eDISH (%s)", open_label),
        adsl_name = "ADSL_OPEN",
        adlb_name = "ADLB_OPEN",
        group_vars = group_vars_from_adsl(open_data$ADSL)
      ),
      tm_t_hyslaw(
        label = sprintf("Hy's Law table (%s)", open_label),
        adsl_name = "ADSL_OPEN",
        adlb_name = "ADLB_OPEN",
        group_vars = group_vars_from_adsl(open_data$ADSL)
      )
    )
  )
}

message(sprintf("Demo open-data source: %s", data_source))

data_args$code <- c(
  "# ADSL / ADLB prepared for tm.hyslaw",
  "# Synthetic fixtures seed eDISH quadrants; public ADaM is exploratory only"
)

app_data <- do.call(teal.data::teal_data, data_args)

app <- teal::init(
  data = app_data,
  modules = do.call(teal::modules, c(list(label = "Lab"), modules_list))
)

shinyApp(ui = app$ui, server = app$server)
