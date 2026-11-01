#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' @keywords internal
assert_required_cols <- function(df, cols, df_name = deparse(substitute(df))) {
  checkmate::assert_data_frame(df)
  checkmate::assert_character(cols, min.len = 1L, any.missing = FALSE)
  checkmate::assert_string(df_name)

  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf("[%s] Missing required column(s): %s", df_name, paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' @keywords internal
is_present_chr <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}


#' @keywords internal
eq_chr <- function(x, value) {
  toupper(trimws(as.character(x))) == toupper(trimws(as.character(value)))
}


#' @keywords internal
hy_col_or <- function(df, col, default) {
  if (col %in% names(df)) {
    return(df[[col]])
  }
  if (length(default) == nrow(df)) {
    return(default)
  }
  rep(default, nrow(df))
}


#' @keywords internal
hy_to_date <- function(x, origin = "1960-01-01") {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(as.Date(x, origin = origin))
  }
  suppressWarnings(as.Date(substr(as.character(x), 1L, 10L)))
}


#' Default eDISH quadrant labels
#'
#' Corner / table labels follow the hepatocellular screening plot (peak
#' maxima). The upper-left quadrant is named Cholestasis, matching FDA ST&F
#' / CDISC ADDILI slide language.
#'
#' @return A named character vector of quadrant codes and display labels.
#' @export
default_edish_quadrants <- function() {
  c(
    hyslaw = "Hy's Law",
    temple = "Temple's corollary",
    isolated_bili = "Cholestasis",
    normal = "Normal or near normal"
  )
}


#' Default cholestatic screening (ALP vs bilirubin) quadrant labels
#'
#' @return A named character vector of quadrant codes and display labels.
#' @export
default_cdish_quadrants <- function() {
  c(
    both = "ALP and BILI elevated",
    isolated_alp = "Isolated ALP elevation",
    isolated_bili = "Isolated hyperbilirubinemia",
    normal = "Normal or near normal"
  )
}


#' @keywords internal
meets_cut <- function(x, cut, op = ">=") {
  if (identical(op, ">")) {
    !is.na(x) & x > cut
  } else {
    !is.na(x) & x >= cut
  }
}


#' @keywords internal
cut_op_label <- function(op = ">=") {
  if (identical(op, ">")) ">" else ">="
}


#' @keywords internal
lab_is_baseline <- function(labs) {
  if ("ABLFL" %in% names(labs)) {
    return(eq_chr(labs$ABLFL, "Y"))
  }
  if ("ADY" %in% names(labs)) {
    ady <- as.numeric(labs$ADY)
    return(!is.na(ady) & ady <= 0)
  }
  rep(FALSE, nrow(labs))
}


#' @keywords internal
lab_is_post <- function(labs) {
  if ("ONTRTFL" %in% names(labs)) {
    return(eq_chr(labs$ONTRTFL, "Y"))
  }
  if ("ADY" %in% names(labs)) {
    ady <- as.numeric(labs$ADY)
    return(!is.na(ady) & ady > 0)
  }
  !lab_is_baseline(labs)
}


#' @keywords internal
edish_quadrant_code <- function(peak_altast_uln,
                                peak_bili_uln,
                                alt_cut = 3,
                                bili_cut = 2,
                                op = ">=") {
  altast_hi <- meets_cut(peak_altast_uln, alt_cut, op)
  bili_hi <- meets_cut(peak_bili_uln, bili_cut, op)
  dplyr::case_when(
    altast_hi & bili_hi ~ "hyslaw",
    altast_hi & !bili_hi ~ "temple",
    !altast_hi & bili_hi ~ "isolated_bili",
    TRUE ~ "normal"
  )
}


#' @keywords internal
cdish_quadrant_code <- function(peak_alp_uln,
                                peak_bili_uln,
                                alp_cut = 2,
                                bili_cut = 2,
                                op = ">=") {
  alp_hi <- meets_cut(peak_alp_uln, alp_cut, op)
  bili_hi <- meets_cut(peak_bili_uln, bili_cut, op)
  dplyr::case_when(
    alp_hi & bili_hi ~ "both",
    alp_hi & !bili_hi ~ "isolated_alp",
    !alp_hi & bili_hi ~ "isolated_bili",
    TRUE ~ "normal"
  )
}


#' @keywords internal
r_ratio_pattern <- function(r_ratio) {
  dplyr::case_when(
    is.na(r_ratio) ~ NA_character_,
    r_ratio >= 5 ~ "Hepatocellular",
    r_ratio > 2 ~ "Mixed",
    TRUE ~ "Cholestatic"
  )
}


#' @keywords internal
factor_arm <- function(x, arms) {
  factor(as.character(x), levels = arms)
}


#' @keywords internal
format_count_pct <- function(n, n_col) {
  pct <- if (!is.na(n_col) && n_col > 0) 100 * n / n_col else NA_real_
  rtables::rcell(c(as.integer(n), pct), format = "xx (xx.x)")
}


#' @keywords internal
first_present <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0L) {
    return(NULL)
  }
  hit[[1L]]
}


#' Coerce treatment and population selectors to single-column ADSL picks.
#'
#' Character vectors are deprecated and become
#' `teal.picks::variables(choices, selected, multiple = FALSE)`.
#' Names on those vectors are dropped so [teal.picks::picks_ui()] can show
#' column labels from the data. `group_vars` replaces `arm_var` when both
#' are supplied. `population_choices` replaces `population_var`; a length-1
#' `population_var` string is then the selected flag.
#'
#' @param arm_var (`variables`, `picks`, or `character`) treatment column.
#' @param group_vars (`NULL` or `character`) deprecated treatment choices.
#' @param population_var (`variables`, `picks`, or `character`) population flag.
#' @param population_choices (`NULL` or `character`) deprecated flag choices.
#' @param adsl_name (`character(1)`) ADSL name in `teal_data`.
#' @param fn_name (`character(1)`) exported function name used in the warning.
#' @param user_env Calling environment for [lifecycle::deprecate_warn()].
#'
#' @return A list with `arm_var` and `population_var` [`teal.picks::picks()`] objects.
#' @keywords internal
#' @noRd
migrate_hyslaw_selectors <- function(arm_var,
                                     group_vars,
                                     population_var,
                                     population_choices,
                                     adsl_name,
                                     fn_name,
                                     user_env = rlang::caller_env()) {
  checkmate::assert_string(adsl_name)
  checkmate::assert_string(fn_name)

  bind_adsl <- function(variables) {
    teal.picks::picks(
      teal.picks::datasets(choices = adsl_name, selected = adsl_name),
      variables
    )
  }

  assert_single <- function(variables, arg_name) {
    if (isTRUE(attr(variables, "multiple", exact = TRUE))) {
      stop(
        sprintf("`%s` must set `multiple = FALSE`.", arg_name),
        call. = FALSE
      )
    }
    invisible(variables)
  }

  as_picks <- function(x, arg_name) {
    if (inherits(x, "picks")) {
      if (is.null(x$variables)) {
        stop(sprintf("`%s` must include `teal.picks::variables()`.", arg_name), call. = FALSE)
      }
      assert_single(x$variables, arg_name)
      if (!is.null(x$datasets)) {
        return(x)
      }
      return(bind_adsl(x$variables))
    }
    if (!inherits(x, "variables")) {
      stop(
        sprintf(
          "`%s` must be a `teal.picks::variables()` object or a character vector of column names.",
          arg_name
        ),
        call. = FALSE
      )
    }
    assert_single(x, arg_name)
    bind_adsl(x)
  }

  from_character <- function(x, arg_name, what, with = NULL, selected = NULL) {
    checkmate::assert_character(
      x,
      min.len = 1L,
      any.missing = FALSE,
      min.chars = 1L,
      .var.name = arg_name
    )
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = what,
      with = with,
      details = paste(
        "Pass `teal.picks::variables(choices, selected, multiple = FALSE)`.",
        "Names on the character vector are ignored.",
        "The selector shows column labels from the data."
      ),
      user_env = user_env
    )
    choices <- unique(unname(x))
    if (is.null(selected)) {
      selected <- choices[[1L]]
    } else {
      selected <- unname(selected)[[1L]]
    }
    if (!selected %in% choices) {
      choices <- unique(c(selected, choices))
    }
    as_picks(
      teal.picks::variables(choices = choices, selected = selected, multiple = FALSE),
      arg_name
    )
  }

  if (!is.null(group_vars)) {
    arm_picks <- if (is.character(group_vars)) {
      from_character(
        group_vars,
        arg_name = "group_vars",
        what = sprintf("%s(group_vars =)", fn_name),
        with = sprintf("%s(arm_var =)", fn_name)
      )
    } else {
      as_picks(group_vars, "group_vars")
    }
  } else if (is.character(arm_var)) {
    arm_picks <- from_character(
      arm_var,
      arg_name = "arm_var",
      what = sprintf("%s(arm_var =)", fn_name)
    )
  } else {
    arm_picks <- as_picks(arm_var, "arm_var")
  }

  pop_selected <- NULL
  if (!is.null(population_choices)) {
    pop_source <- population_choices
    pop_name <- "population_choices"
    pop_what <- sprintf("%s(population_choices =)", fn_name)
    pop_with <- sprintf("%s(population_var =)", fn_name)
    if (is.character(population_var) && length(population_var) == 1L) {
      pop_selected <- unname(population_var)
    }
  } else if (is.character(population_var) && length(population_var) == 1L) {
    pop_selected <- unname(population_var)
    pop_source <- unique(c(pop_selected, "SAFFL", "ITTFL"))
    pop_name <- "population_var"
    pop_what <- sprintf("%s(population_var =)", fn_name)
    pop_with <- NULL
  } else {
    pop_source <- population_var
    pop_name <- "population_var"
    pop_what <- NULL
    pop_with <- NULL
  }

  pop_picks <- if (is.character(pop_source)) {
    from_character(
      pop_source,
      arg_name = pop_name,
      what = pop_what,
      with = pop_with,
      selected = pop_selected
    )
  } else {
    as_picks(pop_source, pop_name)
  }

  list(arm_var = arm_picks, population_var = pop_picks)
}


#' First selected column name from a resolved `picks` object.
#'
#' @param resolved Resolved picks from [teal.picks::picks_srv()].
#' @return `character(1)` or `character(0)`.
#' @keywords internal
#' @noRd
selected_column_name <- function(resolved) {
  selected <- resolved$variables$selected
  if (length(selected) < 1L) {
    return(character())
  }
  as.character(unname(selected[[1L]]))
}
