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
