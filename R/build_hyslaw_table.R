#' Build Hy's Law / eDISH review tables
#'
#' Default output is two stacked sections: maximal treatment-emergent
#' (peak eDISH) quadrant counts, then biochemical concurrent Hy's Law in the
#' selected window. Use [build_liver_elevation_table()] for the nested
#' ALT/AST/BILI/ALP elevation shell.
#'
#' Pass either raw `adsl` / `adlb` or a list returned by [classify_hyslaw()]
#' via `classified` so the teal server can prepare data once.
#'
#' @param adsl ADSL-like data. Ignored when `classified` is supplied.
#' @param adlb ADLB-like data.
#' @param arm_var Treatment / grouping variable.
#' @param show_total Whether to add a Total column.
#' @param title Table title.
#' @param footnotes Optional footnotes.
#' @param classified Optional list from [classify_hyslaw()].
#' @param which Character vector of sections: `"quadrant"`, `"concurrent"`.
#' @param ... Passed to [classify_hyslaw()] when `classified` is `NULL`.
#'
#' @return An `rtables` `VTableTree`.
#'
#' @examples
#' data <- make_synthetic_hyslaw_data(n_extra = 0L, seed = 1L)
#' build_hyslaw_table(data$ADSL, data$ADLB)
#'
#' @export
build_hyslaw_table <- function(adsl = NULL,
                               adlb = NULL,
                               arm_var = "TRT01A",
                               show_total = TRUE,
                               title = "Hy's Law monitoring (exploratory)",
                               footnotes = NULL,
                               classified = NULL,
                               which = c("quadrant", "concurrent"),
                               ...) {
  checkmate::assert_string(arm_var)
  checkmate::assert_flag(show_total)
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_character(footnotes, null.ok = TRUE)
  checkmate::assert_list(classified, null.ok = TRUE)
  which <- unique(match.arg(which, several.ok = TRUE))

  classified <- classified %||% {
    checkmate::assert_data_frame(adsl)
    classify_hyslaw(adsl = adsl, adlb = adlb, arm_var = arm_var, ...)
  }
  pieces <- lapply(which, function(sec) {
    build_hyslaw_section(
      classified = classified,
      section = sec,
      show_total = show_total,
      arm_var = arm_var
    )
  })
  tbl <- Reduce(rbind, pieces)
  finalize_hyslaw_table(
    tbl = tbl,
    classified = classified,
    title = title,
    footnotes = footnotes
  )
}


#' Nested on-treatment liver-test elevation table
#'
#' Counts subjects whose peak on-treatment ULN ratio meets 3/5/10/20x
#' (transaminases) or 1.5/2x (bilirubin, ALP), then the combination of
#' concurrent biochemical Hy's Law split by ALP in the window.
#'
#' @inheritParams build_hyslaw_table
#' @param at_cuts Transaminase ULN multiples.
#' @param bili_cuts Bilirubin ULN multiples.
#' @param alp_cuts ALP ULN multiples.
#'
#' @return An `rtables` `VTableTree`.
#' @export
build_liver_elevation_table <- function(adsl = NULL,
                                        adlb = NULL,
                                        arm_var = "TRT01A",
                                        show_total = TRUE,
                                        title = "Maximal on-treatment liver test elevations (exploratory)",
                                        footnotes = NULL,
                                        classified = NULL,
                                        at_cuts = c(3, 5, 10, 20),
                                        bili_cuts = c(1.5, 2),
                                        alp_cuts = c(1.5, 2),
                                        ...) {
  checkmate::assert_string(arm_var)
  checkmate::assert_flag(show_total)
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_character(footnotes, null.ok = TRUE)
  checkmate::assert_numeric(at_cuts, lower = 0, finite = TRUE, min.len = 1L)
  checkmate::assert_numeric(bili_cuts, lower = 0, finite = TRUE, min.len = 1L)
  checkmate::assert_numeric(alp_cuts, lower = 0, finite = TRUE, min.len = 1L)

  classified <- classified %||% {
    checkmate::assert_data_frame(adsl)
    classify_hyslaw(adsl = adsl, adlb = adlb, arm_var = arm_var, ...)
  }
  tbl <- build_hyslaw_section(
    classified = classified,
    section = "elevation",
    show_total = show_total,
    arm_var = arm_var,
    extra_args = list(
      at_cuts = at_cuts,
      bili_cuts = bili_cuts,
      alp_cuts = alp_cuts
    )
  )
  finalize_hyslaw_table(
    tbl = tbl,
    classified = classified,
    title = title,
    footnotes = footnotes
  )
}


#' Subject listing for notable Hy's Law / eDISH cases
#'
#' @param adsl,adlb Passed to [classify_hyslaw()] when `classified` is `NULL`.
#' @param classified Optional list from [classify_hyslaw()].
#' @param which `"notable"` keeps peak Hy's Law, concurrent, ALP-excluded,
#'   or concurrent cholestatic subjects; `"all"` returns every subject.
#' @param ... Passed to [classify_hyslaw()].
#'
#' @return A data frame. Symptoms, alternative causes, and confirmed Hy's Law
#'   are not derived from ADLB; see footnotes on [build_hyslaw_table()].
#' @export
hyslaw_listing <- function(adsl = NULL,
                           adlb = NULL,
                           classified = NULL,
                           which = c("notable", "all"),
                           ...) {
  which <- match.arg(which)
  if (is.null(classified)) {
    classified <- classify_hyslaw(adsl = adsl, adlb = adlb, ...)
  }
  subjects <- classified$subjects
  if (is.null(subjects) || nrow(subjects) == 0L) {
    return(subjects)
  }
  if (identical(which, "notable")) {
    keep <- subjects$edish_quadrant == "hyslaw" |
      isTRUE_na_false(subjects$concurrent_hyslaw) |
      isTRUE_na_false(subjects$alp_elevated_in_window) |
      isTRUE_na_false(subjects$concurrent_cholestatic)
    subjects <- subjects[keep, , drop = FALSE]
  }
  cols <- intersect(
    c(
      classified$subject_var,
      classified$arm_var,
      "edish_quadrant_label",
      "cdish_quadrant_label",
      "peak_alt_uln",
      "peak_ast_uln",
      "peak_altast_uln",
      "peak_bili_uln",
      "peak_alp_uln",
      "peak_altast_bl",
      "peak_bili_bl",
      "r_ratio",
      "injury_pattern",
      "alp_missing",
      "concurrent_hyslaw",
      "alp_elevated_in_window",
      "potential_hyslaw",
      "concurrent_cholestatic",
      "concurrent_altast_adt",
      "concurrent_bili_adt",
      "concurrent_alp_adt"
    ),
    names(subjects)
  )
  subjects[, cols, drop = FALSE]
}


#' Default method footnotes for Hy's Law tables
#'
#' @param classified Optional list from [classify_hyslaw()] used to fill
#'   window, comparison operator, and ALP settings.
#' @return Character vector of footer notes.
#' @export
default_hyslaw_footnotes <- function(classified = NULL) {
  op <- cut_op_label(classified$compare %||% ">=")
  window_days <- classified$window_days %||% 14
  alt_cut <- classified$alt_cut %||% 3
  bili_cut <- classified$bili_cut %||% 2
  alp_cut <- classified$alp_cut %||% 2
  exclude_alp <- if (is.null(classified)) TRUE else isTRUE(classified$exclude_alp)
  c(
    "Exploratory review table; not a submission TFL.",
    sprintf(
      "Threshold operator is %s (FDA 2009 and many CSR shells use >; change classify_hyslaw(compare = \">\") to match).",
      op
    ),
    "Peak eDISH / maximal abnormalities use each subject's on-treatment maximum ALT or AST / ULN versus maximum bilirubin / ULN. The two maxima are not required to occur on the same day. Baseline records (ABLFL = Y) are excluded from those maxima.",
    sprintf(
      "Biochemical concurrent Hy's Law: bilirubin %s %sx ULN on or after ALT/AST %s %sx ULN, within %s days. This is not the peak-quadrant count and is not a confirmed Hy's Law case.",
      op, bili_cut, op, alt_cut, window_days
    ),
    sprintf(
      "Potential Hy's Law applies optional ALP %s %sx ULN exclusion in the same window (currently %s). Missing ALP is not imputed; subjects without ALP meeting the cut in the window are retained when exclusion is on.",
      op, alp_cut, if (isTRUE(exclude_alp)) "on" else "off"
    ),
    "R-ratio = (ALT/ULN)/(ALP/ULN) at the concurrent ALT date when both exist, otherwise peak ALT/ULN over peak ALP/ULN. Pattern: >=5 hepatocellular, >2 and <5 mixed, <=2 cholestatic.",
    "Symptoms, alternative causes, and 'satisfies Hy's Law' are not assessed from ADLB."
  )
}


#' @keywords internal
hyslaw_col_layout <- function(arm_var, show_total) {
  lyt <- rtables::basic_table()
  lyt <- rtables::split_cols_by(lyt, arm_var)
  if (isTRUE(show_total)) {
    lyt <- rtables::add_overall_col(lyt, "Total")
  }
  rtables::add_colcounts(lyt)
}


#' @keywords internal
build_hyslaw_section <- function(classified,
                                 section,
                                 show_total,
                                 arm_var,
                                 extra_args = list()) {
  if (isTRUE(classified$empty) || NROW(classified$subjects) == 0L) {
    stop("No laboratory records remain after filtering the analysis population.", call. = FALSE)
  }

  subjects <- classified$subjects
  arm_var <- classified$arm_var %||% arm_var
  alt_counts <- classified$alt_counts_df
  if (arm_var %in% names(alt_counts)) {
    arms <- levels(subjects[[arm_var]]) %||% unique(as.character(subjects[[arm_var]]))
    arms <- arms[is_present_chr(arms)]
    alt_counts <- alt_counts[is_present_chr(alt_counts[[arm_var]]), , drop = FALSE]
    alt_counts[[arm_var]] <- factor_arm(alt_counts[[arm_var]], arms)
  } else {
    alt_counts <- subjects
  }

  afun <- switch(
    section,
    quadrant = a_edish_quadrant_counts,
    concurrent = a_hyslaw_concurrent_counts,
    elevation = a_liver_elevation_counts,
    stop("Unknown table section.", call. = FALSE)
  )
  args <- c(
    list(
      classified = classified,
      op = cut_op_label(classified$compare %||% ">=")
    ),
    extra_args
  )

  lyt <- hyslaw_col_layout(arm_var, show_total)
  lyt <- rtables::analyze(
    lyt,
    vars = classified$subject_var,
    afun = afun,
    extra_args = args,
    show_labels = "hidden",
    table_names = paste0("hyslaw_", section)
  )
  rtables::build_table(
    lyt = lyt,
    df = subjects,
    alt_counts_df = alt_counts
  )
}


#' @keywords internal
finalize_hyslaw_table <- function(tbl, classified, title, footnotes) {
  op <- cut_op_label(classified$compare %||% ">=")
  if (is.null(title) || !nzchar(title)) {
    title <- "Hy's Law monitoring (exploratory)"
  }
  rtables::main_title(tbl) <- title
  rtables::subtitles(tbl) <- c(
    sprintf(
      "Peak eDISH cuts: ALT/AST %s %sx ULN, BILI %s %sx ULN (on-treatment maxima)",
      op, classified$alt_cut, op, classified$bili_cut
    ),
    sprintf(
      "Concurrent window: %s days after ALT/AST elevation; ALP exclusion %s; operator %s",
      classified$window_days,
      if (isTRUE(classified$exclude_alp)) "on" else "off",
      op
    )
  )
  notes <- footnotes %||% default_hyslaw_footnotes(classified)
  if (length(notes) > 0L) {
    rtables::main_footer(tbl) <- notes
  }
  tbl
}


#' @keywords internal
a_edish_quadrant_counts <- function(df, .N_col, classified, op, ...) {
  n_hyslaw <- sum(df$edish_quadrant == "hyslaw", na.rm = TRUE)
  n_temple <- sum(df$edish_quadrant == "temple", na.rm = TRUE)
  n_iso <- sum(df$edish_quadrant == "isolated_bili", na.rm = TRUE)
  n_norm <- sum(df$edish_quadrant == "normal", na.rm = TRUE)
  rows <- list(
    rtables::rcell(NULL),
    format_count_pct(n_hyslaw, .N_col),
    format_count_pct(n_temple, .N_col),
    format_count_pct(n_iso, .N_col),
    format_count_pct(n_norm, .N_col)
  )
  names(rows) <- c(
    "Maximal treatment-emergent liver tests (peak eDISH)",
    sprintf(
      "ALT or AST %s %sx ULN and BILI %s %sx ULN (peak Hy's Law quadrant)",
      op, classified$alt_cut, op, classified$bili_cut
    ),
    "Temple's corollary",
    "Cholestasis (isolated hyperbilirubinemia)",
    "Normal or near normal"
  )
  rtables::in_rows(.list = rows, .indent_mods = c(0L, 1L, 1L, 1L, 1L))
}


#' @keywords internal
a_hyslaw_concurrent_counts <- function(df, .N_col, classified, op, ...) {
  n_conc <- sum(isTRUE_na_false(df$concurrent_hyslaw))
  n_pot <- sum(isTRUE_na_false(df$potential_hyslaw))
  n_alp <- sum(isTRUE_na_false(df$concurrent_hyslaw) & isTRUE_na_false(df$alp_elevated_in_window))
  n_chol <- sum(isTRUE_na_false(df$concurrent_cholestatic))
  rows <- list(
    rtables::rcell(NULL),
    format_count_pct(n_conc, .N_col),
    format_count_pct(n_alp, .N_col),
    format_count_pct(n_pot, .N_col),
    format_count_pct(n_chol, .N_col)
  )
  names(rows) <- c(
    sprintf("Biochemical concurrent Hy's Law (%s-day window)", classified$window_days),
    sprintf(
      "ALT or AST %s %sx ULN and BILI %s %sx ULN in window",
      op, classified$alt_cut, op, classified$bili_cut
    ),
    sprintf("Concurrent with ALP %s %sx ULN in window", op, classified$alp_cut),
    "Potential Hy's Law after ALP exclusion",
    sprintf(
      "Cholestatic concurrent (BILI %s %sx ULN within %s days after ALP %s %sx ULN)",
      op, classified$bili_cut, classified$window_days, op, classified$alp_cut
    )
  )
  rtables::in_rows(.list = rows, .indent_mods = c(0L, 1L, 1L, 1L, 1L))
}


#' @keywords internal
a_liver_elevation_counts <- function(df,
                                     .N_col,
                                     classified,
                                     op,
                                     at_cuts,
                                     bili_cuts,
                                     alp_cuts,
                                     ...) {
  row_cut <- function(label, n, indent) {
    val <- if (is.null(n)) rtables::rcell(NULL) else format_count_pct(n, .N_col)
    list(name = label, val = val, indent = indent)
  }
  items <- list(
    row_cut("Peak on-treatment elevations", NULL, 0L)
  )

  add_cuts <- function(items, header, values, cuts, indent_header = 1L) {
    items[[length(items) + 1L]] <- row_cut(header, NULL, indent_header)
    items[[length(items)]]$val <- rtables::rcell(NULL)
    for (cut in cuts) {
      n <- sum(meets_cut(values, cut, classified$compare %||% ">="))
      items[[length(items) + 1L]] <- row_cut(
        sprintf("%s %s %sx ULN", header, op, cut),
        n,
        indent_header + 1L
      )
    }
    items
  }

  items <- add_cuts(items, "ALT", df$peak_alt_uln, at_cuts)
  items <- add_cuts(items, "AST", df$peak_ast_uln, at_cuts)
  items <- add_cuts(items, "ALT or AST", df$peak_altast_uln, at_cuts)
  items <- add_cuts(items, "Bilirubin", df$peak_bili_uln, bili_cuts)
  items <- add_cuts(items, "ALP", df$peak_alp_uln, alp_cuts)

  n_conc <- sum(isTRUE_na_false(df$concurrent_hyslaw))
  n_alp <- sum(isTRUE_na_false(df$concurrent_hyslaw) & isTRUE_na_false(df$alp_elevated_in_window))
  n_pot <- sum(isTRUE_na_false(df$potential_hyslaw))
  items[[length(items) + 1L]] <- row_cut(
    sprintf(
      "Combination: ALT or AST %s %sx ULN and BILI %s %sx ULN in %s-day window",
      op, classified$alt_cut, op, classified$bili_cut, classified$window_days
    ),
    n_conc,
    1L
  )
  items[[length(items) + 1L]] <- row_cut(
    sprintf("ALP %s %sx ULN in window", if (identical(op, ">")) "<=" else "<", classified$alp_cut),
    n_pot,
    2L
  )
  items[[length(items) + 1L]] <- row_cut(
    sprintf("ALP %s %sx ULN in window", op, classified$alp_cut),
    n_alp,
    2L
  )

  rows <- lapply(items, `[[`, "val")
  names(rows) <- vapply(items, `[[`, character(1L), "name")
  indents <- vapply(items, `[[`, integer(1L), "indent")
  rtables::in_rows(.list = rows, .indent_mods = indents)
}
