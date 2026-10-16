#' Liver-injury screening scatter (eDISH / mDISH / cholestatic)
#'
#' Default hepatocellular eDISH is one log-log panel colored by treatment so
#' arm imbalance is visible. Quadrant names are annotated in the corners.
#' Open red circles mark biochemical potential Hy's Law (bilirubin in the
#' concurrent window after ALT/AST elevation, with optional ALP exclusion).
#' Axis maxima are not required to occur on the same day.
#'
#' @param adsl,adlb Passed to [classify_hyslaw()] when `classified` is `NULL`.
#' @param classified Optional list from [classify_hyslaw()].
#' @param which `"edish"` is peak ALT or AST / ULN vs peak BILI / ULN;
#'   `"mdish"` uses peak / baseline; `"cholestatic"` is peak ALP / ULN vs
#'   peak BILI / ULN.
#' @param color_by `"arm"` colors points by the grouping variable (default,
#'   for treatment imbalance). `"quadrant"` colors by screening quadrant.
#' @param log_axes If `TRUE`, both axes use log10 scales.
#' @param facet If `TRUE`, facet by the grouping variable when it has at
#'   least two levels. Default `FALSE` keeps arms on one panel.
#' @param annotate_quadrants If `TRUE`, print quadrant names in the corners.
#' @param mark_potential If `TRUE`, overlay open red circles on biochemical
#'   potential Hy's Law (eDISH / mDISH) or concurrent cholestatic cases.
#' @param title Plot title. Generated from `which` when `NULL`.
#' @param ... Passed to [classify_hyslaw()].
#'
#' @return A `ggplot` object.
#'
#' @examples
#' data <- make_synthetic_hyslaw_data(n_extra = 0L, seed = 1L)
#' plot_edish(data$ADSL, data$ADLB)
#' plot_edish(data$ADSL, data$ADLB, which = "cholestatic")
#'
#' @export
plot_edish <- function(adsl = NULL,
                       adlb = NULL,
                       classified = NULL,
                       which = c("edish", "mdish", "cholestatic"),
                       color_by = c("arm", "quadrant"),
                       log_axes = TRUE,
                       facet = FALSE,
                       annotate_quadrants = TRUE,
                       mark_potential = TRUE,
                       title = NULL,
                       ...) {
  which <- match.arg(which)
  color_by <- match.arg(color_by)
  checkmate::assert_flag(log_axes)
  checkmate::assert_flag(facet)
  checkmate::assert_flag(annotate_quadrants)
  checkmate::assert_flag(mark_potential)
  checkmate::assert_string(title, null.ok = TRUE)

  if (is.null(classified)) {
    classified <- classify_hyslaw(adsl = adsl, adlb = adlb, ...)
  }
  subjects <- classified$subjects
  if (is.null(subjects) || nrow(subjects) == 0L) {
    stop("No subjects available to plot.", call. = FALSE)
  }

  axes <- plot_axis_spec(which, classified)
  keep <- !is.na(subjects[[axes$x_col]]) & !is.na(subjects[[axes$y_col]])
  subjects <- subjects[keep, , drop = FALSE]
  if (nrow(subjects) == 0L) {
    stop(axes$empty_msg, call. = FALSE)
  }

  group_var <- classified$arm_var
  if (!is.null(group_var) && group_var %in% names(subjects)) {
    subjects[[group_var]] <- factor(as.character(subjects[[group_var]]))
  }

  subjects$x_plot <- pmax(subjects[[axes$x_col]], 0.05)
  subjects$y_plot <- pmax(subjects[[axes$y_col]], 0.05)
  subjects$edish_quadrant_label <- factor(
    subjects$edish_quadrant_label,
    levels = unname(default_edish_quadrants())
  )
  if ("cdish_quadrant_label" %in% names(subjects)) {
    subjects$cdish_quadrant_label <- factor(
      subjects$cdish_quadrant_label,
      levels = unname(default_cdish_quadrants())
    )
  }

  quad_var <- if (identical(which, "cholestatic")) "cdish_quadrant_label" else "edish_quadrant_label"
  color_var <- if (identical(color_by, "arm")) group_var else quad_var
  color_lab <- if (identical(color_by, "arm")) group_var else axes$quad_legend
  n_group <- if (!is.null(group_var) && group_var %in% names(subjects)) {
    length(unique(as.character(subjects[[group_var]][!is.na(subjects[[group_var]])])))
  } else {
    0L
  }
  do_facet <- isTRUE(facet) && n_group >= 2L && group_var %in% names(subjects)

  x_cut <- axes$x_cut
  y_cut <- axes$y_cut
  if (is.null(title) || !nzchar(title)) {
    title <- axes$title
  }

  p <- ggplot2::ggplot(
    subjects,
    ggplot2::aes(x = .data$x_plot, y = .data$y_plot, color = .data[[color_var]])
  ) +
    ggplot2::geom_point(size = 2.4, alpha = 0.85) +
    ggplot2::geom_vline(xintercept = x_cut, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = y_cut, linetype = "dashed") +
    ggplot2::labs(
      title = title,
      x = axes$x_lab,
      y = axes$y_lab,
      color = color_lab,
      caption = plot_edish_caption(which, classified, axes, mark_potential)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 8))

  if (isTRUE(mark_potential)) {
    mark <- subjects[isTRUE_na_false(subjects[[axes$mark_col]]), , drop = FALSE]
    if (nrow(mark) > 0L) {
      p <- p + ggplot2::geom_point(
        data = mark,
        mapping = ggplot2::aes(x = .data$x_plot, y = .data$y_plot),
        inherit.aes = FALSE,
        shape = 21,
        size = 4.2,
        stroke = 1.05,
        fill = NA,
        colour = "red"
      )
    }
  }

  if (isTRUE(annotate_quadrants)) {
    p <- p + quadrant_label_layer(x_cut, y_cut, axes$corner_labels)
  }

  if (isTRUE(do_facet)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(group_var)))
  }

  if (isTRUE(log_axes)) {
    p <- p +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10()
  }
  p
}


#' @keywords internal
plot_axis_spec <- function(which, classified) {
  alt_cut <- classified$alt_cut %||% 3
  bili_cut <- classified$bili_cut %||% 2
  alp_cut <- classified$alp_cut %||% 2
  qed <- default_edish_quadrants()
  qcd <- default_cdish_quadrants()

  if (identical(which, "mdish")) {
    return(list(
      x_col = "peak_altast_bl",
      y_col = "peak_bili_bl",
      x_cut = alt_cut,
      y_cut = bili_cut,
      x_lab = "Peak on-treatment ALT or AST / baseline",
      y_lab = "Peak on-treatment BILI / baseline",
      title = "mDISH (peak / baseline ALT or AST vs BILI)",
      empty_msg = "No subjects with both peak transaminase and bilirubin / baseline.",
      quad_legend = "eDISH quadrant (x ULN)",
      mark_col = "potential_hyslaw",
      corner_labels = unname(qed[c("isolated_bili", "hyslaw", "normal", "temple")])
    ))
  }
  if (identical(which, "cholestatic")) {
    return(list(
      x_col = "peak_alp_uln",
      y_col = "peak_bili_uln",
      x_cut = alp_cut,
      y_cut = bili_cut,
      x_lab = "Peak ALP / ULN",
      y_lab = "Peak BILI / ULN",
      title = "Cholestatic screening (peak ALP / ULN vs peak BILI / ULN)",
      empty_msg = "No subjects with both peak ALP and bilirubin / ULN.",
      quad_legend = "Cholestatic quadrant",
      mark_col = "concurrent_cholestatic",
      corner_labels = c("Isolated BILI", "ALP + BILI", "Normal", "Isolated ALP")
    ))
  }

  list(
    x_col = "peak_altast_uln",
    y_col = "peak_bili_uln",
    x_cut = alt_cut,
    y_cut = bili_cut,
    x_lab = "Peak ALT or AST / ULN",
    y_lab = "Peak BILI / ULN",
    title = "eDISH (peak ALT or AST / ULN vs peak BILI / ULN)",
    empty_msg = "No subjects with both peak transaminase and bilirubin / ULN.",
    quad_legend = "eDISH quadrant",
    mark_col = "potential_hyslaw",
    corner_labels = unname(qed[c("isolated_bili", "hyslaw", "normal", "temple")])
  )
}


#' @keywords internal
plot_edish_caption <- function(which, classified, axes, mark_potential) {
  op <- cut_op_label(classified$compare %||% ">=")
  window_days <- classified$window_days %||% 14
  exclude_alp <- isTRUE(classified$exclude_alp)
  scale_lab <- if (identical(which, "mdish")) "baseline" else "ULN"
  line1 <- sprintf(
    "Dashed lines: x %s %s x %s, y %s %s x %s. Axis maxima are not required to occur on the same day.",
    op, axes$x_cut, scale_lab, op, axes$y_cut, scale_lab
  )
  pop <- "On-treatment peaks (ONTRTFL = Y when present); baseline rows are used only for mDISH."
  if (identical(which, "cholestatic")) {
    mark <- sprintf(
      "Red open circles: bilirubin %s %sx ULN on or within %s days after ALP %s %sx ULN.",
      op, classified$bili_cut %||% 2, window_days, op, classified$alp_cut %||% 2
    )
  } else {
    alp_note <- if (isTRUE(exclude_alp)) {
      sprintf("; ALP %s %sx ULN in the same window excludes the case", op, classified$alp_cut %||% 2)
    } else {
      ""
    }
    mark <- sprintf(
      paste0(
        "Red open circles: biochemical potential Hy's Law (bilirubin %s %sx ULN ",
        "on or within %s days after ALT/AST %s %sx ULN%s). Peak Hy's Law ",
        "color/quadrant is not the same rule."
      ),
      op, classified$bili_cut %||% 2, window_days, op, classified$alt_cut %||% 3, alp_note
    )
  }
  if (!isTRUE(mark_potential)) {
    mark <- "Potential / concurrent cases are not circled on this plot."
  }
  paste(line1, pop, mark, "Laboratory screening only; alternative causes are not assessed.", sep = "\n")
}


#' @keywords internal
quadrant_label_layer <- function(x_cut, y_cut, labels) {
  labs_df <- data.frame(
    x_plot = c(x_cut * 0.32, x_cut * 3.2, x_cut * 0.32, x_cut * 3.2),
    y_plot = c(y_cut * 3.2, y_cut * 3.2, y_cut * 0.32, y_cut * 0.32),
    lab = labels,
    stringsAsFactors = FALSE
  )
  ggplot2::geom_text(
    data = labs_df,
    mapping = ggplot2::aes(x = .data$x_plot, y = .data$y_plot, label = .data$lab),
    inherit.aes = FALSE,
    colour = "grey30",
    size = 3.1,
    fontface = "italic"
  )
}
