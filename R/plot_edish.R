#' Liver-injury screening scatter (eDISH / mDISH / cholestatic)
#'
#' Default hepatocellular eDISH is one log-log panel colored by treatment so
#' arm imbalance is visible. Quadrant names are annotated in the corners.
#' Open red circles mark biochemical potential Hy's Law (bilirubin in the
#' concurrent window after ALT/AST elevation, with optional ALP exclusion).
#' Axis maxima are not required to occur on the same day.
#'
#' Use [plot_edish_interactive()] for an interactive `plotly` version with
#' subject-level hover details (peak dates and directed peak gaps).
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
  checkmate::assert_flag(annotate_quadrants)
  checkmate::assert_flag(mark_potential)

  prep <- prep_edish_plot_data(
    adsl = adsl,
    adlb = adlb,
    classified = classified,
    which = which,
    color_by = color_by,
    log_axes = log_axes,
    facet = facet,
    title = title,
    ...
  )
  subjects <- prep$subjects
  axes <- prep$axes
  x_cut <- axes$x_cut
  y_cut <- axes$y_cut

  p <- ggplot2::ggplot(
    subjects,
    ggplot2::aes(x = .data$x_plot, y = .data$y_plot, color = .data[[prep$color_var]])
  ) +
    ggplot2::geom_point(size = 2.4, alpha = 0.85) +
    ggplot2::geom_vline(xintercept = x_cut, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = y_cut, linetype = "dashed") +
    ggplot2::labs(
      title = prep$title,
      x = axes$x_lab,
      y = axes$y_lab,
      color = prep$color_lab,
      caption = plot_edish_caption(prep$which, prep$classified, axes, mark_potential)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 10,
        colour = "grey30",
        lineheight = 1.25,
        margin = ggplot2::margin(t = 10)
      ),
      plot.caption.position = "plot"
    )

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
    p <- p + quadrant_label_layer(subjects, x_cut, y_cut, axes$corner_labels, prep$log_axes)
  }

  if (isTRUE(prep$do_facet)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(prep$group_var)))
  }

  if (isTRUE(prep$log_axes)) {
    p <- p +
      ggplot2::scale_x_log10(
        breaks = edish_log10_breaks,
        labels = edish_log10_labels,
        limits = edish_log10_limits
      ) +
      ggplot2::scale_y_log10(
        breaks = edish_log10_breaks,
        labels = edish_log10_labels,
        limits = edish_log10_limits
      )
  }
  p
}


#' Interactive liver-injury screening scatter (eDISH / mDISH / cholestatic)
#'
#' Builds an interactive `plotly` scatter plot sharing the same classification
#' and axis rules as [plot_edish()], with subject-level hover tooltips showing
#' peak values, peak dates, directed peak gaps (`BILI - ALT/AST` or
#' `BILI - ALP`), concurrent status, ALP window/missing flags, and R-ratio.
#'
#' Red open circles for biochemical potential / concurrent cases skip hover so
#' each subject produces a single tooltip. Quadrant corner labels are placed as
#' non-interactive layout annotations.
#'
#' @inheritParams plot_edish
#' @param show_caption If `TRUE` (default), render the screening note as a
#'   bottom plot annotation with extra bottom margin. Set to `FALSE` when the
#'   caller renders the caption outside the widget (as in [tm_g_edish()]).
#'
#' @return A `plotly` (`htmlwidget`) object.
#'
#' @examples
#' data <- make_synthetic_hyslaw_data(n_extra = 0L, seed = 1L)
#' plot_edish_interactive(data$ADSL, data$ADLB)
#' plot_edish_interactive(data$ADSL, data$ADLB, which = "cholestatic")
#'
#' @export
plot_edish_interactive <- function(adsl = NULL,
                                   adlb = NULL,
                                   classified = NULL,
                                   which = c("edish", "mdish", "cholestatic"),
                                   color_by = c("arm", "quadrant"),
                                   log_axes = TRUE,
                                   facet = FALSE,
                                   annotate_quadrants = TRUE,
                                   mark_potential = TRUE,
                                   title = NULL,
                                   show_caption = TRUE,
                                   ...) {
  checkmate::assert_flag(annotate_quadrants)
  checkmate::assert_flag(mark_potential)
  checkmate::assert_flag(show_caption)

  prep <- prep_edish_plot_data(
    adsl = adsl,
    adlb = adlb,
    classified = classified,
    which = which,
    color_by = color_by,
    log_axes = log_axes,
    facet = facet,
    title = title,
    ...
  )
  subjects <- prep$subjects
  axes <- prep$axes
  subjects$hover_text <- edish_hover_text(subjects, prep$which, prep$classified)

  x_cfg <- plotly_axis_config(subjects$x_plot, axes$x_cut, prep$log_axes, axes$x_lab)
  y_cfg <- plotly_axis_config(subjects$y_plot, axes$y_cut, prep$log_axes, axes$y_lab)
  palette <- edish_color_palette(subjects[[prep$color_var]])

  caption_text <- if (isTRUE(show_caption)) {
    plot_edish_caption(prep$which, prep$classified, axes, mark_potential)
  } else {
    NULL
  }

  if (!isTRUE(prep$do_facet)) {
    p <- build_plotly_panel(
      panel_df = subjects,
      color_var = prep$color_var,
      mark_col = axes$mark_col,
      mark_potential = mark_potential,
      palette = palette,
      legend_seen = character()
    )$plot

    shapes <- plotly_threshold_shapes(axes$x_cut, axes$y_cut, x_ref = "x", y_ref = "y")
    annots <- if (isTRUE(annotate_quadrants)) {
      plotly_quadrant_annotations(axes$corner_labels, x_ref = "x domain", y_ref = "y domain")
    } else {
      list()
    }
    if (!is.null(caption_text)) {
      annots <- c(annots, list(plotly_caption_annotation(caption_text, y_pos = -0.18)))
    }

    return(
      plotly::layout(
        p,
        title = list(text = escape_html(prep$title), x = 0, xanchor = "left", font = list(size = 15)),
        xaxis = x_cfg,
        yaxis = y_cfg,
        legend = list(title = list(text = escape_html(prep$color_lab))),
        shapes = shapes,
        annotations = annots,
        hovermode = "closest",
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        margin = list(t = 50, r = 20, l = 65, b = if (isTRUE(show_caption)) 150 else 55)
      )
    )
  }

  groups <- levels(subjects[[prep$group_var]])
  groups <- groups[groups %in% as.character(subjects[[prep$group_var]])]
  n_panels <- length(groups)
  ncols <- min(n_panels, 2L)
  nrows <- as.integer(ceiling(n_panels / ncols))

  panels <- vector("list", n_panels)
  legend_seen <- character()
  shapes <- list()
  annots <- list()

  x_panel_cfg <- x_cfg
  x_panel_cfg$title <- NULL
  y_panel_cfg <- y_cfg
  y_panel_cfg$title <- NULL

  for (i in seq_len(n_panels)) {
    grp <- groups[[i]]
    sub_df <- subjects[as.character(subjects[[prep$group_var]]) == grp, , drop = FALSE]
    res <- build_plotly_panel(
      panel_df = sub_df,
      color_var = prep$color_var,
      mark_col = axes$mark_col,
      mark_potential = mark_potential,
      palette = palette,
      legend_seen = legend_seen
    )
    legend_seen <- res$legend_seen
    panels[[i]] <- plotly::layout(res$plot, xaxis = x_panel_cfg, yaxis = y_panel_cfg)

    x_ref <- if (i == 1L) "x" else paste0("x", i)
    y_ref <- if (i == 1L) "y" else paste0("y", i)
    shapes <- c(shapes, plotly_threshold_shapes(axes$x_cut, axes$y_cut, x_ref = x_ref, y_ref = y_ref))
    if (isTRUE(annotate_quadrants)) {
      annots <- c(
        annots,
        plotly_quadrant_annotations(
          axes$corner_labels,
          x_ref = paste0(x_ref, " domain"),
          y_ref = paste0(y_ref, " domain")
        )
      )
    }
    annots <- c(
      annots,
      list(list(
        text = paste0("<b>", escape_html(grp), "</b>"),
        x = 0.5,
        y = 1.02,
        xref = paste0(x_ref, " domain"),
        yref = paste0(y_ref, " domain"),
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 12, color = "#222222")
      ))
    )
  }

  sp <- plotly::subplot(
    panels,
    nrows = nrows,
    shareX = TRUE,
    shareY = TRUE,
    titleX = FALSE,
    titleY = FALSE,
    margin = 0.06
  )

  annots <- c(
    annots,
    list(
      list(
        text = escape_html(axes$x_lab),
        x = 0.5,
        y = -0.10,
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "top",
        showarrow = FALSE,
        font = list(size = 13, color = "#222222")
      ),
      list(
        text = escape_html(axes$y_lab),
        x = -0.07,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "middle",
        textangle = -90,
        showarrow = FALSE,
        font = list(size = 13, color = "#222222")
      )
    )
  )
  if (!is.null(caption_text)) {
    annots <- c(annots, list(plotly_caption_annotation(caption_text, y_pos = -0.20)))
  }

  plotly::layout(
    sp,
    title = list(text = escape_html(prep$title), x = 0, xanchor = "left", font = list(size = 15)),
    legend = list(title = list(text = escape_html(prep$color_lab))),
    shapes = shapes,
    annotations = annots,
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(t = 60, r = 20, l = 75, b = if (isTRUE(show_caption)) 165 else 65)
  )
}


#' @keywords internal
prep_edish_plot_data <- function(adsl = NULL,
                                 adlb = NULL,
                                 classified = NULL,
                                 which = c("edish", "mdish", "cholestatic"),
                                 color_by = c("arm", "quadrant"),
                                 log_axes = TRUE,
                                 facet = FALSE,
                                 title = NULL,
                                 ...) {
  which <- match.arg(which)
  color_by <- match.arg(color_by)
  checkmate::assert_flag(log_axes)
  checkmate::assert_flag(facet)
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
  if (is.null(title) || !nzchar(title)) {
    title <- axes$title
  }

  list(
    classified = classified,
    subjects = subjects,
    axes = axes,
    which = which,
    color_by = color_by,
    group_var = group_var,
    color_var = color_var,
    color_lab = color_lab,
    do_facet = do_facet,
    title = title,
    log_axes = log_axes
  )
}


#' @keywords internal
escape_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}


#' @keywords internal
format_peak_date <- function(adt, ady) {
  if (!is.na(adt)) {
    return(format(as.Date(adt), "%Y-%m-%d"))
  }
  if (!is.na(ady)) {
    return(sprintf("Day %g", as.numeric(ady)))
  }
  "Missing"
}


#' @keywords internal
format_directed_gap <- function(bili_adt, x_adt, bili_ady, x_ady) {
  gap <- NA_real_
  if (!is.na(bili_adt) && !is.na(x_adt)) {
    gap <- as.numeric(as.Date(bili_adt) - as.Date(x_adt))
  } else if (!is.na(bili_ady) && !is.na(x_ady)) {
    gap <- as.numeric(bili_ady) - as.numeric(x_ady)
  }
  if (is.na(gap)) {
    return("Missing")
  }
  if (gap > 0) {
    return(sprintf("+%g d", gap))
  }
  if (gap < 0) {
    return(sprintf("%g d", gap))
  }
  "0 d"
}


#' @keywords internal
resolve_x_peak_meta <- function(subjects, which) {
  n <- nrow(subjects)
  if (identical(which, "cholestatic")) {
    return(list(
      param = rep("ALP", n),
      adt = hy_col_or(subjects, "ALP_ADT", as.Date(rep(NA, n))),
      ady = hy_col_or(subjects, "ALP_ADY", rep(NA_real_, n))
    ))
  }
  alt <- hy_col_or(subjects, "peak_alt_uln", rep(NA_real_, n))
  ast <- hy_col_or(subjects, "peak_ast_uln", rep(NA_real_, n))
  alt_adt <- hy_col_or(subjects, "ALT_ADT", as.Date(rep(NA, n)))
  ast_adt <- hy_col_or(subjects, "AST_ADT", as.Date(rep(NA, n)))
  alt_ady <- hy_col_or(subjects, "ALT_ADY", rep(NA_real_, n))
  ast_ady <- hy_col_or(subjects, "AST_ADY", rep(NA_real_, n))

  param <- rep(NA_character_, n)
  adt <- as.Date(rep(NA, n))
  ady <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    a <- alt[[i]]
    s <- ast[[i]]
    if (!is.na(a) && (is.na(s) || a > s)) {
      param[[i]] <- "ALT"
      adt[[i]] <- alt_adt[[i]]
      ady[[i]] <- alt_ady[[i]]
    } else if (!is.na(s) && (is.na(a) || s > a)) {
      param[[i]] <- "AST"
      adt[[i]] <- ast_adt[[i]]
      ady[[i]] <- ast_ady[[i]]
    } else if (!is.na(a) && !is.na(s) && a == s) {
      d_alt <- alt_adt[[i]]
      d_ast <- ast_adt[[i]]
      y_alt <- alt_ady[[i]]
      y_ast <- ast_ady[[i]]
      if (!is.na(d_alt) && !is.na(d_ast)) {
        if (d_alt < d_ast) {
          param[[i]] <- "ALT (tied with AST)"
          adt[[i]] <- d_alt
          ady[[i]] <- y_alt
        } else if (d_ast < d_alt) {
          param[[i]] <- "AST (tied with ALT)"
          adt[[i]] <- d_ast
          ady[[i]] <- y_ast
        } else {
          param[[i]] <- "ALT/AST (tie)"
          adt[[i]] <- d_alt
          ady[[i]] <- if (!is.na(y_alt)) y_alt else y_ast
        }
      } else if (!is.na(d_alt)) {
        param[[i]] <- "ALT (tied with AST)"
        adt[[i]] <- d_alt
        ady[[i]] <- y_alt
      } else if (!is.na(d_ast)) {
        param[[i]] <- "AST (tied with ALT)"
        adt[[i]] <- d_ast
        ady[[i]] <- y_ast
      } else if (!is.na(y_alt) && !is.na(y_ast)) {
        if (y_alt < y_ast) {
          param[[i]] <- "ALT (tied with AST)"
          ady[[i]] <- y_alt
        } else if (y_ast < y_alt) {
          param[[i]] <- "AST (tied with ALT)"
          ady[[i]] <- y_ast
        } else {
          param[[i]] <- "ALT/AST (tie)"
          ady[[i]] <- y_alt
        }
      } else if (!is.na(y_alt)) {
        param[[i]] <- "ALT (tied with AST)"
        ady[[i]] <- y_alt
      } else if (!is.na(y_ast)) {
        param[[i]] <- "AST (tied with ALT)"
        ady[[i]] <- y_ast
      } else {
        param[[i]] <- "ALT/AST (tie)"
      }
    }
  }
  list(param = param, adt = adt, ady = ady)
}


#' @keywords internal
edish_hover_text <- function(subjects, which, classified) {
  n <- nrow(subjects)
  if (n == 0L) {
    return(character())
  }

  axes <- plot_axis_spec(which, classified)
  subject_var <- classified$subject_var %||% "USUBJID"
  arm_var <- classified$arm_var %||% "TRT01A"
  exclude_alp <- isTRUE(classified$exclude_alp)

  subj_vals <- hy_col_or(subjects, subject_var, rep(NA_character_, n))
  arm_vals <- hy_col_or(subjects, arm_var, rep(NA_character_, n))
  quad_col <- if (identical(which, "cholestatic")) "cdish_quadrant_label" else "edish_quadrant_label"
  quad_vals <- hy_col_or(subjects, quad_col, rep(NA_character_, n))

  x_raw <- hy_col_or(subjects, axes$x_col, rep(NA_real_, n))
  y_raw <- hy_col_or(subjects, axes$y_col, rep(NA_real_, n))
  x_meta <- resolve_x_peak_meta(subjects, which)
  bili_adt <- hy_col_or(subjects, "BILI_ADT", as.Date(rep(NA, n)))
  bili_ady <- hy_col_or(subjects, "BILI_ADY", rep(NA_real_, n))

  conc_hl <- isTRUE_na_false(hy_col_or(subjects, "concurrent_hyslaw", rep(FALSE, n)))
  pot_hl <- isTRUE_na_false(hy_col_or(subjects, "potential_hyslaw", rep(FALSE, n)))
  conc_chol <- isTRUE_na_false(hy_col_or(subjects, "concurrent_cholestatic", rep(FALSE, n)))
  alp_elev <- isTRUE_na_false(hy_col_or(subjects, "alp_elevated_in_window", rep(FALSE, n)))
  alp_miss <- isTRUE_na_false(hy_col_or(subjects, "alp_missing", rep(FALSE, n)))
  r_vals <- hy_col_or(subjects, "r_ratio", rep(NA_real_, n))
  pat_vals <- hy_col_or(subjects, "injury_pattern", rep(NA_character_, n))

  unit_lab <- if (identical(which, "mdish")) "x BL" else "x ULN"
  x_name <- if (identical(which, "cholestatic")) "Peak ALP" else "Peak ALT/AST"
  gap_label <- if (identical(which, "cholestatic")) "BILI - ALP" else "BILI - ALT/AST"
  gap_note <- if (identical(which, "cholestatic")) {
    "(positive = BILI after ALP)"
  } else {
    "(positive = BILI after ALT/AST)"
  }
  date_prefix <- if (identical(which, "mdish")) "on-trt peak " else ""

  out <- character(n)
  for (i in seq_len(n)) {
    subj_chr <- if (is_present_chr(subj_vals[[i]])) escape_html(subj_vals[[i]]) else "Missing"
    arm_chr <- if (is_present_chr(arm_vals[[i]])) escape_html(arm_vals[[i]]) else "Missing"
    quad_chr <- if (is_present_chr(quad_vals[[i]])) escape_html(quad_vals[[i]]) else "Missing"

    x_val_chr <- if (is.na(x_raw[[i]])) "Missing" else sprintf("%.2f%s", x_raw[[i]], unit_lab)
    y_val_chr <- if (is.na(y_raw[[i]])) "Missing" else sprintf("%.2f%s", y_raw[[i]], unit_lab)
    x_date_chr <- format_peak_date(x_meta$adt[[i]], x_meta$ady[[i]])
    y_date_chr <- format_peak_date(bili_adt[[i]], bili_ady[[i]])

    x_detail <- if (identical(which, "cholestatic")) {
      sprintf("%s%s", date_prefix, x_date_chr)
    } else {
      p_chr <- if (is_present_chr(x_meta$param[[i]])) x_meta$param[[i]] else "Missing"
      sprintf("%s, %s%s", p_chr, date_prefix, x_date_chr)
    }

    gap_chr <- format_directed_gap(bili_adt[[i]], x_meta$adt[[i]], bili_ady[[i]], x_meta$ady[[i]])
    gap_line <- if (identical(gap_chr, "Missing")) {
      sprintf("Peak gap (%s): Missing", gap_label)
    } else {
      sprintf("Peak gap (%s): %s %s", gap_label, gap_chr, gap_note)
    }

    lines <- c(
      sprintf("<b>Subject:</b> %s | <b>Group:</b> %s", subj_chr, arm_chr),
      sprintf("Quadrant: %s", quad_chr),
      sprintf("%s: %s (%s)", x_name, x_val_chr, x_detail),
      sprintf("Peak BILI: %s (%s%s)", y_val_chr, date_prefix, y_date_chr),
      gap_line
    )

    if (identical(which, "cholestatic")) {
      lines <- c(
        lines,
        sprintf("Concurrent cholestatic: %s", if (conc_chol[[i]]) "Yes" else "No")
      )
    } else {
      hl_status <- if (pot_hl[[i]]) {
        "Yes"
      } else if (conc_hl[[i]] && exclude_alp && alp_elev[[i]]) {
        "No (ALP excluded)"
      } else {
        "No"
      }
      lines <- c(lines, sprintf("Potential Hy's Law: %s", hl_status))
      if (conc_hl[[i]] || alp_miss[[i]]) {
        alp_chr <- if (alp_miss[[i]]) {
          "Missing"
        } else if (alp_elev[[i]]) {
          "Elevated"
        } else {
          "Not elevated"
        }
        lines <- c(lines, sprintf("ALP in window: %s", alp_chr))
      }
    }

    r_chr <- if (is.na(r_vals[[i]])) {
      "Missing"
    } else if (is_present_chr(pat_vals[[i]])) {
      sprintf("%.2f (%s)", r_vals[[i]], escape_html(pat_vals[[i]]))
    } else {
      sprintf("%.2f", r_vals[[i]])
    }
    lines <- c(lines, sprintf("R-ratio: %s", r_chr))

    out[[i]] <- paste(lines, collapse = "<br>")
  }
  out
}


#' @keywords internal
edish_color_palette <- function(x) {
  lvls <- if (is.factor(x)) levels(x) else unique(as.character(x[!is.na(x)]))
  lvls <- lvls[is_present_chr(lvls)]
  base_cols <- c(
    "#F8766D", "#00BFC4", "#7CAE00", "#C77CFF",
    "#E69F00", "#56B4E9", "#009E73", "#CC79A7"
  )
  cols <- rep_len(base_cols, length(lvls))
  stats::setNames(cols, lvls)
}


#' @keywords internal
edish_axis_pad_values <- function(values, cut) {
  c(values, cut * 0.32, cut * 3.2)
}


#' @keywords internal
plotly_axis_config <- function(values, cut, log_axes, title) {
  vals <- edish_axis_pad_values(values, cut)
  if (isTRUE(log_axes)) {
    breaks <- edish_log10_breaks(range(vals, na.rm = TRUE))
    return(list(
      title = list(text = escape_html(title)),
      type = "log",
      range = log10(range(breaks)),
      tickmode = "array",
      tickvals = breaks,
      ticktext = edish_log10_labels(breaks),
      gridcolor = "#EBEBEB",
      linecolor = "#CCCCCC",
      mirror = TRUE,
      zeroline = FALSE
    ))
  }
  lim <- range(vals, na.rm = TRUE)
  pad <- diff(lim) * 0.05
  if (!is.finite(pad) || pad == 0) {
    pad <- 0.5
  }
  list(
    title = list(text = escape_html(title)),
    range = c(lim[1L] - pad, lim[2L] + pad),
    gridcolor = "#EBEBEB",
    linecolor = "#CCCCCC",
    mirror = TRUE,
    zeroline = FALSE
  )
}


#' @keywords internal
build_plotly_panel <- function(panel_df,
                               color_var,
                               mark_col,
                               mark_potential,
                               palette,
                               legend_seen) {
  p <- plotly::plot_ly()
  lvls <- names(palette)
  present <- as.character(panel_df[[color_var]])

  for (lvl in lvls) {
    idx <- which(!is.na(present) & present == lvl)
    if (length(idx) == 0L) {
      next
    }
    sub <- panel_df[idx, , drop = FALSE]
    show_leg <- !lvl %in% legend_seen
    if (show_leg) {
      legend_seen <- c(legend_seen, lvl)
    }
    p <- plotly::add_trace(
      p,
      data = sub,
      x = ~x_plot,
      y = ~y_plot,
      type = "scatter",
      mode = "markers",
      name = lvl,
      legendgroup = lvl,
      showlegend = show_leg,
      text = ~hover_text,
      hoverinfo = "text",
      marker = list(
        size = 8,
        opacity = 0.85,
        color = unname(palette[[lvl]])
      )
    )
  }

  if (isTRUE(mark_potential)) {
    mark <- panel_df[isTRUE_na_false(panel_df[[mark_col]]), , drop = FALSE]
    if (nrow(mark) > 0L) {
      p <- plotly::add_trace(
        p,
        data = mark,
        x = ~x_plot,
        y = ~y_plot,
        type = "scatter",
        mode = "markers",
        showlegend = FALSE,
        hoverinfo = "skip",
        marker = list(
          symbol = "circle-open",
          size = 14,
          color = "red",
          line = list(width = 2, color = "red")
        )
      )
    }
  }

  list(plot = p, legend_seen = legend_seen)
}


#' @keywords internal
plotly_threshold_shapes <- function(x_cut, y_cut, x_ref = "x", y_ref = "y") {
  list(
    list(
      type = "line",
      x0 = x_cut,
      x1 = x_cut,
      y0 = 0,
      y1 = 1,
      xref = x_ref,
      yref = paste0(y_ref, " domain"),
      line = list(dash = "dash", color = "#333333", width = 1.2)
    ),
    list(
      type = "line",
      x0 = 0,
      x1 = 1,
      y0 = y_cut,
      y1 = y_cut,
      xref = paste0(x_ref, " domain"),
      yref = y_ref,
      line = list(dash = "dash", color = "#333333", width = 1.2)
    )
  )
}


#' @keywords internal
plotly_quadrant_annotations <- function(labels, x_ref = "x domain", y_ref = "y domain") {
  xs <- c(0.02, 0.98, 0.02, 0.98)
  ys <- c(0.97, 0.97, 0.03, 0.03)
  xanchors <- c("left", "right", "left", "right")
  yanchors <- c("top", "top", "bottom", "bottom")
  lapply(seq_len(4L), function(i) {
    list(
      text = paste0("<i>", escape_html(labels[[i]]), "</i>"),
      x = xs[[i]],
      y = ys[[i]],
      xref = x_ref,
      yref = y_ref,
      xanchor = xanchors[[i]],
      yanchor = yanchors[[i]],
      showarrow = FALSE,
      font = list(size = 11, color = "#595959")
    )
  })
}


#' @keywords internal
plotly_caption_annotation <- function(caption_text, y_pos = -0.18) {
  list(
    text = gsub("\n", "<br>", escape_html(caption_text), fixed = TRUE),
    x = 0,
    y = y_pos,
    xref = "paper",
    yref = "paper",
    xanchor = "left",
    yanchor = "top",
    align = "left",
    showarrow = FALSE,
    font = list(size = 11, color = "#4D4D4D")
  )
}


# Powers of ten only. Default log breaks insert 1-3 steps on a short range.
edish_log10_breaks <- function(limits) {
  10^(floor(log10(min(limits))):ceiling(log10(max(limits))))
}

edish_log10_limits <- function(limits) {
  range(edish_log10_breaks(limits))
}

edish_log10_labels <- function(breaks) {
  format(breaks, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
}


#' @keywords internal
plot_axis_spec <- function(which, classified) {
  alt_cut <- classified$alt_cut %||% 3
  bili_cut <- classified$bili_cut %||% 2
  alp_cut <- classified$alp_cut %||% 2
  qed <- default_edish_quadrants()

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
      sprintf(";\nALP %s %sx ULN in the same window excludes the case", op, classified$alp_cut %||% 2)
    } else {
      ""
    }
    sep_rule <- if (isTRUE(exclude_alp)) " " else "\n"
    mark <- sprintf(
      paste0(
        "Red open circles: biochemical potential Hy's Law (bilirubin %s %sx ULN ",
        "on or within %s days after ALT/AST %s %sx ULN%s).%sPeak Hy's Law ",
        "color/quadrant is not the same rule."
      ),
      op, classified$bili_cut %||% 2, window_days, op, classified$alt_cut %||% 3, alp_note, sep_rule
    )
  }
  if (!isTRUE(mark_potential)) {
    mark <- "Potential / concurrent cases are not circled on this plot."
  }
  paste(
    line1,
    pop,
    mark,
    "Laboratory screening only; alternative causes are not assessed.",
    sep = "\n"
  )
}


#' @keywords internal
quadrant_label_layer <- function(subjects, x_cut, y_cut, labels, log_axes = TRUE) {
  x_vals <- edish_axis_pad_values(subjects$x_plot, x_cut)
  y_vals <- edish_axis_pad_values(subjects$y_plot, y_cut)
  if (isTRUE(log_axes)) {
    x_lim <- range(edish_log10_breaks(range(x_vals, na.rm = TRUE)))
    y_lim <- range(edish_log10_breaks(range(y_vals, na.rm = TRUE)))
    dx <- (x_lim[2L] / x_lim[1L])^0.025
    dy <- (y_lim[2L] / y_lim[1L])^0.035
    x_pos <- c(x_lim[1L] * dx, x_lim[2L] / dx, x_lim[1L] * dx, x_lim[2L] / dx)
    y_pos <- c(y_lim[2L] / dy, y_lim[2L] / dy, y_lim[1L] * dy, y_lim[1L] * dy)
  } else {
    x_lim <- range(x_vals, na.rm = TRUE)
    y_lim <- range(y_vals, na.rm = TRUE)
    dx <- diff(x_lim) * 0.02
    dy <- diff(y_lim) * 0.03
    x_pos <- c(x_lim[1L] + dx, x_lim[2L] - dx, x_lim[1L] + dx, x_lim[2L] - dx)
    y_pos <- c(y_lim[2L] - dy, y_lim[2L] - dy, y_lim[1L] + dy, y_lim[1L] + dy)
  }
  labs_df <- data.frame(
    x_plot = x_pos,
    y_plot = y_pos,
    hjust = c(0, 1, 0, 1),
    vjust = c(1, 1, 0, 0),
    lab = labels,
    stringsAsFactors = FALSE
  )
  ggplot2::geom_text(
    data = labs_df,
    mapping = ggplot2::aes(
      x = .data$x_plot,
      y = .data$y_plot,
      label = .data$lab,
      hjust = .data$hjust,
      vjust = .data$vjust
    ),
    inherit.aes = FALSE,
    colour = "grey35",
    size = 3.4,
    fontface = "italic"
  )
}
