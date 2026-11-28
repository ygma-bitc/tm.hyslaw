test_that("plot_edish_interactive() builds plotly hover plots for eDISH, mDISH, and cholestatic", {
  data <- synthetic_hyslaw_fixture()

  for (w in c("edish", "mdish", "cholestatic")) {
    p <- plot_edish_interactive(data$ADSL, data$ADLB, which = w)
    expect_s3_class(p, "plotly")
    built <- plotly::plotly_build(p)

    traces <- built$x$data
    marker_traces <- Filter(
      function(tr) length(tr$hoverinfo) > 0L && all(tr$hoverinfo == "text"),
      traces
    )
    skip_traces <- Filter(
      function(tr) length(tr$hoverinfo) > 0L && all(tr$hoverinfo == "skip"),
      traces
    )
    text_traces <- Filter(function(tr) grepl("text", tr$mode %||% "", fixed = TRUE), traces)

    expect_true(length(marker_traces) >= 1L)
    expect_length(skip_traces, 1L)
    expect_length(text_traces, 0L)

    all_hover <- unlist(lapply(marker_traces, `[[`, "text"), use.names = FALSE)
    expect_true(length(all_hover) > 0L)
    expect_true(all(grepl("Subject:", all_hover, fixed = TRUE)))
    expect_true(all(grepl("Peak BILI:", all_hover, fixed = TRUE)))

    if (identical(w, "edish")) {
      expect_true(all(grepl("Peak ALT/AST:", all_hover, fixed = TRUE)))
      expect_true(all(grepl("x ULN", all_hover, fixed = TRUE)))
      expect_true(all(grepl("BILI - ALT/AST", all_hover, fixed = TRUE)))
      expect_true(any(grepl("positive = BILI after ALT/AST", all_hover, fixed = TRUE)))
    } else if (identical(w, "mdish")) {
      expect_true(all(grepl("x BL", all_hover, fixed = TRUE)))
      expect_true(all(grepl("on-trt peak", all_hover, fixed = TRUE)))
    } else {
      expect_true(all(grepl("Peak ALP:", all_hover, fixed = TRUE)))
      expect_true(all(grepl("BILI - ALP", all_hover, fixed = TRUE)))
      expect_false(any(grepl("Peak ALT/AST:", all_hover, fixed = TRUE)))
    }

    annots <- built$x$layout$annotations
    annot_texts <- vapply(annots, `[[`, character(1L), "text")
    expect_true(length(annots) >= 5L)
    expect_true(any(grepl("Laboratory screening only", annot_texts, fixed = TRUE)))
  }
})

test_that("plot_edish_interactive() omits the bottom caption when show_caption = FALSE and uses a larger bottom margin when it is shown", {
  data <- synthetic_hyslaw_fixture()
  p_on <- plot_edish_interactive(data$ADSL, data$ADLB, show_caption = TRUE)
  p_off <- plot_edish_interactive(data$ADSL, data$ADLB, show_caption = FALSE)
  b_on <- plotly::plotly_build(p_on)
  b_off <- plotly::plotly_build(p_off)

  on_texts <- vapply(b_on$x$layout$annotations, `[[`, character(1L), "text")
  off_texts <- vapply(b_off$x$layout$annotations, `[[`, character(1L), "text")
  expect_true(any(grepl("Laboratory screening only", on_texts, fixed = TRUE)))
  expect_false(any(grepl("Laboratory screening only", off_texts, fixed = TRUE)))
  expect_true(any(grepl("Hy's Law", off_texts, fixed = TRUE)))
  expect_gt(b_on$x$layout$margin$b, b_off$x$layout$margin$b)

  b_fac_on <- plotly::plotly_build(
    plot_edish_interactive(data$ADSL, data$ADLB, facet = TRUE, show_caption = TRUE)
  )
  b_fac_off <- plotly::plotly_build(
    plot_edish_interactive(data$ADSL, data$ADLB, facet = TRUE, show_caption = FALSE)
  )
  fac_on_texts <- vapply(b_fac_on$x$layout$annotations, `[[`, character(1L), "text")
  fac_off_texts <- vapply(b_fac_off$x$layout$annotations, `[[`, character(1L), "text")
  expect_true(any(grepl("Laboratory screening only", fac_on_texts, fixed = TRUE)))
  expect_false(any(grepl("Laboratory screening only", fac_off_texts, fixed = TRUE)))
  expect_gt(b_fac_on$x$layout$margin$b, b_fac_off$x$layout$margin$b)
})

test_that("plot_edish_interactive() supports log10 ticks, linear axes, and faceting", {
  data <- synthetic_hyslaw_fixture()

  p_log <- plot_edish_interactive(data$ADSL, data$ADLB, log_axes = TRUE)
  b_log <- plotly::plotly_build(p_log)
  expect_equal(b_log$x$layout$xaxis$type, "log")
  expect_equal(b_log$x$layout$yaxis$type, "log")
  expect_equal(b_log$x$layout$xaxis$tickvals, c(0.1, 1, 10))
  expect_equal(b_log$x$layout$xaxis$ticktext, c("0.1", "1", "10"))

  p_lin <- plot_edish_interactive(data$ADSL, data$ADLB, log_axes = FALSE)
  b_lin <- plotly::plotly_build(p_lin)
  expect_null(b_lin$x$layout$xaxis$type)

  p_fac <- plot_edish_interactive(data$ADSL, data$ADLB, facet = TRUE, color_by = "quadrant")
  b_fac <- plotly::plotly_build(p_fac)
  expect_s3_class(p_fac, "plotly")
  expect_true("xaxis2" %in% names(b_fac$x$layout))
  annot_texts <- vapply(b_fac$x$layout$annotations, `[[`, character(1L), "text")
  expect_true(any(grepl("Investigational", annot_texts, fixed = TRUE)))
  expect_true(any(grepl("Placebo", annot_texts, fixed = TRUE)))
})

test_that("edish_hover_text() resolves composite ALT/AST dates, ties, gaps, and missing values", {
  subjects <- data.frame(
    USUBJID = c("S-ALT", "S-AST", "S-TIE-ALT", "S-TIE-SAME", "S-MISS", "S-ESC"),
    TRT01A = c("Drug", "Drug", "Drug", "Drug", "Placebo", "Arm <50 & >10"),
    edish_quadrant_label = c(
      "Hy's Law", "Hy's Law", "Hy's Law", "Hy's Law",
      "Normal or near normal", "Normal or near normal"
    ),
    peak_alt_uln = c(6.0, 2.0, 5.0, 5.0, 0.02, 1.2),
    peak_ast_uln = c(3.0, 7.5, 5.0, 5.0, NA_real_, 0.8),
    peak_altast_uln = c(6.0, 7.5, 5.0, 5.0, 0.02, 1.2),
    peak_bili_uln = c(2.5, 2.5, 2.5, 2.5, 0.01, 0.9),
    ALT_ADT = as.Date(c("2024-02-01", "2024-02-01", "2024-02-03", "2024-02-04", NA, "2024-02-10")),
    AST_ADT = as.Date(c("2024-02-10", "2024-02-08", "2024-02-07", "2024-02-04", NA, "2024-02-10")),
    BILI_ADT = as.Date(c("2024-02-06", "2024-02-05", "2024-02-03", "2024-02-04", NA, "2024-02-10")),
    concurrent_hyslaw = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    potential_hyslaw = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    alp_elevated_in_window = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    alp_missing = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
    r_ratio = c(6.2, 1.5, NA_real_, 3.0, NA_real_, 2.0),
    injury_pattern = c("Hepatocellular", "Cholestatic", NA_character_, "Mixed", NA_character_, "Cholestatic"),
    stringsAsFactors = FALSE
  )
  classified <- list(
    subject_var = "USUBJID",
    arm_var = "TRT01A",
    exclude_alp = TRUE,
    alt_cut = 3,
    bili_cut = 2,
    alp_cut = 2
  )

  txt <- tm.hyslaw:::edish_hover_text(subjects, "edish", classified)

  # ALT drives peak; BILI 5 days after ALT -> +5 d
  expect_match(txt[[1L]], "Peak ALT/AST: 6.00x ULN \\(ALT, 2024-02-01\\)")
  expect_match(txt[[1L]], "Peak gap \\(BILI - ALT/AST\\): \\+5 d \\(positive = BILI after ALT/AST\\)")
  expect_match(txt[[1L]], "Potential Hy's Law: Yes")
  expect_match(txt[[1L]], "ALP in window: Not elevated")

  # AST drives peak; BILI 3 days before AST -> -3 d; ALP excluded
  expect_match(txt[[2L]], "Peak ALT/AST: 7.50x ULN \\(AST, 2024-02-08\\)")
  expect_match(txt[[2L]], "Peak gap \\(BILI - ALT/AST\\): -3 d")
  expect_match(txt[[2L]], "Potential Hy's Law: No \\(ALP excluded\\)")
  expect_match(txt[[2L]], "ALP in window: Elevated")

  # Tie with different dates -> earlier date (ALT 2024-02-03) and parameter named; 0 d gap
  expect_match(txt[[3L]], "ALT \\(tied with AST\\), 2024-02-03")
  expect_match(txt[[3L]], "Peak gap \\(BILI - ALT/AST\\): 0 d")

  # Tie on same date
  expect_match(txt[[4L]], "ALT/AST \\(tie\\), 2024-02-04")

  # Missing dates & raw unclipped values (< 0.05) & missing ALP
  expect_match(txt[[5L]], "Peak ALT/AST: 0.02x ULN \\(ALT, Missing\\)")
  expect_match(txt[[5L]], "Peak BILI: 0.01x ULN \\(Missing\\)")
  expect_match(txt[[5L]], "Peak gap \\(BILI - ALT/AST\\): Missing")
  expect_match(txt[[5L]], "ALP in window: Missing")
  expect_match(txt[[5L]], "R-ratio: Missing")

  # HTML escaping for special characters in group name
  expect_match(txt[[6L]], "Arm &lt;50 &amp; &gt;10", fixed = TRUE)
})
