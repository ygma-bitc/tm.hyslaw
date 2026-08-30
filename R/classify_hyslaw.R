#' Classify eDISH peak quadrants and concurrent Hy's Law
#'
#' Peak eDISH uses each subject's **on-treatment** maximum ALT or AST / ULN
#' versus maximum bilirubin / ULN (not necessarily the same day). Concurrent
#' (biochemical potential) Hy's Law requires bilirubin meeting `bili_cut` on
#' or after an ALT/AST meeting `alt_cut`, within `window_days`. ALP meeting
#' `alp_cut` in the same window can optionally exclude the subject from
#' potential Hy's Law.
#'
#' Baseline records are kept for mDISH (peak / baseline) ratios. They are not
#' used as eDISH maxima.
#'
#' @param adsl ADSL-like data. Ignored when `prepped` is supplied.
#' @param adlb ADLB-like data. Ignored when `prepped` is supplied.
#' @param prepped Optional list from [prep_hyslaw_data()].
#' @param window_days Concurrent window in days after the ALT/AST elevation.
#'   Default `14` matches the pharmaverse admiral Hy's Law vignette.
#' @param alt_cut ALT/AST ULN multiple.
#' @param bili_cut Bilirubin ULN multiple.
#' @param alp_cut ALP ULN multiple used for cholestasis exclusion and the
#'   cholestatic screening plot.
#' @param exclude_alp If `TRUE`, concurrent cases with ALP meeting `alp_cut`
#'   in the window are not counted as potential Hy's Law.
#' @param compare `">="` (default) or `">"`. FDA / many CSR shells use
#'   strict greater-than; this package defaults to greater-or-equal.
#' @param ... Passed to [prep_hyslaw_data()] when `prepped` is `NULL`.
#'
#' @return The `prepped` list with a subject-level `subjects` data frame.
#' @export
classify_hyslaw <- function(adsl = NULL,
                            adlb = NULL,
                            prepped = NULL,
                            window_days = 14,
                            alt_cut = 3,
                            bili_cut = 2,
                            alp_cut = 2,
                            exclude_alp = TRUE,
                            compare = c(">=", ">"),
                            ...) {
  checkmate::assert_list(prepped, null.ok = TRUE)
  checkmate::assert_number(window_days, lower = 0, finite = TRUE)
  checkmate::assert_number(alt_cut, lower = 0, finite = TRUE)
  checkmate::assert_number(bili_cut, lower = 0, finite = TRUE)
  checkmate::assert_number(alp_cut, lower = 0, finite = TRUE)
  checkmate::assert_flag(exclude_alp)
  compare <- match.arg(compare)

  if (is.null(prepped)) {
    checkmate::assert_data_frame(adsl)
    checkmate::assert_data_frame(adlb)
    prepped <- prep_hyslaw_data(adsl = adsl, adlb = adlb, ...)
  }

  prepped$window_days <- window_days
  prepped$alt_cut <- alt_cut
  prepped$bili_cut <- bili_cut
  prepped$alp_cut <- alp_cut
  prepped$exclude_alp <- exclude_alp
  prepped$compare <- compare

  if (isTRUE(prepped$empty) || NROW(prepped$labs) == 0L) {
    prepped$subjects <- empty_subject_frame(prepped)
    return(prepped)
  }

  labs <- prepped$labs
  subject_var <- prepped$subject_var
  arm_var <- prepped$arm_var
  paramcd_var <- prepped$paramcd_var
  adt_var <- prepped$adt_var
  ady_var <- prepped$ady_var

  post <- labs[lab_is_post(labs), , drop = FALSE]
  base <- labs[lab_is_baseline(labs), , drop = FALSE]
  if (nrow(post) == 0L) {
    post <- labs
  }

  wide <- summarise_param_peaks(post, subject_var, paramcd_var, adt_var, ady_var) |>
    tidyr_safe_pivot(subject_var, paramcd_var)

  wide$peak_alt_uln <- wide$ALT
  wide$peak_ast_uln <- wide$AST
  wide$peak_bili_uln <- wide$BILI
  wide$peak_alp_uln <- wide$ALP
  wide$peak_altast_uln <- pmax(wide$peak_alt_uln, wide$peak_ast_uln, na.rm = TRUE)
  wide$peak_altast_uln[!is.finite(wide$peak_altast_uln)] <- NA_real_
  wide$edish_quadrant <- edish_quadrant_code(
    wide$peak_altast_uln,
    wide$peak_bili_uln,
    alt_cut = alt_cut,
    bili_cut = bili_cut,
    op = compare
  )
  wide$edish_quadrant_label <- unname(default_edish_quadrants()[wide$edish_quadrant])
  wide$cdish_quadrant <- cdish_quadrant_code(
    wide$peak_alp_uln,
    wide$peak_bili_uln,
    alp_cut = alp_cut,
    bili_cut = bili_cut,
    op = compare
  )
  wide$cdish_quadrant_label <- unname(default_cdish_quadrants()[wide$cdish_quadrant])

  if (nrow(base) > 0L) {
    wide_bl <- summarise_param_peaks(base, subject_var, paramcd_var, adt_var, ady_var) |>
      tidyr_safe_pivot(subject_var, paramcd_var)
    bl_keep <- wide_bl[, c(subject_var, "ALT", "AST", "BILI", "ALP"), drop = FALSE]
    names(bl_keep) <- c(subject_var, "base_alt_uln", "base_ast_uln", "base_bili_uln", "base_alp_uln")
    wide <- merge(wide, bl_keep, by = subject_var, all.x = TRUE, sort = FALSE)
  } else {
    wide$base_alt_uln <- NA_real_
    wide$base_ast_uln <- NA_real_
    wide$base_bili_uln <- NA_real_
    wide$base_alp_uln <- NA_real_
  }
  wide$base_altast_uln <- pmax(wide$base_alt_uln, wide$base_ast_uln, na.rm = TRUE)
  wide$base_altast_uln[!is.finite(wide$base_altast_uln)] <- NA_real_
  wide$peak_altast_bl <- ifelse(
    !is.na(wide$base_altast_uln) & wide$base_altast_uln > 0,
    wide$peak_altast_uln / wide$base_altast_uln,
    NA_real_
  )
  wide$peak_bili_bl <- ifelse(
    !is.na(wide$base_bili_uln) & wide$base_bili_uln > 0,
    wide$peak_bili_uln / wide$base_bili_uln,
    NA_real_
  )
  wide$peak_alp_bl <- ifelse(
    !is.na(wide$base_alp_uln) & wide$base_alp_uln > 0,
    wide$peak_alp_uln / wide$base_alp_uln,
    NA_real_
  )

  concurrent <- concurrent_pairs(
    labs = post,
    subject_var = subject_var,
    paramcd_var = paramcd_var,
    adt_var = adt_var,
    window_days = window_days,
    alt_cut = alt_cut,
    bili_cut = bili_cut,
    alp_cut = alp_cut,
    compare = compare,
    at_params = c("ALT", "AST")
  )
  concurrent_chol <- concurrent_pairs(
    labs = post,
    subject_var = subject_var,
    paramcd_var = paramcd_var,
    adt_var = adt_var,
    window_days = window_days,
    alt_cut = alp_cut,
    bili_cut = bili_cut,
    alp_cut = alp_cut,
    compare = compare,
    at_params = "ALP",
    check_alp = FALSE
  )
  names(concurrent_chol)[names(concurrent_chol) == "concurrent_hyslaw"] <- "concurrent_cholestatic"
  names(concurrent_chol)[names(concurrent_chol) == "concurrent_altast_adt"] <- "concurrent_alp_adt"
  concurrent_chol$alp_elevated_in_window <- NULL
  concurrent_chol$concurrent_bili_adt <- NULL

  classified_part <- merge(wide, concurrent, by = subject_var, all.x = TRUE, sort = FALSE)
  classified_part <- merge(
    classified_part,
    concurrent_chol,
    by = subject_var,
    all.x = TRUE,
    sort = FALSE
  )

  arm_map <- unique(prepped$adsl[, c(subject_var, arm_var), drop = FALSE])
  arms <- unique(as.character(arm_map[[arm_var]][is_present_chr(arm_map[[arm_var]])]))
  subjects <- merge(arm_map, classified_part, by = subject_var, all.x = TRUE, sort = FALSE)
  subjects$edish_quadrant[is.na(subjects$edish_quadrant)] <- "normal"
  subjects$edish_quadrant_label <- unname(default_edish_quadrants()[subjects$edish_quadrant])
  subjects$cdish_quadrant[is.na(subjects$cdish_quadrant)] <- "normal"
  subjects$cdish_quadrant_label <- unname(default_cdish_quadrants()[subjects$cdish_quadrant])
  subjects$concurrent_hyslaw <- isTRUE_na_false(subjects$concurrent_hyslaw)
  subjects$alp_elevated_in_window <- isTRUE_na_false(subjects$alp_elevated_in_window)
  subjects$concurrent_cholestatic <- isTRUE_na_false(subjects$concurrent_cholestatic)
  subjects$alp_missing <- is.na(subjects$peak_alp_uln)
  subjects$potential_hyslaw <- subjects$concurrent_hyslaw &
    (if (isTRUE(exclude_alp)) !subjects$alp_elevated_in_window else TRUE)
  subjects$r_ratio <- ifelse(
    !is.na(subjects$peak_alp_uln) & subjects$peak_alp_uln > 0,
    subjects$peak_alt_uln / subjects$peak_alp_uln,
    NA_real_
  )
  subjects$r_ratio <- refine_r_ratio(
    subjects,
    labs = post,
    subject_var = subject_var,
    paramcd_var = paramcd_var,
    adt_var = adt_var
  )
  subjects$injury_pattern <- r_ratio_pattern(subjects$r_ratio)
  subjects[[arm_var]] <- factor_arm(subjects[[arm_var]], arms)

  prepped$subjects <- subjects
  prepped$empty <- FALSE
  prepped
}


#' @keywords internal
summarise_param_peaks <- function(labs, subject_var, paramcd_var, adt_var, ady_var) {
  if (nrow(labs) == 0L) {
    out <- data.frame(
      peak_ratio = numeric(),
      peak_adt = as.Date(character()),
      peak_ady = numeric(),
      stringsAsFactors = FALSE
    )
    out[[subject_var]] <- character()
    out[[paramcd_var]] <- character()
    return(out)
  }
  peaks <- labs |>
    dplyr::group_by(.data[[subject_var]], .data[[paramcd_var]]) |>
    dplyr::summarise(
      peak_ratio = suppressWarnings(max(.data$RATIO_ULN, na.rm = TRUE)),
      peak_adt = {
        idx <- which.max(.data$RATIO_ULN)
        if (length(idx) == 0L) as.Date(NA) else .data[[adt_var]][[idx]]
      },
      peak_ady = {
        idx <- which.max(.data$RATIO_ULN)
        if (length(idx) == 0L) NA_real_ else as.numeric(.data[[ady_var]][[idx]])
      },
      .groups = "drop"
    )
  peaks$peak_ratio[!is.finite(peaks$peak_ratio)] <- NA_real_
  peaks
}


#' @keywords internal
tidyr_safe_pivot <- function(peaks, subject_var, paramcd_var) {
  peaks <- as.data.frame(peaks, stringsAsFactors = FALSE)
  params <- unique(as.character(peaks[[paramcd_var]]))
  params <- params[is_present_chr(params)]
  base <- unique(peaks[, subject_var, drop = FALSE])
  if (nrow(base) == 0L) {
    base <- data.frame(stringsAsFactors = FALSE)
    base[[subject_var]] <- character()
  }
  for (pc in params) {
    sl <- peaks[peaks[[paramcd_var]] == pc, , drop = FALSE]
    keep <- as.data.frame(
      sl[, c(subject_var, "peak_ratio", "peak_adt", "peak_ady"), drop = FALSE],
      stringsAsFactors = FALSE
    )
    names(keep) <- c(subject_var, pc, paste0(pc, "_ADT"), paste0(pc, "_ADY"))
    base <- merge(base, keep, by = subject_var, all.x = TRUE, sort = FALSE)
  }
  if (!"ALT" %in% names(base)) base$ALT <- NA_real_
  if (!"AST" %in% names(base)) base$AST <- NA_real_
  if (!"BILI" %in% names(base)) base$BILI <- NA_real_
  if (!"ALP" %in% names(base)) base$ALP <- NA_real_
  base
}


#' @keywords internal
isTRUE_na_false <- function(x) {
  out <- as.logical(x)
  out[is.na(out)] <- FALSE
  out
}


#' @keywords internal
empty_subject_frame <- function(prepped) {
  subject_var <- prepped$subject_var %||% "USUBJID"
  arm_var <- prepped$arm_var %||% "TRT01A"
  out <- data.frame(
    peak_alt_uln = numeric(),
    peak_ast_uln = numeric(),
    peak_bili_uln = numeric(),
    peak_alp_uln = numeric(),
    peak_altast_uln = numeric(),
    base_alt_uln = numeric(),
    base_ast_uln = numeric(),
    base_bili_uln = numeric(),
    base_alp_uln = numeric(),
    base_altast_uln = numeric(),
    peak_altast_bl = numeric(),
    peak_bili_bl = numeric(),
    peak_alp_bl = numeric(),
    edish_quadrant = character(),
    edish_quadrant_label = character(),
    cdish_quadrant = character(),
    cdish_quadrant_label = character(),
    concurrent_hyslaw = logical(),
    alp_elevated_in_window = logical(),
    concurrent_cholestatic = logical(),
    alp_missing = logical(),
    potential_hyslaw = logical(),
    r_ratio = numeric(),
    injury_pattern = character(),
    concurrent_altast_adt = as.Date(character()),
    concurrent_bili_adt = as.Date(character()),
    concurrent_alp_adt = as.Date(character()),
    stringsAsFactors = FALSE
  )
  out[[subject_var]] <- character()
  out[[arm_var]] <- character()
  out
}


#' @keywords internal
refine_r_ratio <- function(subjects, labs, subject_var, paramcd_var, adt_var) {
  r <- subjects$r_ratio
  if (nrow(subjects) == 0L || nrow(labs) == 0L) {
    return(r)
  }
  has_date <- "concurrent_altast_adt" %in% names(subjects)
  if (!isTRUE(has_date)) {
    return(r)
  }
  for (i in seq_len(nrow(subjects))) {
    adt <- subjects$concurrent_altast_adt[[i]]
    if (is.na(adt)) {
      next
    }
    id <- subjects[[subject_var]][[i]]
    alt <- labs[
      labs[[subject_var]] == id & labs[[paramcd_var]] == "ALT" & labs[[adt_var]] == adt,
      "RATIO_ULN",
      drop = TRUE
    ]
    alp <- labs[
      labs[[subject_var]] == id & labs[[paramcd_var]] == "ALP" & labs[[adt_var]] == adt,
      "RATIO_ULN",
      drop = TRUE
    ]
    if (length(alt) > 0L && length(alp) > 0L && isTRUE(alp[[1L]] > 0) && !is.na(alt[[1L]])) {
      r[[i]] <- alt[[1L]] / alp[[1L]]
    }
  }
  r
}


#' @keywords internal
concurrent_pairs <- function(labs,
                             subject_var,
                             paramcd_var,
                             adt_var,
                             window_days,
                             alt_cut,
                             bili_cut,
                             alp_cut,
                             compare = ">=",
                             at_params = c("ALT", "AST"),
                             check_alp = TRUE) {
  altast <- labs[
    labs[[paramcd_var]] %in% at_params &
      meets_cut(labs$RATIO_ULN, alt_cut, compare) &
      !is.na(labs[[adt_var]]),
    ,
    drop = FALSE
  ]
  bili <- labs[
    labs[[paramcd_var]] == "BILI" &
      meets_cut(labs$RATIO_ULN, bili_cut, compare) &
      !is.na(labs[[adt_var]]),
    ,
    drop = FALSE
  ]
  alp <- labs[
    labs[[paramcd_var]] == "ALP" &
      meets_cut(labs$RATIO_ULN, alp_cut, compare) &
      !is.na(labs[[adt_var]]),
    ,
    drop = FALSE
  ]

  ids <- unique(as.character(labs[[subject_var]]))
  out <- data.frame(
    concurrent_hyslaw = FALSE,
    alp_elevated_in_window = FALSE,
    concurrent_altast_adt = as.Date(rep(NA, length(ids))),
    concurrent_bili_adt = as.Date(rep(NA, length(ids))),
    stringsAsFactors = FALSE
  )
  out[[subject_var]] <- ids

  if (nrow(altast) == 0L || nrow(bili) == 0L) {
    return(out)
  }

  for (i in seq_along(ids)) {
    id <- ids[[i]]
    aa <- altast[altast[[subject_var]] == id, , drop = FALSE]
    bb <- bili[bili[[subject_var]] == id, , drop = FALSE]
    if (nrow(aa) == 0L || nrow(bb) == 0L) {
      next
    }
    hit <- NULL
    for (j in seq_len(nrow(aa))) {
      delta <- as.numeric(bb[[adt_var]] - aa[[adt_var]][[j]])
      ok <- !is.na(delta) & delta >= 0 & delta <= window_days
      if (any(ok)) {
        k <- which(ok)[[1L]]
        hit <- list(altast_adt = aa[[adt_var]][[j]], bili_adt = bb[[adt_var]][[k]])
        break
      }
    }
    if (is.null(hit)) {
      next
    }
    out$concurrent_hyslaw[[i]] <- TRUE
    out$concurrent_altast_adt[[i]] <- hit$altast_adt
    out$concurrent_bili_adt[[i]] <- hit$bili_adt
    if (isTRUE(check_alp)) {
      ap <- alp[alp[[subject_var]] == id, , drop = FALSE]
      if (nrow(ap) > 0L) {
        alp_delta <- as.numeric(ap[[adt_var]] - hit$altast_adt)
        out$alp_elevated_in_window[[i]] <- any(
          !is.na(alp_delta) & alp_delta >= 0 & alp_delta <= window_days,
          na.rm = TRUE
        )
      }
    }
  }

  out
}
