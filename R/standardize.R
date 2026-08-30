#' Standardize an ADSL-like data frame for Hy's Law review
#'
#' Harmonizes common CDISC Pilot / `pharmaverseadam` / `random.cdisc.data`
#' column variants so [prep_hyslaw_data()] can join treatment and population
#' flags.
#'
#' @param adsl A data frame with at least `USUBJID`.
#' @return A standardized ADSL data frame.
#' @export
standardize_adsl <- function(adsl) {
  checkmate::assert_data_frame(adsl)
  assert_required_cols(adsl, "USUBJID")

  trt01a <- if ("TRT01A" %in% names(adsl)) {
    adsl[["TRT01A"]]
  } else if ("ARM" %in% names(adsl)) {
    adsl[["ARM"]]
  } else {
    rep("Treatment A", nrow(adsl))
  }

  out <- adsl
  out$STUDYID <- as.character(hy_col_or(adsl, "STUDYID", "MOCK-STUDY"))
  out$USUBJID <- as.character(adsl$USUBJID)
  out$TRT01A <- as.character(trt01a)
  out$SAFFL <- as.character(hy_col_or(adsl, "SAFFL", "Y"))
  out$ITTFL <- as.character(hy_col_or(adsl, "ITTFL", out$SAFFL))
  out$SEX <- as.character(hy_col_or(adsl, "SEX", NA_character_))
  out$RACE <- as.character(hy_col_or(adsl, "RACE", NA_character_))
  out$COUNTRY <- as.character(hy_col_or(adsl, "COUNTRY", NA_character_))

  out[!duplicated(out$USUBJID), , drop = FALSE]
}


#' Standardize an ADLB / ADLBC-like data frame
#'
#' Maps Pilot `A1HI` to `ANRHI`, `ALKPH` to `ALP`, and common value columns
#' (`AVAL`, `LBSTRESN`) so [prep_hyslaw_data()] can compute ULN multiples.
#'
#' @param adlb A laboratory BDS data frame.
#' @param adsl Optional standardized ADSL used to back-fill `TRT01A`.
#' @return A standardized ADLB data frame.
#' @export
standardize_adlb <- function(adlb, adsl = NULL) {
  checkmate::assert_data_frame(adlb)
  checkmate::assert_data_frame(adsl, null.ok = TRUE)
  assert_required_cols(adlb, "USUBJID")

  aval_var <- first_present(adlb, c("AVAL", "LBSTRESN"))
  if (is.null(aval_var)) {
    stop("[ADLB] Missing required column(s): AVAL", call. = FALSE)
  }
  anrhi_var <- first_present(adlb, c("ANRHI", "A1HI", "LBSTNRHI"))
  if (is.null(anrhi_var)) {
    stop("[ADLB] Missing required column(s): ANRHI", call. = FALSE)
  }

  paramcd <- if ("PARAMCD" %in% names(adlb)) {
    as.character(adlb$PARAMCD)
  } else if ("LBTESTCD" %in% names(adlb)) {
    as.character(adlb$LBTESTCD)
  } else {
    stop("[ADLB] Missing required column(s): PARAMCD", call. = FALSE)
  }
  paramcd[toupper(paramcd) == "ALKPH"] <- "ALP"
  paramcd[toupper(paramcd) == "TBILI"] <- "BILI"

  out <- adlb
  out$STUDYID <- as.character(hy_col_or(adlb, "STUDYID", "MOCK-STUDY"))
  out$USUBJID <- as.character(adlb$USUBJID)
  out$PARAMCD <- toupper(trimws(paramcd))
  out$PARAM <- as.character(hy_col_or(adlb, "PARAM", hy_col_or(adlb, "LBTEST", out$PARAMCD)))
  out$AVAL <- as.numeric(adlb[[aval_var]])
  out$ANRHI <- as.numeric(adlb[[anrhi_var]])
  out$ADT <- if ("ADT" %in% names(adlb)) {
    hy_to_date(adlb$ADT)
  } else if ("LBDTC" %in% names(adlb)) {
    hy_to_date(adlb$LBDTC)
  } else {
    as.Date(rep(NA, nrow(adlb)))
  }
  out$ADY <- if ("ADY" %in% names(adlb)) {
    as.numeric(adlb$ADY)
  } else if ("LBDY" %in% names(adlb)) {
    as.numeric(adlb$LBDY)
  } else {
    as.numeric(rep(NA, nrow(adlb)))
  }
  if ("AVISIT" %in% names(adlb)) {
    out$AVISIT <- as.character(adlb$AVISIT)
  } else if ("VISIT" %in% names(adlb)) {
    out$AVISIT <- as.character(adlb$VISIT)
  }
  if ("ONTRTFL" %in% names(adlb)) {
    out$ONTRTFL <- as.character(adlb$ONTRTFL)
  }
  if ("ABLFL" %in% names(adlb)) {
    out$ABLFL <- as.character(adlb$ABLFL)
  }
  if ("BASE" %in% names(adlb)) {
    out$BASE <- as.numeric(adlb$BASE)
  }
  if ("TRT01A" %in% names(adlb)) {
    out$TRT01A <- as.character(adlb$TRT01A)
  }

  if (!is.null(adsl) && "TRT01A" %in% names(adsl)) {
    arm_map <- unique(adsl[, c("USUBJID", "TRT01A"), drop = FALSE])
    names(arm_map)[2L] <- "TRT01A_ADSL"
    out <- merge(out, arm_map, by = "USUBJID", all.x = TRUE, sort = FALSE)
    if ("TRT01A" %in% names(out)) {
      out$TRT01A <- ifelse(is_present_chr(out$TRT01A), out$TRT01A, out$TRT01A_ADSL)
    } else {
      out$TRT01A <- out$TRT01A_ADSL
    }
    out$TRT01A_ADSL <- NULL
  }

  out
}
