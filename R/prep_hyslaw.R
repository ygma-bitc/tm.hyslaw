#' Prepare ADSL / ADLB records for Hy's Law review
#'
#' Filters liver-related laboratory parameters, computes ULN multiples, and
#' joins the analysis population. An empty result is returned as a structured
#' list (`empty = TRUE`) so teal modules can show a no-data message.
#'
#' @param adsl ADSL-like subject-level data.
#' @param adlb ADLB-like BDS laboratory data.
#' @param arm_var Treatment / grouping variable.
#' @param subject_var Subject identifier.
#' @param paramcd_var Parameter code variable.
#' @param aval_var Analysis value variable.
#' @param anrhi_var ULN variable.
#' @param adt_var Analysis date variable.
#' @param ady_var Analysis day variable.
#' @param population_var Optional population flag variable.
#' @param population_value Population flag value to keep.
#' @param ontrt_only If `TRUE` and `ONTRTFL` exists, keep on-treatment records
#'   plus baseline (`ABLFL == "Y"`) when that flag is present.
#' @param liver_paramcds Parameter codes to keep. `ALKPH` is treated as ALP.
#'
#' @return A list with long `labs`, `adsl`, `alt_counts_df`, metadata, and
#'   `empty`.
#' @export
prep_hyslaw_data <- function(adsl,
                             adlb,
                             arm_var = "TRT01A",
                             subject_var = "USUBJID",
                             paramcd_var = "PARAMCD",
                             aval_var = "AVAL",
                             anrhi_var = "ANRHI",
                             adt_var = "ADT",
                             ady_var = "ADY",
                             population_var = "SAFFL",
                             population_value = "Y",
                             ontrt_only = TRUE,
                             liver_paramcds = c("ALT", "AST", "BILI", "ALP", "ALKPH")) {
  checkmate::assert_data_frame(adsl)
  checkmate::assert_data_frame(adlb)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(subject_var)
  checkmate::assert_string(paramcd_var)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(anrhi_var)
  checkmate::assert_string(adt_var)
  checkmate::assert_string(ady_var)
  checkmate::assert_string(population_var, null.ok = TRUE)
  checkmate::assert_flag(ontrt_only)
  checkmate::assert_character(liver_paramcds, min.len = 1L)

  adsl <- standardize_adsl(adsl)
  adlb <- standardize_adlb(adlb, adsl)

  assert_required_cols(adlb, c(subject_var, paramcd_var, aval_var, anrhi_var), "ADLB")
  assert_required_cols(adsl, c(subject_var, arm_var), "ADSL")

  if (!is.null(population_var) && population_var %in% names(adsl)) {
    adsl_pop <- adsl[eq_chr(adsl[[population_var]], population_value), , drop = FALSE]
  } else {
    adsl_pop <- adsl
  }

  labs <- adlb[adlb[[subject_var]] %in% adsl_pop[[subject_var]], , drop = FALSE]
  labs[[paramcd_var]] <- toupper(trimws(as.character(labs[[paramcd_var]])))
  labs[[paramcd_var]][labs[[paramcd_var]] == "ALKPH"] <- "ALP"
  labs <- labs[labs[[paramcd_var]] %in% toupper(liver_paramcds) | labs[[paramcd_var]] == "ALP", , drop = FALSE]
  labs <- labs[labs[[paramcd_var]] %in% c("ALT", "AST", "BILI", "ALP"), , drop = FALSE]

  if (isTRUE(ontrt_only) && "ONTRTFL" %in% names(labs)) {
    keep_ontrt <- eq_chr(labs$ONTRTFL, "Y")
    keep_base <- "ABLFL" %in% names(labs) & eq_chr(labs$ABLFL, "Y")
    labs <- labs[keep_ontrt | keep_base, , drop = FALSE]
  }

  labs[[aval_var]] <- as.numeric(labs[[aval_var]])
  labs[[anrhi_var]] <- as.numeric(labs[[anrhi_var]])
  labs$RATIO_ULN <- ifelse(
    !is.na(labs[[anrhi_var]]) & labs[[anrhi_var]] > 0,
    labs[[aval_var]] / labs[[anrhi_var]],
    NA_real_
  )
  if (adt_var %in% names(labs)) {
    labs[[adt_var]] <- hy_to_date(labs[[adt_var]])
  } else {
    labs[[adt_var]] <- as.Date(rep(NA, nrow(labs)))
  }
  if (!ady_var %in% names(labs)) {
    labs[[ady_var]] <- as.numeric(rep(NA, nrow(labs)))
  }

  if (!arm_var %in% names(labs)) {
    arm_map <- unique(adsl_pop[, c(subject_var, arm_var), drop = FALSE])
    labs <- merge(labs, arm_map, by = subject_var, all.x = TRUE, sort = FALSE)
  }

  labs <- labs[
    !is.na(labs$RATIO_ULN) & is_present_chr(labs[[arm_var]]),
    ,
    drop = FALSE
  ]

  empty <- nrow(labs) == 0L || nrow(adsl_pop) == 0L
  list(
    labs = labs,
    adsl = adsl_pop,
    alt_counts_df = adsl_pop,
    arm_var = arm_var,
    subject_var = subject_var,
    paramcd_var = paramcd_var,
    aval_var = aval_var,
    anrhi_var = anrhi_var,
    adt_var = adt_var,
    ady_var = ady_var,
    empty = empty
  )
}
