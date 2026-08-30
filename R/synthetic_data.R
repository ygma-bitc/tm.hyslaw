#' Build synthetic ADSL / ADLB fixtures for Hy's Law demos and tests
#'
#' Seeds subjects into the four eDISH peak quadrants and two extra patterns
#' that distinguish peak-only Hy's Law from concurrent-window monitoring:
#' a peak pair outside the bilirubin window, and a concurrent pair with
#' elevated ALP (cholestasis exclusion).
#'
#' @param n_extra `integer(1)` Extra normal-range subjects per arm.
#' @param seed `integer(1)` Random seed for extra subjects only.
#'
#' @return A list with `ADSL` and `ADLB`.
#'
#' @examples
#' data <- make_synthetic_hyslaw_data(seed = 1L)
#' names(data)
#' unique(data$ADLB$PARAMCD)
#'
#' @export
make_synthetic_hyslaw_data <- function(n_extra = 6L, seed = 20260829L) {
  checkmate::assert_integerish(n_extra, len = 1L, lower = 0L)
  checkmate::assert_integerish(seed, len = 1L)

  origin <- as.Date("2024-01-01")
  uln <- list(ALT = 40, AST = 40, BILI = 17, ALP = 120)

  lab_row <- function(usubjid, paramcd, multiple, ady, ontrt = "Y") {
    data.frame(
      STUDYID = "HYLAW-SYN",
      USUBJID = usubjid,
      PARAMCD = paramcd,
      PARAM = switch(
        paramcd,
        ALT = "Alanine Aminotransferase (U/L)",
        AST = "Aspartate Aminotransferase (U/L)",
        BILI = "Bilirubin (umol/L)",
        ALP = "Alkaline Phosphatase (U/L)",
        paramcd
      ),
      AVAL = unname(uln[[paramcd]] * multiple),
      ANRHI = unname(uln[[paramcd]]),
      ADY = as.integer(ady),
      ADT = origin + as.integer(ady),
      AVISIT = paste("Day", ady),
      ONTRTFL = ontrt,
      ABLFL = ifelse(ady <= 0L, "Y", ""),
      stringsAsFactors = FALSE
    )
  }

  panel <- function(usubjid, ady, alt, ast, bili, alp, ontrt = "Y") {
    rbind(
      lab_row(usubjid, "ALT", alt, ady, ontrt),
      lab_row(usubjid, "AST", ast, ady, ontrt),
      lab_row(usubjid, "BILI", bili, ady, ontrt),
      lab_row(usubjid, "ALP", alp, ady, ontrt)
    )
  }

  subjects <- list(
    list(id = "P-HY-01", arm = "Placebo", pattern = "concurrent"),
    list(id = "P-TEMP-01", arm = "Placebo", pattern = "temple"),
    list(id = "P-BILI-01", arm = "Placebo", pattern = "isolated_bili"),
    list(id = "P-NORM-01", arm = "Placebo", pattern = "normal"),
    list(id = "I-HY-01", arm = "Investigational", pattern = "concurrent"),
    list(id = "I-PEAK-01", arm = "Investigational", pattern = "peak_only"),
    list(id = "I-ALP-01", arm = "Investigational", pattern = "alp_exclude"),
    list(id = "I-TEMP-01", arm = "Investigational", pattern = "temple"),
    list(id = "I-BILI-01", arm = "Investigational", pattern = "isolated_bili"),
    list(id = "I-NORM-01", arm = "Investigational", pattern = "normal")
  )

  adlb_fixed <- do.call(rbind, lapply(subjects, function(s) {
    id <- s$id
    switch(
      s$pattern,
      concurrent = rbind(
        panel(id, 0L, 0.8, 0.7, 0.6, 0.9, "N"),
        panel(id, 10L, 4.2, 3.6, 1.1, 1.1),
        panel(id, 16L, 2.0, 1.8, 2.4, 1.2)
      ),
      peak_only = rbind(
        panel(id, 0L, 0.9, 0.8, 0.7, 0.8, "N"),
        panel(id, 8L, 4.5, 3.8, 1.0, 1.0),
        panel(id, 40L, 1.4, 1.2, 2.6, 1.1)
      ),
      alp_exclude = rbind(
        panel(id, 0L, 0.8, 0.8, 0.6, 0.9, "N"),
        panel(id, 12L, 3.8, 3.2, 1.0, 2.6),
        panel(id, 18L, 2.2, 1.9, 2.3, 2.4)
      ),
      temple = rbind(
        panel(id, 0L, 0.7, 0.7, 0.5, 0.8, "N"),
        panel(id, 14L, 5.0, 4.2, 1.2, 1.1)
      ),
      isolated_bili = rbind(
        panel(id, 0L, 0.8, 0.7, 0.6, 0.9, "N"),
        panel(id, 20L, 1.6, 1.4, 2.5, 1.0)
      ),
      normal = rbind(
        panel(id, 0L, 0.7, 0.6, 0.5, 0.8, "N"),
        panel(id, 21L, 1.2, 1.1, 0.8, 0.9)
      ),
      stop("Unknown synthetic pattern.", call. = FALSE)
    )
  }))

  extra <- NULL
  if (n_extra > 0L) {
    set.seed(as.integer(seed))
    extra_ids <- c(
      sprintf("P-X-%02d", seq_len(n_extra)),
      sprintf("I-X-%02d", seq_len(n_extra))
    )
    extra_arms <- c(
      rep("Placebo", n_extra),
      rep("Investigational", n_extra)
    )
    extra <- do.call(rbind, lapply(seq_along(extra_ids), function(i) {
      rbind(
        panel(extra_ids[[i]], 0L, 0.6 + runif(1L, 0, 0.3), 0.6, 0.5, 0.8, "N"),
        panel(extra_ids[[i]], 21L, 0.8 + runif(1L, 0, 0.5), 0.9, 0.7, 0.9)
      )
    }))
    extra_adsl <- data.frame(
      STUDYID = "HYLAW-SYN",
      USUBJID = extra_ids,
      TRT01A = extra_arms,
      SAFFL = "Y",
      ITTFL = "Y",
      SEX = ifelse(seq_along(extra_ids) %% 2L == 0L, "F", "M"),
      RACE = ifelse(seq_along(extra_ids) %% 3L == 0L, "ASIAN", "WHITE"),
      COUNTRY = c("USA", "CAN", "GBR")[((seq_along(extra_ids) - 1L) %% 3L) + 1L],
      stringsAsFactors = FALSE
    )
  } else {
    extra_adsl <- NULL
  }

  fixed_adsl <- data.frame(
    STUDYID = "HYLAW-SYN",
    USUBJID = vapply(subjects, `[[`, character(1L), "id"),
    TRT01A = vapply(subjects, `[[`, character(1L), "arm"),
    SAFFL = "Y",
    ITTFL = "Y",
    SEX = c("F", "M", "F", "M", "F", "M", "F", "M", "F", "M"),
    RACE = c("WHITE", "ASIAN", "WHITE", "BLACK", "WHITE", "ASIAN", "WHITE", "BLACK", "WHITE", "ASIAN"),
    COUNTRY = c("USA", "CAN", "GBR", "USA", "CAN", "GBR", "USA", "CAN", "GBR", "USA"),
    stringsAsFactors = FALSE
  )

  adsl <- rbind(fixed_adsl, extra_adsl)
  adsl$TRT01A <- factor(adsl$TRT01A, levels = c("Placebo", "Investigational"))
  adlb <- rbind(adlb_fixed, extra)
  rownames(adsl) <- NULL
  rownames(adlb) <- NULL

  list(ADSL = adsl, ADLB = adlb)
}
