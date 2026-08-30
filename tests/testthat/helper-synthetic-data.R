synthetic_hyslaw_fixture <- function(...) {
  make_synthetic_hyslaw_data(n_extra = 0L, seed = 1L, ...)
}

printed_table <- function(tbl) {
  paste(utils::capture.output(suppressMessages(print(tbl))), collapse = "\n")
}

subject_row <- function(classified, id) {
  classified$subjects[classified$subjects$USUBJID == id, , drop = FALSE]
}
