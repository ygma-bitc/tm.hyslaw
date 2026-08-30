# Convenience wrapper for local development.
# Usage from the repository root:
#   shiny::runApp("app.R")

demo_app <- file.path("inst", "examples", "app.R")

if (file.exists(demo_app)) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(quiet = TRUE)
  } else if (!requireNamespace("tm.hyslaw", quietly = TRUE)) {
    stop(
      "Install the package or install pkgload and run from the repository root.",
      call. = FALSE
    )
  }
  shiny::runApp(demo_app, launch.browser = TRUE)
} else {
  if (!requireNamespace("tm.hyslaw", quietly = TRUE)) {
    stop("Package tm.hyslaw is not installed.", call. = FALSE)
  }
  shiny::runApp(
    system.file("examples", "app.R", package = "tm.hyslaw"),
    launch.browser = TRUE
  )
}
