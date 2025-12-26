#' Launch the HOME Shiny App
#'
#' Opens the interactive dashboard for Harmonized Orphanhood Mortality Estimation.
#'
#' @return A Shiny App object.
#' @export
app_HOME <- function() {
  # Localiza a pasta onde o app foi instalado
  app_dir <- system.file("shiny", "home_app", package = "HOME")

  if (app_dir == "") {
    stop("Could not find the Shiny app. Try re-installing the 'HOME' package.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
