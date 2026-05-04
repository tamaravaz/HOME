#' Launch the HOME Shiny Application
#'
#' Opens the interactive dashboard for indirect adult mortality estimation
#' using the orphanhood-based methods implemented in the \pkg{HOME} package.
#' The application provides a graphical interface to \code{\link{om_estimate_index}},
#' \code{\link{om_plot_linearity}}, \code{\link{om_sensitivity}}, and
#' \code{\link{om_sensitivity_family}}.
#'
#' @details
#' The Shiny application is located in \code{inst/shiny/home_app/} and is
#' launched via \code{shiny::runApp()}. An active R session with the
#' \pkg{shiny}, \pkg{bslib}, \pkg{plotly}, \pkg{DT}, \pkg{readxl}, and
#' \pkg{writexl} packages is required; these are listed under \code{Suggests}
#' in the package \code{DESCRIPTION} file.
#'
#' @return This function is called for its side effect of launching a Shiny
#'   application. It does not return a meaningful value; the return value of
#'   \code{shiny::runApp()} is returned invisibly.
#'
#' @seealso \code{\link{om_estimate_index}} for the underlying estimation
#'   function.
#'
#' @examples
#' \dontrun{
#'   app_HOME()
#' }
#'
#' @export
app_HOME <- function() {
  app_dir <- system.file("shiny", "home_app", package = "HOME")

  if (!nzchar(app_dir)) {
    stop(
      "Could not find the Shiny app directory. ",
      "Try re-installing the 'HOME' package.",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
