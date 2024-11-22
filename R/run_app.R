
#' Run the App
#' @import DT
#' @import calendR
#' @import colourpicker
#' @import ggplot2
#' @import lubridate
#'
#' @return A NULL object
#' @export
#'
#' @examples
#' library(shinycalendr)
#' if(interactive()){
#' run_app()
#' }
run_app <- function() {
  appDir <- system.file("ShinyCalendR", package = "shinycalendr")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `shinycalendr`.", call. = FALSE)
  }
  shiny::runApp(paste0(appDir, "/app.R"), )
}
