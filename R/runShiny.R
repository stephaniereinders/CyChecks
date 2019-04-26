#' @name runShiny
#' @title Run Shiny App
#' @description Launch Shiny App that displays pay information for subsets of Iowa State University Employees
#' @return a Shiny App
#' @examples
#' \dontrun{
#' runShiny()
#' }
#' @export

runShiny <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "CyChecks")
  if (appDir == ""){
    stop("Couldn't locate example directory. Try re-installing `CyChecks`", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
