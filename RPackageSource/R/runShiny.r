#' run shiny app
#' @export

runShiny <- function() {
    appDir <- system.file("shinyApp", package = "COMETS")
    if (appDir == "") {
        stop(" The ShinyApp directory was not found.
             Try re-installing `COMETS`.",
             call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = "normal")
}
