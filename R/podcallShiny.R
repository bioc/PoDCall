#' @title PoDCall shiny launcher
#'
#' @importFrom shiny runApp
#' @importFrom shinyjs useShinyjs disable enable
#' @importFrom DT dataTableOutput coerceValue datatable renderDT
#'
#' @description This function launches the PoDCall shiny app in a web browser
#'
#' @return Does not return anything, but launches PoDCall shiny app
#'
#' @export
#'
#' @examples
#' \dontrun{
#' podcallShiny()
#' }
podcallShiny <- function() {
    appDir <- system.file("shinyApp", package="PoDCall")
    if (appDir == "") {
        stop("Could not find app directory. Try re-installing `PoDCall`.",
            call. = FALSE)
    }

    shiny::runApp(appDir,
                launch.browser=TRUE,
                display.mode="normal")
}
