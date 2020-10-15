#' Open the app that displays piecewise regression with 2 knots for spruce data
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{ shinyKnots()}
shinyKnots<-function(){
  shiny::runApp(system.file("Knots", package="regsisF20"),launch.browser = TRUE)
}
