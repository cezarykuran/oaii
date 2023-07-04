#' Run demo shiny app
#'
#' @inherit shiny::runApp description params return
#' @export
#'
#' @examples
#' \dontrun{
#'   demoApp()
#'   demoApp("127.0.0.1", 80)
#' }
#'
demoApp <- function(host = "0.0.0.0", port = 3838) {
  shiny::runApp(
    system.file("app", package = "oaii"),
    host = host,
    port = port
  )
}
