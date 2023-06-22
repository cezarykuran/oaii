demo <- function(host = "0.0.0.0", port = 38383) {
  shiny::runApp(
    system.file("app", package = "oaii"),
    host = host,
    port = port
  )
}
