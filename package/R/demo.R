#' Run demo shiny app
#'
#' @inherit shiny::runApp description params return
#' @export
#'
#' @examples
#' \dontrun{
#'   demo_shiny()
#'   demo_shiny("127.0.0.1", 80)
#' }
#'
demo_shiny <- function(host = "0.0.0.0", port = 3838) {
  req_pkgs <- c(
    "base64enc",
    "cli",
    "colourpicker",
    "fontawesome",
    "htmltools",
    "log4r",
    "markdown",
    "shiny",
    "shinyjs"
  )
  if (check_demo_req_pkgs(req_pkgs)) {
    shiny::runApp(
      system.file("app", "shiny", package = "oaii"),
      host = host,
      port = port
    )
  }
}

#' Run one of the console application
#'
#' Run one of the console application from the package app/cli directory.
#' @inheritParams request
#' @param show_source flag, should the code of the selected script be displayed
#' before executing it?
#' @return unused
#' @export
#'
#' @examples
#' \dontrun{
#'   demo_cli()
#'   demo_cli("super-secret-key")
#' }
#'
demo_cli <- function(api_key = Sys.getenv("API_KEY"), show_source = TRUE) {
  if (!check_demo_req_pkgs("cli", "jsonlite")) return()

  # get OpenAI api key (if missing)
  while(!nchar(api_key)) {
    api_key <- readline("api_key: ")
  }
  cli::cat_line()

  # get list of available scripts
  apps <- sort(list.files(
    system.file("app", "cli", package = "oaii"),
    pattern = "*.R",
    full.names = TRUE
  ))

  # let user chose script/application
  n <- NA
  while(is.na(n) || n < 1 || n > length(apps)+1) {
    cli::cat_line("Select demo CLI app:", col = "green")
    for(n in seq_along(apps)) {
      app_basename <- basename(apps[n])
      app_name <- gsub("_", " ", substr(app_basename, 4, nchar(app_basename) -2))
      cli::cat_line(" ", cli::style_bold(n), ": ", app_name)
    }
    cli::cat_line(" ", cli::style_bold(n+1), ": ", cli::style_bold("exit"))

    n <- as.integer(readline("selection: "))
    cli::cat_line()
  }
  if (n == length(apps)+1) return(invisible())

  # display app source
  if (show_source) {
    cli::cli_rule("Script content")
    cli::cat_line(readLines(apps[n]))
    cli::cat_line()
  }

  # run app and display output
  cli::cli_rule("Script output")
  cli::cat_line()
  source(file  = apps[n], echo = FALSE, local = TRUE)
}

#' Check the existence of required packages
#'
#' @param ... required package name(s)
#' @return FALSE if one of the packages is missing, TRUE otherwise
#' @noRd
#'
check_demo_req_pkgs <- function(...) {
  req <- unlist(list(...))
  missed <- req[!req %in% rownames(utils::installed.packages())]
  if (length(missed)) {
    log_error("this demo required '", paste0(missed, collapse = "', '"), "' packages!")
    FALSE
  }
  else TRUE
}
