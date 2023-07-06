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

#' Run demo shiny app
#'
#' @inherit shiny::runApp description params return
#' @export
#'
#' @examples
#' \dontrun{
#'   demoShinyApp()
#'   demoShinyApp("127.0.0.1", 80)
#' }
#'
demoShinyApp <- function(host = "0.0.0.0", port = 3838) {
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

#' Run demo chat console application
#'
#' Run simle demo chat application in the console. If you do not want to enter
#' the api key, set the API_KEY environment variable.
#' @return unused
#' @export
#'
#' @examples
#' \dontrun{
#'   demoChatApp()
#' }
demoChatApp <- function() {
  if (!check_demo_req_pkgs("cli")) return()

  # ---- settings -----

  settings <- new.env()
  settings$model <- "gpt-3.5-turbo"
  settings$temperature <- 0.7
  settings$n <- 1
  settings$max_tokens <- 50
  settings$presence_penalty <- 0
  settings$frequency_penalty <- 0


  # ---- helpers ----

  display_help <- function() {
    cli::cat_line(cli::col_green("Available commands:"))
    cli::cat_line(" /quit or /exit - terminate program")
    cli::cat_line(" /help - print this information")
    cli::cat_line(" /model model_name")
    cli::cat_line(" /temperature temperature_value")
    cli::cat_line(" /n n_value")
    cli::cat_line(" /max_tokens max_tokens_value")
    cli::cat_line(" /presence_penalty presence_penalty_value")
    cli::cat_line(" /frequency_penalty frequency_penalty_value")
    cli::cat_line()
  }

  display_settings <- function() {
    n_size <- max(nchar(names(settings))) + 1
    fmt <- paste0(" %-", n_size, "s: %s")

    cli::cat_line(cli::col_green("Current settings:"))
    for (n in names(settings)) {
      cli::cat_line(sprintf(fmt, n, settings[[n]]))
    }
    cli::cat_line()
  }

  is_cmd <- function(txt) substr(txt, 1, 1) == "/"

  exec_cmd <- function(txt) {
    cmd <- unlist(strsplit(substring(txt, 2), "\\s+", perl = TRUE))
    switch(cmd[1],
      # display info
      help = display_help(),
      settings = display_settings(),

      # settings (integers)
      n =,
      max_tokens =
      {
        settings[[cmd[1]]] <- as.integer(cmd[2])
        display_settings()
      },
      # settings (floats)
      temperature =,
      presence_penalty =,
      frequency_penalty =
      {
        settings[[cmd[1]]] <- as.double(cmd[2])
        display_settings()
      },

      # default
      {
       cli::cat_line(cli::col_br_red("Unknown command '", cmd[1], "'"))
       display_help()
      }
    )
  }


  # ---- main ----

  # get OpenAI api key
  API_KEY <- Sys.getenv("API_KEY")
  while(!nchar(API_KEY)) {
    API_KEY <- readline("API_KEY: ")
  }
  cli::cat_line()

  # display help
  display_help()

  # start conversation
  dialog <- NULL
  repeat {
    # get text from user
    text <- ""
    while(!nchar(text)) {
      text <- readline(cli::col_blue("you: "))
    }

    # check if the text is a command, if so, process it
    if (is_cmd(text)) {
      if (text %in% c("/exit", "/quit")) return(invisible(NULL))
      exec_cmd(text)
      next
    }
    # create dialog data.frame from `text`
    dialog_user <- dialog_df(text)

    # send request
    log <- utils::capture.output({
      res_content <- chat_request(
        API_KEY,
        merge_dialog_df(dialog, dialog_user),
        model = settings$model,
        temperature = settings$temperature,
        n = settings$n,
        max_tokens = settings$max_tokens,
        presence_penalty = settings$presence_penalty,
        frequency_penalty = settings$frequency_penalty
      )
    }, type = "message")

    # process response
    if (is_error(res_content)) {
      for (n in seq_along(log)) {
        cli::cat_line(cli::col_red(log[n]))
      }
    }
    else {
      dialog_ai <- chat_fetch_messages(res_content)
      dialog <- merge_dialog_df(dialog, dialog_user, dialog_ai)
      for(n in seq(NROW(dialog_ai))) {
        cat(cli::col_blue("ai: "))
        cli::cat_line(dialog_ai[n, "content"])
      }
    }
  }
}
