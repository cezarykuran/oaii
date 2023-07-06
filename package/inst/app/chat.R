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
    quit =, exit = quit(),
    help = {
      display_help()
      return()
    },
    settings = {
      display_settings()
      return()
    },
    n = {
      settings$n <- as.integer(cmd[2])
    },
    temperature = {
      settings$temperature <- as.double(cmd[2])
    },
    max_tokens = {
      settings$max_tokens <- as.integer(cmd[2])
    },
    presence_penalty = {
      settings$presence_penalty <- as.double(cmd[2])
    },
    frequency_penalty = {
      settings$frequency_penalty <- as.double(cmd[2])
    },
    # default
    {
     cli::cat_line(cli::col_br_red("Unknown command '", cmd[1], "'"))
     display_help()
     return()
    }
  )
  display_settings()
}


# ---- main ----

# prepare stdin
cstdin <- file("stdin")
open(cstdin)
on.exit({
  close(cstdin)
})

# get OpenAI api key
API_KEY <- Sys.getenv("API_KEY")
while(!nchar(API_KEY)) {
  cat("API_KEY: ")
  API_KEY <- readLines(cstdin, 1)
}

# display help
display_help()

# start conversation
dialog <- NULL
repeat {
  cat(cli::col_blue("you: "))
  q_str <- readLines(cstdin, 1)

  if (is_cmd(q_str)) {
    exec_cmd(q_str)
    next
  }

  dialog_q <- oaii::dialog_df(q_str)
  log <- capture.output({
    res_content <-
      oaii::chat_request(
        API_KEY,
        oaii::merge_dialog_df(dialog, dialog_q),
        model = settings$model,
        temperature = settings$temperature,
        n = settings$n,
        max_tokens = settings$max_tokens,
        presence_penalty = settings$presence_penalty,
        frequency_penalty = settings$frequency_penalty
      )
  }, type = "message")
  if (oaii::is_error(res_content)) {
    for (n in seq_along(log)) {
      cli::cat_line(cli::col_red(log[n]))
    }
  }
  else {
    dialog_a <- oaii::chat_fetch_messages(res_content)
    dialog <- oaii::merge_dialog_df(dialog, dialog_q, dialog_a)
    for(n in seq(NROW(dialog_a))) {
      cat(cli::col_blue("ai: "))
      cli::cat_line(dialog_a[n, "content"])
    }
  }
}
