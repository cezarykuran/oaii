#' Send chat request
#' See \url{https://platform.openai.com/docs/api-reference/chat}
#'
#' @param api_key API endpoint parameter
#' @param messages API endpoint parameter
#' @param model API endpoint parameter
#' @param temperature API endpoint parameter
#' @param n API endpoint parameter
#' @param max_tokens API endpoint parameter
#' @param presence_penalty API endpoint parameter
#' @param frequency_penalty API endpoint parameter
#'
#' @export
#'
chat_request <- function(
    api_key,
    messages,
    model = "gpt-3.5-turbo",
    temperature = 0.7,
    n = 1,
    max_tokens = 50,
    presence_penalty = 0,
    frequency_penalty = 0
) {
  request(
    "https://api.openai.com/v1/chat/completions",
    api_key,
    body = list(
      model = model,
      messages = messages,
      temperature = temperature,
      n = n,
      max_tokens = max_tokens,
      presence_penalty = presence_penalty,
      frequency_penalty = frequency_penalty
    )
  )
}

is_chat_message <- function(m) {
  req <- c("content", "role")
  is.list(m) &&
    all(names(m) %in% req) &&
    all(req %in% names(m))
}

#' Create chat message "object"
#'
#' @param content string, message content
#' @param role string, message "owner"
#' @export
#'
chat_message <- function(content, role = "user") {
  list(
    role = role,
    content = content
  )
}

#' @export
chat_fetch_messages <- function(res_content) {
  lapply(res_content$choices, function(l) l$message)
}

#' @export
chat_merge_messages <- function(...) {
  messages <- NULL
  for (m in list(...)) {
    if (is.list(m)) {
      if (is_chat_message(m)) m <- list(m)
      messages <- c(messages, m)
    }
  }
  messages
}
