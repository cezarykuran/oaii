#' API chat: send completions request
#'
#' \url{https://platform.openai.com/docs/api-reference/completions}
#' @inherit request params return
#' @param messages list (messages "object"), a list of messages comprising
#' the conversation so far
#' @param model string, ID of the model to use.
#' See the \href{https://platform.openai.com/docs/models/model-endpoint-compatibility}{model endpoint compatibility table}
#' for details on which models work with the Chat API.
#' @param temperature double, what sampling temperature to use, between 0 and 2.
#' Higher values like 0.8 will make the output more random,
#' while lower values like 0.2 will make it more focused and deterministic.
#' @param n integer, how many chat completion choices
#' to generate for each input message.
#' @param max_tokens integer, the maximum number of tokens to generate
#' in the chat completion
#' @param presence_penalty double, number between -2.0 and 2.0.
#' Positive values penalize new tokens based on whether they appear in the text
#' so far, increasing the model's likelihood to talk about new topics.
#' @param frequency_penalty double, number between -2.0 and 2.0.
#' Positive values penalize new tokens based on their existing frequency
#' in the text so far, decreasing the model's likelihood to repeat the same line
#' verbatim.
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

#' Test if x is a chat message object
#'
#' @param x R object to test
#'
is_chat_message <- function(x) {
  req <- c("content", "role")
  is.list(x) &&
    all(names(x) %in% req) &&
    all(req %in% names(x))
}

#' Create chat message "object"
#'
#' @param content string, message content
#' @param role string, message role ("owner")
#' @export
#'
chat_message <- function(content, role = "user") {
  list(
    role = role,
    content = content
  )
}

#' Fetch messages from response content
#'
#' @param res_content response object
#' @export
#'
chat_fetch_messages <- function(res_content) {
  lapply(res_content$choices, function(l) l$message)
}

#' Merge chat messages
#'
#' @param ... chat message and/or messages objects
#' @export
#'
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
