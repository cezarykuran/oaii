#' API completions: roxygen template
#'
#' @inherit request params return
#' @keywords internal
#'
files_roxygen_tpl <- function(
    api_key
) NULL

#' API completions: create request
#'
#' \url{https://platform.openai.com/docs/api-reference/completions/create}
#' @inherit files_roxygen_tpl params return
#' @param prompt API endpoint parameter
#' @param model string, ID of the model to use. You can use the
#' \href{https://platform.openai.com/docs/api-reference/models/list}{List models}
#' API to see all of your available models, or see our
#' \href{https://platform.openai.com/docs/models/overview}{Model overview}
#' for descriptions of them.API endpoint parameter
#' @param suffix string/NULL, the suffix that comes after a completion
#' of inserted text.
#' @param max_tokens integer, the maximum number of
#' \href{https://platform.openai.com/tokenizer}{tokens} to generate
#' in the completion. The token count of your prompt plus max_tokens cannot
#' exceed the model's context length.
#' @param temperature double, what sampling temperature to use, between 0 and 2.
#' Higher values like 0.8 will make the output more random,
#' while lower values like 0.2 will make it more focused and deterministic.
#' @param n integer, How many completions to generate for each prompt.
#' Note: Because this parameter generates many completions,
#' it can quickly consume your token quota. Use carefully
#' and ensure that you have reasonable settings for `max_tokens` and `stop`.
#' @param echo logical, echo back the prompt in addition to the completion
#' @param presence_penalty double, Number between -2.0 and 2.0.
#' Positive values penalize new tokens based on whether they appear in the text
#' so far, increasing the model's likelihood to talk about new topics.
#' @param frequency_penalty double, Number between -2.0 and 2.0.
#' Positive values penalize new tokens based on their existing frequency
#' in the text so far, decreasing the model's likelihood to repeat the same line
#' verbatim.
#' @export
#'
completions_create_request <- function(
    api_key,
    prompt,
    model = "gpt-3.5-turbo",
    suffix = NULL,
    max_tokens = 50,
    temperature = 0.7,
    #top_p = 1,
    n = 1,
    #stream = FALSE,
    #logprobs = NULL,
    echo = FALSE,
    #stop = NULL,
    presence_penalty = 0,
    frequency_penalty = 0
    #best_of = 1,
    #logit_bias = NULL,
    #user =
) {
  request(
    "https://api.openai.com/v1/completions",
    api_key,
    body = list(
      model = model,
      prompt = prompt,
      temperature = temperature,
      n = n,
      echo = echo,
      max_tokens = max_tokens,
      presence_penalty = presence_penalty,
      frequency_penalty = frequency_penalty
    )
  )
}

#' Fetch messages from response content
#'
#' @param res_content response object
#' @export
#'
completions_fetch_text <- function(res_content) {
  lapply(res_content$choices, function(l) l$text)
}

#' Create completion message "object"
#'
#' @param content string, message content
#' @param role string, message "owner"
#' @export
#'
completion_message <- function(content, role = "user") {
  list(
    role = role,
    content = content
  )
}
