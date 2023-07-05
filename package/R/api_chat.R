#' API chat: send create (chat) request
#'
#' \url{https://platform.openai.com/docs/api-reference/chat/create}
#' @inherit request params return
#' @param messages data.frame, data.frame with messages comprising the conversation so far
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
#' @examples
#' \dontrun{
#'   question <- dialog_df("hi")
#'   res_content <- chat_request(
#'     api_key = "my-secret-api-key-string",
#'     messages = question
#'   )
#'   if (!is_error(res_content)) {
#'     answer <- chat_fetch_messages(res_content)
#'     conversation <- merge_dialog_df(question, answer)
#'     print(conversation)
#'   }
#' }
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
  # asserts
  stopifnot(
    "`messages` must be a data.frame" = checkmate::testDataFrame(messages),
    "`model` must be a non-empty string" = checkmate::testString(model, min.chars = 1),
    "`temperature` must be a double" = checkmate::testDouble(temperature),
    "`n` must be a integer" = checkmate::testInt(n),
    "`max_tokens` must be a integer" = checkmate::testInt(max_tokens),
    "`presence_penalty` must be a double" = checkmate::testDouble(presence_penalty),
    "`frequency_penalty` must be a double" = checkmate::testDouble(frequency_penalty)
  )

  request(
    "https://api.openai.com/v1/chat/completions",
    api_key,
    body = list(
      model = model,
      messages = messages[, c("role", "content")],
      temperature = temperature,
      n = n,
      max_tokens = max_tokens,
      presence_penalty = presence_penalty,
      frequency_penalty = frequency_penalty
    )
  )
}

#' Fetch messages from response content
#'
#' Fetch messages (dialog data.frame with chat messages) from response content
#' @inherit chat_request examples
#' @param res_content response object returned by \link{chat_request}
#' @return Messages from response as dialog data.frame (see \link{dialog_df})
#' @export
#'
chat_fetch_messages <- function(res_content) {
  do.call(merge_dialog_df, lapply(res_content$choices, function(choice) {
    dialog_df(
      content = choice$message$content,
      role = choice$message$role,
      finish_reason = choice$finish_reason
    )
  }))
}
