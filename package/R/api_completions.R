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
#' @param top_p double, an alternative to sampling with temperature,
#' called nucleus sampling, where the model considers the results of the tokens
#' with top_p probability mass. So 0.1 means only the tokens comprising
#' the top 10\% probability mass are considered.
#' @param n integer, How many completions to generate for each prompt.
#' Note: Because this parameter generates many completions,
#' it can quickly consume your token quota. Use carefully
#' and ensure that you have reasonable settings for `max_tokens` and `stop`.
#' @param stream flag, Whether to stream back partial progress. If set,
#' tokens will be sent as data-only server-sent events as they become available,
#' with the stream terminated by a data: `[DONE]` message.
#' @param logprobs integer, Include the log probabilities on the logprobs most
#' likely tokens, as well the chosen tokens. For example, if logprobs is 5,
#' the API will return a list of the 5 most likely tokens. The API will always
#' return the logprob of the sampled token, so there may be up to logprobs+1
#' elements in the response.
#' @param echo logical, echo back the prompt in addition to the completion
#' @param stop string or array, up to 4 sequences where the API will stop
#' generating further tokens. The returned text will not contain the stop sequence.
#' @param presence_penalty double, Number between -2.0 and 2.0.
#' Positive values penalize new tokens based on whether they appear in the text
#' so far, increasing the model's likelihood to talk about new topics.
#' @param frequency_penalty double, Number between -2.0 and 2.0.
#' Positive values penalize new tokens based on their existing frequency
#' in the text so far, decreasing the model's likelihood to repeat the same line
#' verbatim.
#' @param best_of integer, Generates best_of completions server-side and returns
#' the "best" (the one with the highest log probability per token). Results
#' cannot be streamed.
#' When used with n, best_of controls the number of candidate completions
#' and n specifies how many to return – best_of must be greater than n.
#' @param user string, A unique identifier representing your end-user,
#' which can help OpenAI to monitor and detect abuse
#' @export
#'
#' @examples
#' \dontrun{
#'   prompt <- "x=1, y=2, z=x*y, z=?"
#'   res_content <- completions_request(
#'     api_key = "my-secret-api-key-string",
#'     model = "text-davinci-003",
#'     prompt = prompt
#'   )
#'   if (!is_error(res_content)) {
#'     answer <- completions_fetch_text(res_content)
#'     print(answer)
#'   }
#' }
#'
completions_request <- function(
    api_key,
    model,
    prompt,
    suffix = NULL,
    max_tokens = NULL,
    temperature = NULL,
    top_p = NULL,
    n = NULL,
    stream = NULL,
    logprobs = NULL,
    echo = NULL,
    stop = NULL,
    presence_penalty = NULL,
    frequency_penalty = NULL,
    best_of = NULL,
    #logit_bias = NULL,
    user = NULL
) {
  # asserts
  stopifnot(
    "`model` must be a non-empty string" = checkmate::testString(model, min.chars = 1),
    "`prompt` must be a non-empty character vector" =
        checkmate::testCharacter(prompt, min.len = 1, min.chars = 1),
    "`suffix` must be a NULL or non-empty string"= checkmate::testString(suffix, min.chars = 1, null.ok = TRUE),
    "`max_tokens` must be a NULL or integer" = checkmate::testInt(max_tokens, null.ok = TRUE),
    "`temperature` must be a NULL or double" = checkmate::testDouble(temperature, null.ok = TRUE),
    "`top_p` must be a NULL or double" = checkmate::testDouble(top_p, null.ok = TRUE),
    "`n` must be a NULL or integer" = checkmate::testInt(n, null.ok = TRUE),
    "`stream` must be a NULL or flag" = checkmate::testFlag(stream, null.ok = TRUE),
    "`logprobs` must be a NULL or integer" = checkmate::testInt(logprobs, null.ok = TRUE),
    "`echo` must be a NULL or flag" = checkmate::testFlag(echo, null.ok = TRUE),
    "`stop` must be a NULL, string or array" =
        checkmate::testString(stop, null.ok = TRUE) || checkmate::testArray(stop, null.ok = TRUE),
    "`presence_penalty` must be a double" = checkmate::testDouble(presence_penalty, null.ok = TRUE),
    "`frequency_penalty` must be a double" = checkmate::testDouble(frequency_penalty, null.ok = TRUE),
    "`best_of` must be a NULL or integer" = checkmate::testInt(best_of, null.ok = TRUE),
    # logit_bias
    "`user` must be a NULL or non-empty string"= checkmate::testString(user, min.chars = 1, null.ok = TRUE)
  )

  request(
    "https://api.openai.com/v1/completions",
    api_key,
    body = list(
      model = model,
      prompt = prompt,
      suffix = suffix,
      max_tokens = max_tokens,
      temperature = temperature,
      top_p = top_p,
      n = n,
      stream = stream,
      logprobs = logprobs,
      echo = echo,
      stop = stop,
      presence_penalty = presence_penalty,
      frequency_penalty = frequency_penalty,
      best_of = best_of,
      #logit_bias = logit_bias,
      user = user
    )
  )
}

#' Fetch completions text from response content
#'
#' Fetch completions text from response content (\link{completions_request})
#' as dialog data.frame
#' @inherit completions_request examples
#' @param res_content response object returned by \link{completions_request}
#' @param role string, dialog role (phrase owner)
#' @param ltrim flag, trim left white space character(s) from text
#' @return dialog data.frame
#' @export
#'
completions_fetch_text <- function(res_content, role = "ai", ltrim = TRUE) {
  # asserts
  stopifnot(
    "`role` must be a non-empty string" = checkmate::testString(role, min.chars = 1),
    "`ltrim` must be a flag" = checkmate::testFlag(ltrim)
  )

  do.call(merge_dialog_df, lapply(res_content$choices, function(choice) {
    dialog_df(
      role = role,
      content =
        if (ltrim) sub("^[\\s]+", "", choice$text, perl = TRUE)
        else choice$text,
      finish_reason = choice$finish_reason
    )
  }))
}
