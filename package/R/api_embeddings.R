#' API embeddings: create embeddings
#'
#' Creates an embedding vector representing the input text. To get more details, visit
#' https://platform.openai.com/docs/api-reference/embeddings/create
#' https://platform.openai.com/docs/guides/embeddings
#' @inherit request params return
#' @param input character vector, input text to embed, encoded as a string or array of tokens. To embed multiple inputs
#' in a single request, pass an array of strings or array of token arrays. The input must not exceed the max input
#' tokens for the model (8192 tokens for text-embedding-ada-002), cannot be an empty string, and any array must be 2048
#' dimensions or less.
#' @param model string, ID of the model to use. You can use the list models API
#' (https://platform.openai.com/docs/api-reference/models/list) to see all of your available models, or see our model
#' overview (https://platform.openai.com/docs/models/overview) for descriptions of them.
#' @param encoding_format string, he format to return the embeddings in. Can be either float (default) or base64.
#' @param user string, a unique identifier representing your end-user, which can help OpenAI to monitor and detect
#' abuse. To learn more visit https://platform.openai.com/docs/guides/safety-best-practices/end-user-ids
#' @export
#'
embeddings_create_request <- function(
    api_key,
    input,
    model,
    encoding_format = NULL,
    user = NULL
) {
  # asserts
  stopifnot(
    "`input` must be a non-empty character vector" =
        checkmate::testCharacter(input, min.chars = 1, min.len = 1),
    "`model` must be a non-empty string" =
        checkmate::testString(model, min.chars = 1),
    "`encoding_format` must be NULL or a non-empty string" =
        checkmate::testString(encoding_format, min.chars = 1, null.ok = TRUE),
    "`user` must be NULL or a non-empty string" =
        checkmate::testString(user, min.chars = 1, null.ok = TRUE)
  )

  request(
    endpoint = "https://api.openai.com/v1/embeddings",
    api_key = api_key,
    method = "POST",
    body = list(
      input = input,
      model = model,
      encoding_format = encoding_format,
      user = user
    )
  )
}

#' API embeddings: get embedding object
#'
#' Represents an embedding vector returned by embedding endpoint. To get more details, visit
#' https://platform.openai.com/docs/api-reference/embeddings/object
#' https://platform.openai.com/docs/guides/embeddings
#' @inherit request params return
#' @param index integer, The index of the embedding in the list of embeddings.
#' @param embedding double vector, the embedding vector, which is a "list of floats". The length of vector depends on
#' the model as listed in the embedding guide.
#' @param object string, the object type, which is always "embedding".
#' @export
#'
embeddings_object_request <- function(
    api_key,
    index,
    embedding,
    object
) {
  # asserts
  stopifnot(
    "`index` must be a double" = checkmate::testDouble(index, len = 1),
    "`embedding` must be a double vector" =
        checkmate::testDouble(embedding, min.len = 1),
    "`object` must a non-empty string" =
        checkmate::testString(object, min.chars = 1)
  )

  request(
    endpoint = "https://api.openai.com/v1/embeddings",
    api_key = api_key,
    method = "POST",
    body = list(
      index = index,
      embedding = embedding,
      object = object
    )
  )
}
