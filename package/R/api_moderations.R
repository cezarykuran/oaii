#' API moderations: create moderation
#'
#' Given a input text, outputs if the model classifies it as violating OpenAI's
#' content policy. To get more details, visit
#' https://platform.openai.com/docs/api-reference/moderations/create
#' https://platform.openai.com/docs/guides/moderation
#' @inherit request params return
#' @param input string, the input text to classify
#' @param model string, two content moderations models are available:
#' 'text-moderation-stable' and 'text-moderation-latest'. The default is
#' 'text-moderation-latest' which will be automatically upgraded over time. This
#' ensures you are always using our most accurate model. If you use
#' 'text-moderation-stable', we will provide advanced notice before updating the
#' model. Accuracy of 'text-moderation-stable' may be slightly lower than for
#' 'text-moderation-latest'.
#' @export
#'
moderation_create_request <- function(
    api_key,
    input,
    model = NULL
) {
  # asserts
  stopifnot(
    "`input` must be a non-empty string" = checkmate::testString(input),
    "`model` must be a non-empty string" =
      checkmate::testString(model, min.chars = 1, null.ok = TRUE)
  )

  request(
    endpoint = "https://api.openai.com/v1/moderations",
    api_key = api_key,
    method = "POST",
    body = list(
      input = input,
      model = model
    )
  )
}

#' API moderations: get moderation object
#'
#' Represents policy compliance report by OpenAI's content moderation model
#' against a given input. To get more details, visit
#' https://platform.openai.com/docs/api-reference/moderations/object
#' https://platform.openai.com/docs/guides/moderation
#' @inherit request params return
#' @param id string, The unique identifier for the moderation request
#' @param model string, The model used to generate the moderation results
#' @export
#'
moderations_object_request <- function(
    api_key,
    id,
    model
  ) {
  # asserts
  stopifnot(
    "`id` must be a non-empty string" = checkmate::testDouble(id, min.len = 1),
    "`model` must a non-empty string" =
        checkmate::testString(model, min.chars = 1)
  )

  request(
    endpoint = "https://api.openai.com/v1/embeddings",
    api_key = api_key,
    method = "POST",
    body = list(
      id = id,
      model = model
    )
  )
}
