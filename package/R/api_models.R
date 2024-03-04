#' API models: list request
#'
#' Lists the currently available models, and provides basic information about each one such as the owner and
#' availability. To get more details, visit: https://platform.openai.com/docs/models
#' https://platform.openai.com/docs/api-reference/models/list
#' @inherit request params return
#' @export
#'
#' @examples
#' \dontrun{
#' res_content <- models_list_request()
#' if (!is_error(res_content)) {
#'   models_list_df <- models_fetch_list(res_content)
#'   print(models_list_df)
#' }
#' }
#'
models_list_request <- function(api_key = api_get_key()) {
  request(
    endpoint = "https://api.openai.com/v1/models",
    api_key = api_key,
    method = "GET"
  )
}

#' Extract models from response object
#'
#' Extract models list as data.frame from response object
#' @inherit models_list_request examples
#' @param res_content response object returned by \link{models_list_request}
#' @return List of available models as data.frame
#' @export
#'
models_fetch_list <- function(res_content) {
  models <- as.data.frame(do.call(rbind, res_content$data))
  class(models) <- c("oaii_models_df", class(models))
  models
}

#' API models: retrieve model request
#'
#' Retrieves a model instance, providing basic information about the model such as the owner and permissioning.
#' To get more details, visit: https://platform.openai.com/docs/models
#' https://platform.openai.com/docs/api-reference/models/list
#' @inherit request params return
#' @param model string, the ID of the model to use for this request
#' @export
#'
models_retrieve_request <- function(
    model,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`model` must be a non-empty string" = checkmate::testString(model, min.chars = 1)
  )

  request(
    endpoint = paste0("https://api.openai.com/v1/models/", model),
    api_key = api_key,
    method = "GET"
  )
}


#' API models: delete model request
#'
#' Delete a fine-tuned model. You must have the Owner role in your organization to delete a model. To get more details,
#' visit https://platform.openai.com/docs/models
#' https://platform.openai.com/docs/api-reference/fine-tunes/delete-model
#' @inherit request params return
#' @param model string, the model to delete
#' @export
#'
models_delete_request <- function(
    model,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`model` must be a non-empty string" = checkmate::testString(model, min.chars = 1)
  )

  request(
    endpoint = paste0("https://api.openai.com/v1/models/", model),
    api_key = api_key,
    method = "DELETE"
  )
}
