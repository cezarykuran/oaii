#' API models: roxygen template
#'
#' @inherit request params return
#' @keywords internal
#'
files_roxygen_tpl <- function(
    api_key
) NULL


#' API models: list request
#'
#' \url{https://platform.openai.com/docs/api-reference/models/list}
#' @inherit fine_tunes_roxygen_tpl params return
#' @export
#'
models_list_request <- function(api_key) {
  request(
    endpoint = "https://api.openai.com/v1/models",
    api_key = api_key,
    method = "GET"
  )
}

#' API models: delete request
#'
#' \url{https://platform.openai.com/docs/api-reference/fine-tunes/delete-model}
#' @inherit fine_tunes_roxygen_tpl params return
#' @param model string, the model to delete
#' @export
#'
models_delete_request <- function(api_key, model) {
  # asserts
  stopifnot(
    "`model` must be a non-empty string" = checkmate::testString(model, min.chars = 1),
  )

  request(
    endpoint = paste0("https://api.openai.com/v1/models/", model),
    api_key = api_key,
    method = "DELETE"
  )
}
