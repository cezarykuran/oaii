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
#' @examples
#' \dontrun{
#' res_content <- models_list_request("my-secret-api-key-string")
#' if (!is_error(res_content)) {
#'   models_list_df <- models_fetch_list(res_content)
#'   print(models_list_df)
#' }
#' }
#'
models_list_request <- function(api_key) {
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
  class(models) <- c("oaiiModelsDF", class(models))
  models
}

#' print S3 method for oaiiModelsDF class
#'
#' @inherit base::print description params return
#' @export
#'
print.oaiiModelsDF <- function(x, ...) {
  x %>%
    df_col_dt_format(
      "created",
      on_missing_col = "skip"
    ) %>%
    df_col_obj_implode(
      "permission",
      props_glue = ", ",
      on_missing_col = "skip"
    ) -> x
  NextMethod()
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
    "`model` must be a non-empty string" = checkmate::testString(model, min.chars = 1)
  )

  request(
    endpoint = paste0("https://api.openai.com/v1/models/", model),
    api_key = api_key,
    method = "DELETE"
  )
}
