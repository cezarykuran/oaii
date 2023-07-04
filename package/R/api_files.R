#' API files: roxygen template
#'
#' @inherit request params return
#' @param file_id string, id of the uploaded file
#' @keywords internal
#'
files_roxygen_tpl <- function(
  api_key,
  file_id
) NULL

#' API files: get list request
#'
#' \url{https://platform.openai.com/docs/api-reference/files/list}
#' @inherit files_roxygen_tpl params return
#' @export
#'
files_list_request <- function(api_key) {
  request(
    endpoint = "https://api.openai.com/v1/files",
    api_key = api_key,
    method = "GET"
  )
}

#' API files: upload request
#'
#' \url{https://platform.openai.com/docs/api-reference/files/upload}
#' @inherit files_roxygen_tpl params return
#' @param file string, path or content of the JSON Lines file to be uploaded
#' @param purpose string, the intended purpose of the uploaded documents.
#' Use "fine-tune" for Fine-tuning.
#' @export
#'
files_upload_request <- function(api_key, file, purpose) {
  # asserts
  stopifnot(
    "`file` must be a fle path or raw content" =
      checkmate::testFileExists(file) || checkmate::testRaw(file),
    "`purpose` must be a non-empty string" = checkmate::testString(purpose, min.chars = 1)
  )

  files <- c()
  on.exit(unlink(files))

  request(
    endpoint = "https://api.openai.com/v1/files",
    api_key = api_key,
    body = list(
      purpose = purpose,
      file =
        if (file.exists(file)) httr::upload_file(file)
        else {
          files["file"] <- tempfile()
          writeBin(file, files["file"])
          httr::upload_file(files["file"])
        }
    ),
    encode = "multipart"
  )
}

#' API files: delete request
#'
#' \url{https://platform.openai.com/docs/api-reference/files/delete}
#' @inherit files_roxygen_tpl params return
#' @export
#'
files_delete_request <- function(api_key, file_id) {
  # asserts
  stopifnot(
    "`file_id` must be a non-empty string" = checkmate::testString(file_id, min.chars = 1)
  )

  request(
    endpoint = paste0("https://api.openai.com/v1/files/", file_id),
    api_key = api_key,
    method = "DELETE"
  )
}

#' API files: retrieve request
#'
#' \url{https://platform.openai.com/docs/api-reference/files/retrieve}
#' @inherit files_roxygen_tpl params return
#' @export
#'
files_retrieve_request <- function(api_key, file_id) {
  # asserts
  stopifnot(
    "`file_id` must be a non-empty string" = checkmate::testString(file_id, min.chars = 1)
  )

  request(
    endpoint = paste0("https://api.openai.com/v1/files/", file_id),
    api_key = api_key,
    method = "GET"
  )
}

#' API files: retrieve content request
#'
#' \url{https://platform.openai.com/docs/api-reference/files/retrieve-content}
#' @inherit files_roxygen_tpl params return
#' @export
#'
files_retrieve_content_request <- function(api_key, file_id) {
  # asserts
  stopifnot(
    "`file_id` must be a non-empty string" = checkmate::testString(file_id, min.chars = 1)
  )

  request(
    endpoint = paste0("https://api.openai.com/v1/files/", file_id, "/content"),
    api_key = api_key,
    method = "GET"
  )
}
