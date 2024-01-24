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

#' API files: upload request
#'
#' Upload a file that can be used across various endpoints. The size of all the files uploaded by one organization can
#' be up to 100 GB. The size of individual files can be a maximum of 512 MB or 2 million tokens for Assistants. See the
#' Assistants Tools guide (https://platform.openai.com/docs/assistants/tools) to learn more about the types of files
#' supported. The Fine-tuning API only supports .jsonl files. To get more details, visit:
#' https://platform.openai.com/docs/api-reference/files/upload
#' @inherit files_roxygen_tpl params return
#' @inheritParams api_upload_file
#' @param file string/raw, path or content of the JSON Lines file to be uploaded
#' @param purpose string, the intended purpose of the uploaded documents. Use "fine-tune" for Fine-tuning.
#' @param file_type NULL/string, mime type of `file`. See \link{api_upload_file}
#' @export
#'
files_upload_request <- function(api_key, file, purpose, file_type = NULL) {
  # asserts
  stopifnot(
    "`purpose` must be a non-empty string" = checkmate::testString(purpose, min.chars = 1)
  )

  request(
    endpoint = "https://api.openai.com/v1/files",
    api_key = api_key,
    body = list(
      purpose = purpose,
      file = api_upload_file(file, file_type)
    ),
    encode = "multipart"
  )
}

#' API files: get list request
#'
#' Returns a list of files that belong to the user's organization. To get more details, visit:
#' https://platform.openai.com/docs/api-reference/files/list
#' @inherit files_roxygen_tpl params return
#' @param purpose NULL/string, only return files with the given purpose
#' @export
#'
#' @examples
#' \dontrun{
#' res_content <- files_list_request("my-secret-api-key-string")
#' if (!is_error(res_content)) {
#'   files_list_df <- files_fetch_list(res_content)
#'   print(files_list_df)
#' }
#' }
#'
files_list_request <- function(api_key, purpose = NULL) {
  request(
    endpoint = "https://api.openai.com/v1/files",
    api_key = api_key,
    method = "GET",
    query = list(purpose = purpose)
  )
}

#' Extract files list as data.frame from response object
#'
#' @inherit files_list_request examples
#' @param res_content response object returned by \link{files_list_request}
#' @return Files list as data.frame
#' @export
#'
files_fetch_list <- function(res_content) {
  files <- as.data.frame(do.call(rbind, res_content$data))
  class(files) <- c("oaii_files_df", class(files))
  files
}

#' print S3 method for oaii_files_df class
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_files_df <- function(x, ...) {
  x <- df_col_dt_format(x, "created_at", on_missing_col = "skip")
  NextMethod()
}

#' API files: retrieve file request
#'
#' Returns information about a specific file. To get more details, visit: 
#' https://platform.openai.com/docs/api-reference/files/retrieve
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

#' API files: delete file request
#'
#' Delete a file. To get more details, visit https://platform.openai.com/docs/api-reference/files/delete
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


#' API files: retrieve content request
#'
#' To get more details, visit https://platform.openai.com/docs/api-reference/files/retrieve-content
#' @inherit files_roxygen_tpl params return
#' @export
#' 
#' @examples
#' \dontrun{
#' res_content <- files_retrieve_content_request(
#'   "my-secret-api-key-string",
#'   "some-file-id"
#' )
#' if (!is_error(res_content)) {
#'   writeBin(res_content, "some-file.jsonl")
#' }
#' }
#'
files_retrieve_content_request <- function(api_key, file_id) {
  # asserts
  stopifnot(
    "`file_id` must be a non-empty string" =
        checkmate::testString(file_id, min.chars = 1)
  )

  request(
    endpoint = paste0("https://api.openai.com/v1/files/", file_id, "/content"),
    api_key = api_key,
    method = "GET"
  )
}
