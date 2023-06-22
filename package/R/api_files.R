#' @export
files_list_request <- function(api_key) {
  request(
    endpoint = "https://api.openai.com/v1/files",
    api_key = api_key,
    method = "GET"
  )
}

#' @export
files_upload_request <- function(
  api_key,
  file,
  purpose
) {

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

#' @export
files_delete_request <- function(api_key, file_id) {
  request(
    endpoint = paste0("https://api.openai.com/v1/files/", file_id),
    api_key = api_key,
    method = "DELETE"
  )
}

#' @export
files_retrive_request <- function(api_key, file_id) {
  request(
    endpoint = paste0("https://api.openai.com/v1/files/", file_id),
    api_key = api_key,
    method = "GET"
  )
}

#' @export
files_retrive_content_request <- function(api_key, file_id) {
  request(
    endpoint = paste0("https://api.openai.com/v1/files/", file_id, "/content"),
    api_key = api_key,
    method = "GET"
  )
}
