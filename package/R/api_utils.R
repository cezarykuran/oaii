#' Create "uploaded file" object
#'
#' See \link[httr]{upload_file}
#' @inheritParams httr::upload_file
#' @param f string/raw, content of file or path to the file
#' @return NULL if `f` was NULL otherwise "uploaded_file" object
api_upload_file <- function(f, type = NULL) {
  if (is.null(f)) {
    NULL
  }
  else if (is.raw(f)) {
    f_tmp <- tempfile()
    writeBin(f, f_tmp)
    httr::upload_file(f_tmp, type)
  }
  else {
    httr::upload_file(f, type)
  }
}

#' Get the OpenAI API key from environment variable
#' 
#' @seealso [api_set_key()]
#' @return API key string
#' @export
#' 
api_get_key <- function() {
  api_key <- Sys.getenv("OAII_API_KEY")
  if (!nchar(api_key)) log_error("No key found in OAII_API_KEY system environment!")
  api_key
}

#' Store the OpenAI API key as environment variable
#' 
#' @seealso [api_get_key()]
#' @inheritParams request
#' @return api_key
#' @export
#' 
#' @examples
#' \dontrun{
#' api_set_key("my-secret-api-key-string")
#' }
#' 
api_set_key <- function(api_key) {
  # asserts
  stopifnot(
    "`api_key` must be a non-empty string" = checkmate::testString(api_key, min.chars = 1)
  )
  Sys.setenv(OAII_API_KEY = api_key)
}
