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
    httr::upload_file(f_tmp)
  }
  else {
    httr::upload_file(f)
  }
}
