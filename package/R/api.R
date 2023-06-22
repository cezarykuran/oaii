
#' API request
#'
#' @param endpoint string, API endpoint
#' @param api_key string, API key
#' @param body NULL/list, request body
#' @param encode request encode
#' @param method request method
#'
request <- function(endpoint, api_key, body = NULL, encode = "json", method = "POST") {
  tryCatch(
    expr = {
      res <- do.call(
        get(method, asNamespace("httr")),
        list(
          url = endpoint,
          config = httr::add_headers("Authorization" = paste("Bearer", api_key)),
          body = body,
          encode = encode
        )
      )

      content <- httr::content(res)

      stopifnot(
        "API endpoind responsed with code != 200" = httr::http_error(res) == FALSE
      )

      content
    },
    error = function(e) {
      e$message_long <- paste0(
        e$message,
        if (exists("content")) {
          if (is.null(content$error))
            paste0(", content: '", content, "'")
          else
            paste0(", api error message: '", content$error$message, "'")
        }
      )
      log_error(e$message_long)
      e
    }
  )
}

#' @export
is_error <- function(x) {
  inherits(x, c("error", "simpleError"))
}
