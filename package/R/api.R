#' API request
#'
#' \url{https://platform.openai.com/docs/api-reference/making-requests}
#' @inheritParams httr::POST
#' @param endpoint string, API endpoint url
#' @param api_key string, API key
#' @param method string, request method
#' @return content of the httr response object or SimpleError
#' @export
#'
request <- function(
    endpoint,
    api_key,
    body = NULL,
    encode = "json",
    method = "POST"
) {
  tryCatch(
    expr = {
      log_debug("request('", endpoint, "', ...)")
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
      # append status_code
      e$status_code <- if (exists("res")) res$status_code

      # append long error message
      e$message_long <- paste0(
        e$message,
        if (exists("content")) {
          if (is.null(content$error))
            paste0(", content: '", content, "'")
          else
            paste0(", api error message: '", content$error$message, "'")
        }
      )

      # log error
      log_error(e$message_long)

      # return error "object"
      e
    }
  )
}

#' Test if object belongs to "error" class
#'
#' @param x R variable
#' @export
#'
is_error <- function(x) {
  inherits(x, c("error", "simpleError"))
}
