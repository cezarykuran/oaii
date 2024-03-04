#' API request
#'
#' To get more details, visit https://platform.openai.com/docs/api-reference/making-requests
#' @inheritParams httr::POST
#' @param endpoint string, API endpoint url
#' @param api_key string, OpenAI API key
#' (see https://platform.openai.com/account/api-keys)
#' @param query NULL/list, query string elements as list(name1 = value1, name2 = value2)
#' @param method string, request method
#' @return \link[httr]{content} of the httr \link[httr]{response} object
#' or SimpleError (\link[base]{conditions}) enhanced with
#' two additional fields: `status_code` (response$status_code)
#' and `message_long` (built on response content)
#' @param content_class NULL/character vector, NULL or additional class
#' name(s) (S3) appended to the response content
#' @export
#'
request <- function(
    endpoint,
    api_key = api_get_key(),
    body = NULL,
    query = NULL,
    encode = "json",
    method = "POST",
    content_class = NULL
  ) {

  # asserts
  stopifnot(
    "`endpoint` must be a non-empty string" = checkmate::testString(endpoint, min.chars = 1),
    "`api_key` must be a non-empty string" = checkmate::testString(api_key, min.chars = 1),
    "`body` must be a NULL or list" = checkmate::testList(body, null.ok = TRUE),
    "`query` must be a NULL or list" = checkmate::testList(query, null.ok = TRUE),
    "`encode` must be a non-empty string" = checkmate::testString(encode, min.chars = 1),
    "`method` must be a non-empty string" = checkmate::testString(method, min.chars = 1),
    "`content_class` must be a NULL or character vector" =
        checkmate::testCharacter(content_class, min.chars = 1, null.ok = TRUE)
  )

  tryCatch(
    expr = {
      log_debug("request(endpoint='", endpoint, "', method='", method, "', ...)")
      res <- do.call(
        get(method, asNamespace("httr")),
        list(
          url = endpoint,
          config = httr::add_headers("Authorization" = paste("Bearer", api_key)),
          body = body,
          query = query,
          encode = encode
        )
      )

      # extract content
      content <- httr::content(res)

      stopifnot(
        "API endpoind responsed with code != 200" = httr::http_error(res) == FALSE
      )

      class(content) <- c(content_class, class(content))
      content
    },
    error = function(e) {
      # append oaii_res_se class
      class(e) <- c("oaii_res_se", class(e))

      # append status_code
      e$status_code <- if (exists("res")) res$status_code

      # append long error message
      e$message_long <- paste0(
        e$message,
        if (exists("content")) {
          if (is.null(content$error))
            paste0(", content: '", content, "'")
          else
            paste0(", recived error message: '", content$error$message, "'")
        }
      )

      # log error
      log_error(e$message_long)

      # return modified SimpleError "object"
      e
    }
  )
}
