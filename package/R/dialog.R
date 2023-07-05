#' Create dialog data.frame
#'
#' @param content string, message content
#' @param role string, message role ("owner")
#' @param finish_reason see \url{https://platform.openai.com/docs/guides/gpt/chat-completions-response-format}
#' @return a one-row data.frame with columns: content, role and finish_reason
#' @export
#'
#' @examples
#' dialog_df("some text message")
#' dialog_df("some another text message", role = "assistant")
#'
dialog_df <- function(content, role = "user", finish_reason = "stop") {
  # asserts
  stopifnot(
    "`content` must be a non-empty string" = checkmate::testString(content, min.chars = 1),
    "`role` must be a non-empty string" = checkmate::testString(role, min.chars = 1),
    "`finish_reason` must be a non-empty string" = checkmate::testString(finish_reason, min.chars = 1)
  )
  df <-
    data.frame(
      role = role,
      content = content,
      finish_reason = finish_reason
    )
  rownames(df) <- NULL
  df
}


#' Merge multiple dialog data.frame
#'
#' @seealso [dialog_df()]
#' @param ... dialog data.frame or NULL
#' @return data.frame containing all input dialogs
#' @export
#'
#' @examples
#' d1 <- dialog_df("message 1")
#' d2 <- dialog_df("message 2")
#' print(
#'   merge_dialog_df(
#'     d1,
#'     merge_dialog_df(d1, d2),
#'     NULL,
#'     d2
#'   )
#' )
#'
merge_dialog_df <- function(...) {
  df <-
    data.frame(
      role = character(0),
      content = character(0),
      finish_reason = character(0)
    )

  for (x in list(...)) {
    if (is.data.frame(x)) df <- rbind(df, x)
  }
  rownames(df) <- NULL
  df
}
