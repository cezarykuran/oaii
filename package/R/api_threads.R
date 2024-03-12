#' API threads: create thread
#'
#' Create threads that assistants can interact with. To get more details, visit
#' https://platform.openai.com/docs/api-reference/threads/createThread
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param messages NULL/list, a list of messages to start the thread with. The message "object" description:
#' list(
#'   list(
#'     # string (required), the role of the entity that is creating the message. Currently only user is supported.
#'     role = 
#'     # string (required), the content of the message.
#'     content = 
#'     # character vector (optional), a list of File IDs that the message should use. There can be a maximum of 10 files
#'     # attached to a message. Useful for tools like retrieval and code_interpreter that can access and use files.
#'     file_ids = 
#'     # named list (optional), set of 16 key-value pairs that can be attached to an object. This can be useful for
#'     storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long
#'     and values can be a maximum of 512 characters long.
#'     metadata = list(
#'       meta1 = "value 2"
#'     )
#'   )
#' )
#' @param metadata NULL/list, set of 16 key-value pairs that can be attached to an object. This can be useful for
#' storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long
#' and values can be a maximum of 512 characters long.
#' @export
#' 
threads_create_thread_request <- function(
    messages = NULL,
    metadata = NULL,
    api_key = api_get_key()
) {
  
  # asserts
  stopifnot(
    "`messages` must be a NULL or list" = checkmate::testList(messages, min.len = 1, null.ok = TRUE),
    "`metadata` must be a NULL or list" = checkmate::testList(metadata, min.len = 1, null.ok = TRUE)
  )
  
  request(
    "https://api.openai.com/v1/threads",
    api_key,
    body = list(
      messages = messages,
      metadata = metadata
    )
  )
}

#' API threads: retrieve thread
#'
#' Retrieves a thread. To get more details, visit
#' https://platform.openai.com/docs/api-reference/threads/getThread
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread to retrieve.
#' @export
#' 
threads_retrieve_thread_request <- function(
    thread_id,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id),
    api_key,
    method = "GET"
  )
}

#' API threads: modify thread
#'
#' Modifies a thread. To get more details, visit
#' https://platform.openai.com/docs/api-reference/threads/modifyThread
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread to modify. Only the `metadata` can be modified.
#' @param metadata NULL/list, set of 16 key-value pairs that can be attached to an object. This can be useful for
#' storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long
#' and values can be a maximum of 512 characters long.
#' @export
#' 
threads_modify_thread_request <- function(
    thread_id,
    metadata = NULL,
    api_key = api_get_key()
) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`metadata` must be a NULL or list" = checkmate::testList(metadata, min.len = 1, null.ok = TRUE)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id),
    api_key,
    body = list(
      metadata = metadata
    )
  )
}

#' API threads: delete thread
#'
#' Delete a thread. To get more details, visit
#' https://platform.openai.com/docs/api-reference/threads/deleteThread
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread to delete
#' @export
#' 
threads_delete_thread_request <- function(
    thread_id,
    api_key = api_get_key()
) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id),
    api_key,
    method = "DELETE"
  )
}
