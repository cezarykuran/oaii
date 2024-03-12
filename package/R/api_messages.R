#' API messages: create message
#'
#' Create a message. To get more details, visit
#' https://platform.openai.com/docs/api-reference/messages/createMessage
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread to create a message for.
#' @param role string, the role of the entity that is creating the message. Currently only user is supported.
#' @param content string, the content of the message.
#' @param file_ids NULL/character vector, a list of File IDs that the message should use. There can be a maximum of 10
#' files attached to a message. Useful for tools like retrieval and code_interpreter that can access and use files.
#' @param metadata NULL/list, set of 16 key-value pairs that can be attached to an object. This can be useful for
#' storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long
#' and values can be a maxium of 512 characters long.
#' @export
#' 
messages_create_message_request <- function(
    thread_id,
    role,
    content,
    file_ids = NULL,
    metadata = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`role` must be a non-empty string" = checkmate::testString(role, min.chars = 1),
    "`content` must be a non-empty string" = checkmate::testString(content, min.chars = 1),
    "`file_ids` must be a NULL or non-empty character vector" =
        checkmate::testCharacter(file_ids, min.len = 1, min.chars = 1, null.ok = TRUE),
    "`metadata` must be a NULL or list" = checkmate::testList(metadata, min.len = 1, null.ok = TRUE)
  )

  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/messages"),
    api_key,
    body = list(
      role = role,
      content = content,
      file_ids = file_ids,
      metadata = metadata
    )
  )
}

#' API messages: list messages
#'
#' Returns a list of messages for a given thread. To get more details, visit
#' https://platform.openai.com/docs/api-reference/messages/listMessages
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread the messages belong to
#' @param limit NULL/integer, a limit on the number of objects to be returned. Limit can range between 1 and 100,
#' and the default is 20.
#' @param order NULL/string, sort order by the created_at timestamp of the objects. asc for ascending order and desc for
#' descending order. Defaults to desc
#' @param after NULL/string, a cursor for use in pagination. after is an object ID that defines your place in the list.
#' For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can
#' include after=obj_foo in order to fetch the next page of the list.
#' @param before NULL/string, a cursor for use in pagination. before is an object ID that defines your place in the
#' list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can
#' include before=obj_foo in order to fetch the previous page of the list.
#' @export
#' 
messages_list_messages_request <- function(
    thread_id,
    limit = NULL,
    order = NULL,
    after = NULL,
    before = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`limit` must be a NULL or non-empty integer" = checkmate::testInt(limit, null.ok = TRUE),
    "`order` must be a NULL or non-empty string" = checkmate::testString(order, min.chars = 1, null.ok = TRUE),
    "`after` must be a NULL or non-empty string" = checkmate::testString(after, min.chars = 1, null.ok = TRUE),
    "`before` must be a NULL or non-empty string" = checkmate::testString(before, min.chars = 1, null.ok = TRUE)
  )

  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/messages"),
    api_key,
    method = "GET",
    query = list(
      limit = limit,
      order = order,
      after = after,
      before = before
    )
  )
}

#' API messages: list message files
#'
#' Returns a list of message files. To get more details, visit
#' https://platform.openai.com/docs/api-reference/messages/listMessageFiles
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread the messages belong to
#' @param message_id string, the ID of the message that the files belongs to
#' @param limit NULL/integer, a limit on the number of objects to be returned. Limit can range between 1 and 100,
#' and the default is 20.
#' @param order NULL/string, sort order by the created_at timestamp of the objects. asc for ascending order and desc for
#' descending order. Defaults to desc
#' @param after NULL/string, a cursor for use in pagination. after is an object ID that defines your place in the list.
#' For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can
#' include after=obj_foo in order to fetch the next page of the list.
#' @param before NULL/string, a cursor for use in pagination. before is an object ID that defines your place in the
#' list. For instance, if you make a list request and receive 100 objects, ending with obj_foo, your subsequent call can
#' include before=obj_foo in order to fetch the previous page of the list.
#' @export
#' 
messages_list_message_files_request <- function(
    thread_id,
    message_id,
    limit = NULL,
    order = NULL,
    after = NULL,
    before = NULL,
    api_key = api_get_key()
) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`message_id` must be a non-empty string" = checkmate::testString(message_id, min.chars = 1),
    "`limit` must be a NULL or non-empty integer" = checkmate::testInt(limit, null.ok = TRUE),
    "`order` must be a NULL or non-empty string" = checkmate::testString(order, min.chars = 1, null.ok = TRUE),
    "`after` must be a NULL or non-empty string" = checkmate::testString(after, min.chars = 1, null.ok = TRUE),
    "`before` must be a NULL or non-empty string" = checkmate::testString(before, min.chars = 1, null.ok = TRUE)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/messages/", message_id, "/files"),
    api_key,
    method = "GET",
    query = list(
      limit = limit,
      order = order,
      after = after,
      before = before
    )
  )
}

#' API messages: retrieve message
#'
#' Retrieve a message. To get more details, visit
#' https://platform.openai.com/docs/api-reference/messages/getMessage
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread the messages belong to
#' @param message_id string, the ID of the message that the files belongs to
#' @export
#' 
messages_retrieve_message_request <- function(
    thread_id,
    message_id,
    api_key = api_get_key()
) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`message_id` must be a non-empty string" = checkmate::testString(message_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/messages/", message_id),
    api_key,
    method = "GET"
  )
}

#' API messages: retrieve message file
#'
#' Retrieve a message file. To get more details, visit
#' https://platform.openai.com/docs/api-reference/messages/getMessageFile
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread the messages belong to
#' @param message_id string, the ID of the message that the files belongs to
#' @param file_id string, the ID of the file being retrieved
#' @export
#' 
messages_retrieve_message_file_request <- function(
    thread_id,
    message_id,
    file_id,
    api_key = api_get_key()
) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`message_id` must be a non-empty string" = checkmate::testString(message_id, min.chars = 1),
    "`file_id` must be a non-empty string" = checkmate::testString(file_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/messages/", message_id, "/files/", file_id),
    api_key,
    method = "GET"
  )
}

#' API messages: modify message
#'
#' Modifies a message. To get more details, visit
#' https://platform.openai.com/docs/api-reference/messages/modifyMessage
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread to which this message belongs
#' @param message_id string, the ID of the message to modify
#' @param metadata NULL/list, set of 16 key-value pairs that can be attached to an object. This can be useful for
#' storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long
#' and values can be a maximum of 512 characters long.
#' @export
#' 
messages_modify_message_request <- function(
    thread_id,
    message_id,
    metadata = NULL,
    api_key = api_get_key()
) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`message_id` must be a non-empty string" = checkmate::testString(message_id, min.chars = 1),
    "`metadata` must be a NULL or list" = checkmate::testList(metadata, min.len = 1, null.ok = TRUE)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/messages/", message_id),
    api_key,
    body = list(
      metadata = metadata
    )
  )
}
