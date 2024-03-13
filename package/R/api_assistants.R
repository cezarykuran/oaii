#' API assistants: create assistant
#'
#' Create an assistant with a model and instructions. To get more details, visit
#' https://platform.openai.com/docs/api-reference/assistants/createAssistant
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param model string, ID of the model to use. You can use the List models API to see all of your available models,
#' or see our model overview for descriptions of them.
#' @param name NULL/string, the name of the assistant. The maximum length is 256 characters.
#' @param description NULL/string, the description of the assistant. The maximum length is 512 characters.
#' @param instructions NULL/string, the system instructions that the assistant uses. The maximum length is 32768
#' characters.
#' @param tools NULL/list, a list of tool enabled on the assistant. There can be a maximum of 128 tools per
#' assistant. Tools can be of types code_interpreter, retrieval, or function.
#' @param file_ids NULL/character vector, a list of file IDs attached to this assistant. There can be a maximum of 20
#' files attached to the assistant. Files are ordered by their creation date in ascending order.
#' @param metadata NULL/list, set of 16 key-value pairs that can be attached to an object. This can be useful for
#' storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long
#' and values can be a maxium of 512 characters long.
#' @export
#' 
assistants_create_assistant_request <- function(
    model,
    name = NULL,
    description = NULL,
    instructions = NULL,
    tools = NULL,
    file_ids = NULL,
    metadata = NULL,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`model` must be a non-empty string" = checkmate::testString(model, min.chars = 1),
    "`name` must be a NULL or non-empty string" = checkmate::testString(name, min.chars = 1, null.ok = TRUE),
    "`description` must be a NULL or non-empty string" =
        checkmate::testString(description, min.chars = 1, null.ok = TRUE),
    "`instructions` must be a NULL or non-empty string" =
        checkmate::testString(instructions, min.chars = 1, null.ok = TRUE),
    "`tools` must be a NULL or non-empty list" = checkmate::testList(tools, min.len = 1, null.ok = TRUE),
    "`file_ids` must be a NULL or non-empty character vector" =
        checkmate::testCharacter(file_ids, min.len = 1, min.chars = 1, null.ok = TRUE),
    "`metadata` must be a NULL or list" = checkmate::testList(metadata, min.len = 1, null.ok = TRUE)
  )

  request(
    "https://api.openai.com/v1/assistants",
    api_key,
    body = list(
      model = model,
      name = name,
      description = description,
      tools = tools,
      file_ids = file_ids,
      metadata = metadata
    )
  )
}

#' API assistants: create assistant file
#'
#' Create an assistant file by attaching a file (https://platform.openai.com/docs/api-reference/files) to an assistant
#' (https://platform.openai.com/docs/api-reference/assistants). To get more details, visit
#' https://platform.openai.com/docs/api-reference/assistants/createAssistantFile
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param assistant_id string, the ID of the assistant for which to create a File.
#' @param file_id string, a file ID (with purpose="assistants") that the assistant should use. Useful for tools like
#' retrieval and code_interpreter that can access files.
#' @export
#' 
assistants_create_file_request <- function(
    assistant_id,
    file_id,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`assistant_id` must be a non-empty string" = checkmate::testString(assistant_id, min.chars = 1),
    "`file_id` must be a non-empty string" = checkmate::testString(file_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/assistants/", assistant_id, "/files"),
    api_key,
    body = list(
      file_id = file_id
    )
  )
}

#' API assistants: list assistants
#'
#' Returns a list of assistants. To get more details, visit
#' https://platform.openai.com/docs/api-reference/assistants/listAssistants
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
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
assistants_list_asistants_request <- function(
    limit = NULL,
    order = NULL,
    after = NULL,
    before = NULL,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`limit` must be a NULL or non-empty integer" = checkmate::testInt(limit, null.ok = TRUE),
    "`order` must be a NULL or non-empty string" = checkmate::testString(order, min.chars = 1, null.ok = TRUE),
    "`after` must be a NULL or non-empty string" = checkmate::testString(after, min.chars = 1, null.ok = TRUE),
    "`before` must be a NULL or non-empty string" = checkmate::testString(before, min.chars = 1, null.ok = TRUE)
  )
  
  request(
    "https://api.openai.com/v1/assistants",
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

#' API assistants: list assistant files
#'
#' Returns a list of assistant files. To get more details, visit
#' https://platform.openai.com/docs/api-reference/assistants/listAssistantFiles
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param assistant_id string, the ID of the assistant the file belongs to.
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
assistants_list_asistants_request <- function(
    assistant_id,
    limit = NULL,
    order = NULL,
    after = NULL,
    before = NULL,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`assistant_id` must be a non-empty string" = checkmate::testString(assistant_id, min.chars = 1),
    "`limit` must be a NULL or non-empty integer" = checkmate::testInt(limit, null.ok = TRUE),
    "`order` must be a NULL or non-empty string" = checkmate::testString(order, min.chars = 1, null.ok = TRUE),
    "`after` must be a NULL or non-empty string" = checkmate::testString(after, min.chars = 1, null.ok = TRUE),
    "`before` must be a NULL or non-empty string" = checkmate::testString(before, min.chars = 1, null.ok = TRUE)
  )
  
  request(
    paste0("https://api.openai.com/v1/assistants/", assistant_id,"/files"),
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

#' API assistants: retrieve assistant
#'
#' Retrieves an assistant. To get more details, visit
#' https://platform.openai.com/docs/api-reference/assistants/getAssistant
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param assistant_id string, the ID of the assistant to retrieve.
#' @export
#' 
assistants_retrieve_assistant_request <- function(
    assistant_id,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`assistant_id` must be a non-empty string" = checkmate::testString(assistant_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/assistants/", assistant_id),
    api_key,
    method = "GET"
  )
}

#' API assistants: retrieve assistant file
#'
#' Retrieves an AssistantFile. To get more details, visit
#' https://platform.openai.com/docs/api-reference/assistants/getAssistantFile
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param assistant_id string, the ID of the assistant who the file belongs to.
#' @param file_id string, the ID of the file we're getting.
#' 
assistants_retrieve_assistant_file_request <- function(
    assistant_id,
    file_id,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`assistant_id` must be a non-empty string" = checkmate::testString(assistant_id, min.chars = 1),
    "`file_id` must be a non-empty string" = checkmate::testString(file_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/assistants/", assistant_id,"/files/", file_id),
    api_key,
    method = "GET"
  )
}

#' API assistants: modify assistant
#'
#' Modifies an assistant. To get more details, visit
#' https://platform.openai.com/docs/api-reference/assistants/modifyAssistant
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param assistant_id string, the ID of the assistant to modify
#' @param model string, ID of the model to use. You can use the List models API to see all of your available models, or
#' see our model overview (https://platform.openai.com/docs/models/overview) for descriptions of them.
#' @param name NULL/string, the name of the assistant. The maximum length is 256 characters.
#' @param description NULL/string, the description of the assistant. The maximum length is 512 characters.
#' @param instructions NULL/string, the system instructions that the assistant uses. The maximum length is 32768
#' characters.
#' @param tools NULL/list, a list of tool enabled on the assistant. There can be a maximum of 128 tools per
#' assistant. Tools can be of types code_interpreter, retrieval, or function.
#' @param file_ids NULL/character vector, a list of file IDs attached to this assistant. There can be a maximum of 20
#' files attached to the assistant. Files are ordered by their creation date in ascending order.
#' @param metadata NULL/list, set of 16 key-value pairs that can be attached to an object. This can be useful for
#' storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long
#' and values can be a maxium of 512 characters long.
#' @export
#' 
assistants_modify_assistant_request <- function(
    assistant_id,
    model = NULL,
    name = NULL,
    description = NULL,
    instructions = NULL,
    tools = NULL,
    file_ids = NULL,
    metadata = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`assistant_id` must be a non-empty string" = checkmate::testString(assistant_id, min.chars = 1),
    "`model` must be a NULL or non-empty string" = checkmate::testString(model, min.chars = 1, null.ok = TRUE),
    "`name` must be a NULL or non-empty string" = checkmate::testString(name, min.chars = 1, null.ok = TRUE),
    "`description` must be a NULL or non-empty string" =
        checkmate::testString(description, min.chars = 1, null.ok = TRUE),
    "`instructions` must be a NULL or non-empty string" =
        checkmate::testString(instructions, min.chars = 1, null.ok = TRUE),
    "`tools` must be a NULL or non-empty list" = checkmate::testList(tools, min.len = 1, null.ok = TRUE),
    "`file_ids` must be a NULL or non-empty character vector" =
        checkmate::testCharacter(file_ids, min.len = 1, min.chars = 1, null.ok = TRUE),
    "`metadata` must be a NULL or list" = checkmate::testList(metadata, min.len = 1, null.ok = TRUE)
  )
  
  request(
    paste0("https://api.openai.com/v1/assistants/", assistant_id),
    api_key,
    body = list(
      model = model,
      name = name,
      description = description,
      instructions = instructions,
      tools = tools,
      file_ids = file_ids,
      metadata = metadata
    )
  )
}

#' API assistants: delete assistant
#'
#' Delete an assistant. To get more details, visit
#' https://platform.openai.com/docs/api-reference/assistants/deleteAssistant
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param assistant_id string, the ID of the assistant to delete
#' @export
#' 
assistants_delete_assistant_request <- function(
    assistant_id,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`assistant_id` must be a non-empty string" = checkmate::testString(assistant_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/assistants/", assistant_id),
    api_key,
    method = "DELETE"
  )
}

#' API assistants: delete assistant file
#'
#' Delete an AssistantFile. To get more details, visit
#' https://platform.openai.com/docs/api-reference/assistants/deleteAssistantFile
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param assistant_id string, the ID of the assistant who the file belongs to.
#' @param file_id string, the ID of the file to delete
#' 
assistants_delete_assistant_file_request <- function(
    assistant_id,
    file_id,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`assistant_id` must be a non-empty string" = checkmate::testString(assistant_id, min.chars = 1),
    "`file_id` must be a non-empty string" = checkmate::testString(file_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/assistants/", assistant_id, "/files/", file_id),
    api_key,
    method = "DELETE"
  )
}
