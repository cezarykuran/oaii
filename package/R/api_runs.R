#' API runs: create run
#'
#' Create a run. To get more details, visit
#' https://platform.openai.com/docs/api-reference/runs/createRun
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread to run
#' @param assistant_id string, the ID of the assistant to use to execute this run
#' @param model NULL/string, the ID of the model (https://platform.openai.com/docs/api-reference/models) to be used to
#' execute this run. If a value is provided here, it will override the model associated with the assistant. If not, the
#' model associated with the assistant will be used.
#' @param instructions NULL/string, overrides the instructions
#' (https://platform.openai.com/docs/api-reference/assistants/createAssistant) of the assistant. This is useful for
#' modifying the behavior on a per-run basis.
#' @param additional_instructions NULL/string, appends additional instructions at the end of the instructions for the
#' run. This is useful for modifying the behavior on a per-run basis without overriding other instructions.
#' @param tools NULL/named list, override the tools the assistant can use for this run. This is useful for modifying the
#' behavior on a per-run basis. Example:
#' # code interpreter tool
#' list(
#'   type = "code_interpreter"
#' )
#' # or retrieval tool
#' list(
#'   type = "retrieval"
#' )
#' # or function tool
#' list(
#'   type = "retrieval",
#'   function = list(
#'     # string (optional), a description of what the function does, used by the model to choose when and how to call
#'     # the function.
#'     description = 
#'     # string (required), the name of the function to be called. Must be a-z, A-Z, 0-9, or contain underscores and
#'     # dashes, with a maximum length of 64.
#'     name =
#'     # list (optional), the parameters the functions accepts. See the guide
#'     # (https://platform.openai.com/docs/guides/text-generation/function-calling) for examples. Omitting parameters
#'     # defines a function with an empty parameter list.
#'     parameters = list (
#'     )
#'  )
#' )
#' @param metadata NULL/list, set of 16 key-value pairs that can be attached to an object. This can be useful for
#' storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long
#' and values can be a maximum of 512 characters long.
#' @export
#' 
#' @examples
#' \dontrun{
#' runs_create_run_request(
#'   thread_id = "thread_abc123",
#'   assistant_id = "asst_abc123"
#' )
#' }
#' 
runs_create_run_request <- function(
    thread_id,
    assistant_id,
    model = NULL,
    instructions = NULL,
    additional_instructions = NULL,
    tools = NULL,
    metadata = NULL,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`assistant_id` must be a non-empty string" = checkmate::testString(assistant_id, min.chars = 1),
    "`model` must be a NULL or non-empty string" = checkmate::testString(model, min.chars = 1, null.ok = TRUE),
    "`instructions` must be a NULL or non-empty string" =
        checkmate::testString(instructions, min.chars = 1, null.ok = TRUE),
    "`additional_instructions` must be a NULL or non-empty string" =
        checkmate::testString(additional_instructions, min.chars = 1, null.ok = TRUE),
    "`tools` must be a NULL or non-empty list" =
        checkmate::testList(tools, min.len = 1, null.ok = TRUE),
    "`metadata` must be a NULL or non-empty list" =
        checkmate::testList(metadata, min.len = 1, null.ok = TRUE)
  )

  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/runs"),
    api_key,
    body = list(
      assistant_id = assistant_id,
      model = model,
      instructions = instructions,
      additional_instructions = additional_instructions,
      tools = tools,
      metadata = metadata
    )
  )
}

#' API runs: create thread and run
#'
#' Create a thread and run it in one request. To get more details, visit
#' https://platform.openai.com/docs/api-reference/runs/createThreadAndRun
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param assistant_id string, the ID of the assistant to use to execute this run
#' @param thread NULL/list, 
#' list(
#'   # messages "array" (list of list(s))
#'   messages = list(
#'     list(
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
#'     metadata = list (
#'       meta1 = "value1"
#'     )
#'   )
#'  ),
#'  # named list (optional), set of 16 key-value pairs that can be attached to an object. This can be useful for
#'  storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long
#'  and values can be a maximum of 512 characters long.
#'  metadata = list(
#'    metaX = "value y"
#'  )
#' )
#' @param model NULL/string, the ID of the model (https://platform.openai.com/docs/api-reference/models) to be used to
#' execute this run. If a value is provided here, it will override the model associated with the assistant. If not, the
#' model associated with the assistant will be used.
#' @param instructions NULL/string, overrides the instructions
#' (https://platform.openai.com/docs/api-reference/assistants/createAssistant) of the assistant. This is useful for
#' modifying the behavior on a per-run basis.
#' @param tools NULL/named list, override the tools the assistant can use for this run. This is useful for modifying the
#' behavior on a per-run basis. Example:
#' # code interpreter tool
#' list(
#'   type = "code_interpreter"
#' )
#' # or retrieval tool
#' list(
#'   type = "retrieval"
#' )
#' # or function tool
#' list(
#'   type = "retrieval",
#'   function = list(
#'     # string (optional), a description of what the function does, used by the model to choose when and how to call
#'     # the function.
#'     description = 
#'     # string (required), the name of the function to be called. Must be a-z, A-Z, 0-9, or contain underscores and
#'     # dashes, with a maximum length of 64.
#'     name =
#'     # list (optional), the parameters the functions accepts. See the guide
#'     # (https://platform.openai.com/docs/guides/text-generation/function-calling) for examples. Omitting parameters
#'     # defines a function with an empty parameter list.
#'     parameters = list (
#'     )
#'  )
#' )
#' @param metadata NULL/list, set of 16 key-value pairs that can be attached to an object. This can be useful for
#' storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long
#' and values can be a maximum of 512 characters long.
#' @export
#' 
runs_create_thread_and_run_request <- function(
    assistant_id,
    thread,
    model = NULL,
    instructions = NULL,
    tools = NULL,
    metadata = NULL,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`assistant_id` must be a non-empty string" = checkmate::testString(assistant_id, min.chars = 1),
    "`thread` must be a non-empty list" =
        checkmate::testList(metadata, min.len = 1),
    "`model` must be a NULL or non-empty string" = checkmate::testString(model, min.chars = 1, null.ok = TRUE),
    "`instructions` must be a NULL or non-empty string" =
        checkmate::testString(instructions, min.chars = 1, null.ok = TRUE),
    "`tools` must be a NULL or non-empty list" =
        checkmate::testList(tools, min.len = 1, null.ok = TRUE),
    "`metadata` must be a NULL or non-empty list" =
        checkmate::testList(metadata, min.len = 1, null.ok = TRUE)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/runs"),
    api_key,
    body = list(
      assistant_id = assistant_id,
      thread = thread,
      model = model,
      instructions = instructions,
      tools = tools,
      metadata = metadata
    )
  )
}

#' API runs: list runs
#'
#' Returns a list of runs belonging to a thread. To get more details, visit
#' https://platform.openai.com/docs/api-reference/runs/listRuns
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread the run belongs to
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
runs_list_runs_request <- function(
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
    paste0("https://api.openai.com/v1/threads/", thread_id,"/runs"),
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

#' API runs: list run steps
#'
#' Returns a list of runs belonging to a thread. To get more details, visit
#' https://platform.openai.com/docs/api-reference/runs/listRuns
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread the run belongs to
#' @param run_id string, the ID of the run the run steps belong to
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
runs_list_run_steps_request <- function(
    thread_id,
    run_id,
    limit = NULL,
    order = NULL,
    after = NULL,
    before = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`run_id` must be a non-empty string" = checkmate::testString(run_id, min.chars = 1),
    "`limit` must be a NULL or non-empty integer" = checkmate::testInt(limit, null.ok = TRUE),
    "`order` must be a NULL or non-empty string" = checkmate::testString(order, min.chars = 1, null.ok = TRUE),
    "`after` must be a NULL or non-empty string" = checkmate::testString(after, min.chars = 1, null.ok = TRUE),
    "`before` must be a NULL or non-empty string" = checkmate::testString(before, min.chars = 1, null.ok = TRUE)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/runs/", run_id, "/steps"),
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

#' API runs: retrieve run
#'
#' Retrieves a thread. To get more details, visit
#' https://platform.openai.com/docs/api-reference/threads/getThread
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, The ID of the thread (https://platform.openai.com/docs/api-reference/threads) that was run
#' @param run_id string, the ID of the run to retrieve
#' @export
#' 
runs_retrieve_run_request <- function(
    thread_id,
    run_id,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`run_id` must be a non-empty string" = checkmate::testString(run_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/runs/", run_id),
    api_key,
    method = "GET"
  )
}

#' API runs: retrieve run step
#'
#' Retrieves a run step. To get more details, visit
#' https://platform.openai.com/docs/api-reference/runs/getRunStep
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread (https://platform.openai.com/docs/api-reference/threads) to which the
#' run and run step belongs
#' @param run_id string, the ID of the run to which the run step belongs
#' @param step_id string, the ID of the run step to retrieve
#' @export
#' 
runs_retrieve_run_request <- function(
    thread_id,
    run_id,
    step_id,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`run_id` must be a non-empty string" = checkmate::testString(run_id, min.chars = 1),
    "`step_id` must be a non-empty string" = checkmate::testString(run_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/runs/", run_id, "/steps/", step_id),
    api_key,
    method = "GET"
  )
}

#' API runs: modify run
#'
#' Modifies a run. To get more details, visit
#' https://platform.openai.com/docs/api-reference/runs/modifyRun
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread (https://platform.openai.com/docs/api-reference/threads) that was run
#' @param run_id string, the ID of the run to modify
#' @param metadata NULL/list, set of 16 key-value pairs that can be attached to an object. This can be useful for
#' storing additional information about the object in a structured format. Keys can be a maximum of 64 characters long
#' and values can be a maximum of 512 characters long.
#' @export
#' 
runs_modify_run_request <- function(
    thread_id,
    run_id,
    metadata = NULL,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`run_id` must be a non-empty string" = checkmate::testString(run_id, min.chars = 1),
    "`metadata` must be a NULL or non-empty list" =
        checkmate::testList(metadata, min.len = 1, null.ok = TRUE)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/runs/", run_id),
    api_key,
    body = list(
      metadata = metadata
    )
  )
}

#' API runs: submit tool outputs to run
#'
#' When a run has the status: "requires_action" and required_action.type is submit_tool_outputs, this endpoint can be
#' used to submit the outputs from the tool calls once they're all completed. All outputs must be submitted in a single
#' request. To get more details, visit https://platform.openai.com/docs/api-reference/runs/submitToolOutputs
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread (https://platform.openai.com/docs/api-reference/threads) to which this
#' run belongs
#' @param run_id string, the ID of the run that requires the tool output submission
#' @param tool_outputs list, a list of tools for which the outputs are being submitted.
#' list(
#'   # string (optional), the ID of the tool call in the required_action object within the run object the output is
#'   # being submitted for.
#'   tool_call_id = 
#'   # string (optional), the output of the tool call to be submitted to continue the run
#'   output = 
#' )
#' @export
#' 
runs_submit_tool_outputs_request <- function(
    thread_id,
    run_id,
    tool_outputs,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`run_id` must be a non-empty string" = checkmate::testString(run_id, min.chars = 1),
    "`tool_outputs` must be a non-empty list" = checkmate::testList(tool_outputs, min.len = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/runs/", run_id, "/submit_tool_outputs"),
    api_key,
    body = list(
      tool_outputs = tool_outputs
    )
  )
}

#' API runs: cancel a run
#'
#' Cancels a run that is "in_progress". To get more details, visit 
#' https://platform.openai.com/docs/api-reference/runs/cancelRun
#' https://platform.openai.com/docs/assistants
#' @inherit request params return
#' @param thread_id string, the ID of the thread (https://platform.openai.com/docs/api-reference/threads) to which this
#' run belongs
#' @param run_id string, the ID of the run to cancel
#' @export
#' 
runs_cancel_run_request <- function(
    thread_id,
    run_id,
    api_key = api_get_key()
  ) {
  
  # asserts
  stopifnot(
    "`thread_id` must be a non-empty string" = checkmate::testString(thread_id, min.chars = 1),
    "`run_id` must be a non-empty string" = checkmate::testString(run_id, min.chars = 1)
  )
  
  request(
    paste0("https://api.openai.com/v1/threads/", thread_id, "/runs/", run_id, "/cancel"),
    api_key
  )
}
