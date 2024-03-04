#' API fine-tuning: create job (model) request
#'
#' Creates a fine-tuning job which begins the process of creating a new model from a given dataset. To get more details,
#' visit https://platform.openai.com/docs/guides/fine-tuning
#' https://platform.openai.com/docs/api-reference/fine-tuning/create
#' @inherit request params return
#' @param model string, the name of the base model to fine-tune. You can select one of the supported models:
#' gpt-3.5-turbo-1106 (recommended), gpt-3.5-turbo-0613, babbage-002, davinci-002, gpt-4-0613 (experimental)
#' @param training_file string, the ID of an uploaded file that contains training data. See \link{files_upload_request}.
#' @param hyperparameters list/NULL, the hyperparameters used for the fine-tuning job.
#' `hyperparameters$batch_size` string/integer/NULL defaults to "auto", number of examples in each batch. A larger batch
#' size means that model parameters are updated less frequently, but with lower variance.
#' `hyperparameters$learning_rate_multiplier` string/number/NULL defaults to "auto", scaling factor for the learning
#' rate. A smaller learning rate may be useful to avoid overfitting.
#' `hyperparameters$n_epochs` string/integer/NULL, defaults to "auto", the number of epochs to train the model for. An
#' epoch refers to one full cycle through the training dataset.
#' @param suffix string/NULL, A string of up to 18 characters that will be added to your fine-tuned model name. For
#' example, a suffix of "custom-model-name" would produce a model name like 
#' ft:gpt-3.5-turbo:openai:custom-model-name:7p4lURel
#' @param validation_file string/NULL, the ID of an uploaded file that contains validation data. If you provide this
#' file, the data is used to generate validation metrics periodically during fine-tuning. These metrics can be viewed in
#' the fine-tuning results file. The same data should not be present in both train and validation files. Your dataset
#' must be formatted as a JSONL file. You must upload your file with the purpose fine-tune.
#' @export
#'
fine_tuning_create_job_request <- function(
    model,
    training_file,
    hyperparameters = NULL,
    suffix = NULL,
    validation_file = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`model` must be a non-empty string" = checkmate::testString(model, min.chars = 1),
    "`training_file` must be a non-empty string" = checkmate::testString(training_file, min.chars = 1),
    "`hyperparameters` must be a NULL or list" = is.null(hyperparameters) ||
        checkmate::testList(hyperparameters) &&
        all(names(hyperparameters) %in% c("batch_size", "learning_rate_multiplier", "n_epochs")),
    "`hyperparameters$n_epochs` must be a integer" =
        is.null(hyperparameters) ||
        checkmate::testInt(hyperparameters$n_epochs, null.ok = TRUE),
    "`hyperparameters$batch_size` must be a NULL, string or double" =
        is.null(hyperparameters) ||
        checkmate::testDouble(hyperparameters$batch_size, null.ok = TRUE) ||
        checkmate::testString(hyperparameters$batch_size, null.ok = TRUE),
    "`hyperparameters$learning_rate_multiplier` must be a NULL or double" =
        is.null(hyperparameters) ||
        checkmate::testDouble(hyperparameters$earning_rate_multiplier, null.ok = TRUE),
    "`suffix` must be a NULL or non-empty string" =
        checkmate::testString(suffix, min.chars = 1, null.ok = TRUE),
    "`validation_file` must be a NULL or non-empty string" =
        checkmate::testString(validation_file, min.chars = 1, null.ok = TRUE)
  )

  request("https://api.openai.com/v1/fine_tuning/jobs",
    api_key,
    body = list(
      model = model,
      training_file = training_file,
      hyperparameters = hyperparameters,
      validation_file = validation_file,
      suffix = suffix
    )
  )
}

#' API fine-tuning: list jobs request
#'
#' List your organization's fine-tuning jobs. To get more details, visit
#' https://platform.openai.com/docs/guides/fine-tuning
#' https://platform.openai.com/docs/api-reference/fine-tuning/list
#' @inherit request params return
#' @param after NULL/string, identifier for the last job from the previous pagination request
#' @param limit NULL/integer, number of fine-tuning jobs to retrieve (default 20)
#' @export
#'
#' @examples
#' \dontrun{
#' res_content <- fine_tuning_jobs_list_request()
#' if (!is_error(res_content)) {
#'   fine_tuning_jobs_df <- fine_tuning_fetch_jobs_list(res_content)
#'   print(fine_tuning_jobs_df)
#' }
#' }
#'
fine_tuning_jobs_list_request <- function(
    after = NULL,
    limit = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`after` must be a NULL or non-empty string" =
        checkmate::testString(after, min.chars = 1, null.ok = TRUE),
    "`limit` must be a NULL or integer" = checkmate::testInt(limit, null.ok = TRUE)
  )

  request(
    endpoint = "https://api.openai.com/v1/fine_tuning/jobs",
    api_key = api_key,
    method = "GET",
    query = list(
      after = after,
      limit = limit
    )
  )
}

#' API fine-tuning: extract fine-tuning jobs list from response object
#'
#' Extract fine-tuning jobs list as data.frame from response object
#' @inherit fine_tuning_jobs_list_request examples
#' @param res_content response object returned by \link{fine_tuning_jobs_list_request}
#' @return fine-tuning list models as data.frame
#' @export
#'
fine_tuning_fetch_jobs_list <- function(res_content) {
  jobs <- as.data.frame(do.call(rbind, res_content$data))
  class(jobs) <- c("oaii_fine_tuning_jobs_df", class(jobs))
  jobs
}

#' API fine-tuning: list events request
#'
#' Get status updates for a fine-tuning job. To get more details, visit
#' https://platform.openai.com/docs/guides/fine-tuning
#' https://platform.openai.com/docs/api-reference/fine-tuning/list-events
#' @inherit request params return
#' @param fine_tuning_job_id string, the ID of the fine-tuning job to get events for
#' @param after string/NULL, identifier for the last event from the previous pagination request.
#' @param limit integer/NULL, number of events to retrieve (defaults to 20)
#' @export
#'
#' @examples
#' \dontrun{
#' res_content <- fine_tuning_events_list_request("job-id")
#' if (!is_error(res_content)) {
#'   fine_tuning_events_df <- fine_tuning_fetch_events_list(res_content)
#'   print(fine_tuning_events_df)
#' }
#' }
#'
fine_tuning_events_list_request <- function(
    fine_tuning_job_id,
    after = NULL,
    limit = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`fine_tuning_job_id` must be a non-empty string" =
      checkmate::testString(fine_tuning_job_id, min.chars = 1),
    "`after` must be a NULL or non-empty string" =
      checkmate::testString(after, min.chars = 1, null.ok = TRUE),
    "`limit` must be a NULL or integer" = checkmate::testInt(limit, null.ok = TRUE)
  )
  
  request(
    endpoint = paste0("https://api.openai.com/v1/fine_tuning/jobs/", fine_tuning_job_id, "/events"),
    api_key = api_key,
    method = "GET",
    query = list(
      after = after,
      limit = limit
    )
  )
}

#' API fine-tuning: job list from response object
#'
#' Extract fine-tuning job list as data.frame from response object
#' @inherit fine_tuning_events_list_request examples
#' @param res_content response object returned by \link{fine_tuning_events_list_request}
#' @return fine-tuning events list as data.frame
#' @export
#'
fine_tuning_fetch_events_list <- function(res_content) {
  events <- as.data.frame(do.call(rbind, res_content$data))
  class(events) <- c("oaii_fine_tuning_events_df", class(events))
  events
}

#' API fine-tuning: retrieve fine-tuning job request
#'
#' Get info about a fine-tuning job. To get more details, visit
#' https://platform.openai.com/docs/guides/fine-tuning
#' https://platform.openai.com/docs/api-reference/fine-tuning/retrieve
#' @inherit request params return
#' @param fine_tuning_job_id string, the ID of the fine-tuning job to get events for
#' @export
#'
#' @examples
#' \dontrun{
#' res_content <- fine_tuning_retrive_job_request("job-id")
#' if (!is_error(res_content)) {
#'   fine_tuning_events_df <- fine_tuning_fetch_events_list(res_content)
#'   print(fine_tuning_events_df)
#' }
#' }
#'
fine_tuning_retrive_job_request <- function(
    fine_tuning_job_id,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`fine_tuning_job_id` must be a non-empty string" =
      checkmate::testString(fine_tuning_job_id, min.chars = 1)
  )

  request(
    endpoint = paste0("https://api.openai.com/v1/fine_tuning/jobs/", fine_tuning_job_id),
    api_key = api_key,
    method = "GET"
  )
}

#' API fine-tuning: fetch retrived job object from response object
#'
#' Extract fine-tuning job object from response object
#' @inherit fine_tuning_retrive_job_request examples
#' @param res_content response object returned by \link{fine_tuning_retrive_job_request}
#' @return fine-tuning job object
#' @export
#'
fine_tuning_fetch_retrived_job <- function(res_content) {
  job <- res_content
  class(job) <- c("oaii_fine_tuning_job", class(job))
  job
}

#' API fine-tuning: cancel fine-tuning job request
#'
#' Immediately cancel a fine-tune job. To get more details, visit
#' https://platform.openai.com/docs/guides/fine-tuning
#' https://platform.openai.com/docs/api-reference/fine-tuning/cancel
#' @inherit request params return
#' @param fine_tuning_job_id string, the ID of the fine-tuning job to cancel
#' @export
#'
#' @examples
#' \dontrun{
#' res_content <- fine_tuning_cancel_job_request("job-id")
#' if (!is_error(res_content)) {
#'   message("job canceled")
#' }
#' }
#'
fine_tuning_cancel_job_request <- function(
    fine_tuning_job_id,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`fine_tuning_job_id` must be a non-empty string" =
      checkmate::testString(fine_tuning_job_id, min.chars = 1)
  )
  
  request(
    endpoint = paste0("https://api.openai.com/v1/fine_tuning/jobs/", fine_tuning_job_id, "/cancel"),
    api_key = api_key,
    method = "GET"
  )
}
