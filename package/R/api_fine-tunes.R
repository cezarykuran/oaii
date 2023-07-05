#' API fine tunes: roxygen template
#'
#' @inherit request params return
#' @keywords internal
#'
fine_tunes_roxygen_tpl <- function(
    api_key
) NULL


#' API fine tunes: create (model) request
#'
#' \url{https://platform.openai.com/docs/api-reference/fine-tunes/create}
#' @inherit fine_tunes_roxygen_tpl params return
#' @param training_file string, the ID of an uploaded file that contains
#' training data. See files_upload_request().
#' @param validation_file string/NULL, the ID of an uploaded file that contains
#' validation data.
#' @param model string, the name of the base model to fine-tune.
#' You can select one of "ada", "babbage", "curie", "davinci", or a fine-tuned
#' model created after 2022-04-21. To learn more about these models, see the
#' \href{https://platform.openai.com/docs/models}{Models} documentation.
#' @param n_epochs integer, the number of epochs to train the model for.
#' An epoch refers to one full cycle through the training dataset.
#' @param batch_size integer/NULL, the batch size to use for training.
#' The batch size is the number of training examples used to train a single
#' forward and backward pass. By default, the batch size will be dynamically
#' configured to be ~0.2% of the number of examples in the training set,
#' capped at 256 - in general, we've found that larger batch sizes tend
#' to work better for larger datasets.
#' @param learning_rate_multiplier double/NULL, the learning rate multiplier
#' to use for training. The fine-tuning learning rate is the original
#' learning rate used for pretraining multiplied by this value. By default,
#' the learning rate multiplier is the 0.05, 0.1, or 0.2 depending
#' on final batch_size (larger learning rates tend to perform better with larger
#' batch sizes). We recommend experimenting with values in the range 0.02 to 0.2
#' to see what produces the best results.
#' @param prompt_loss_weight double, The weight to use for loss on the prompt
#' tokens. This controls how much the model tries to learn to generate
#' the prompt (as compared to the completion which always has a weight of 1.0),
#' and can add a stabilizing effect to training when completions are short.
#' If prompts are extremely long (relative to completions), it may make sense
#' to reduce this weight so as to avoid over-prioritizing learning the prompt.
#' @param compute_classification_metrics flag, if set, we calculate
#' classification-specific metrics such as accuracy and F-1 score using
#' the validation set at the end of every epoch. These metrics can be viewed
#' in the results file. In order to compute classification metrics, you must
#' provide a validation_file. Additionally, you must specify
#' classification_n_classes for multiclass classification
#' or classification_positive_class for binary classification.
#' @param classification_n_classes integer/NULL, the number of classes in a
#' classification task. This parameter is required for multiclass classification.
#' @param classification_positive_class string/NULL, The positive class in
#' binary classification. This parameter is needed to generate precision,
#' recall, and F1 metrics when doing binary classification.
#' @param classification_betas array/NULL, if this is provided, we calculate
#' F-beta scores at the specified beta values. The F-beta score is a
#' generalization of F-1 score. This is only used for binary classification.
#' With a beta of 1 (i.e. the F-1 score), precision and recall are given the
#' same weight. A larger beta score puts more weight on recall and less on
#' precision. A smaller beta score puts more weight on precision and less on
#' recall.
#' @param suffix string/NULL, a string of up to 40 characters that will be added
#' to your fine-tuned model name. For example, a suffix of "my-model"
#' would produce a model name like ada:ft-your-org:my-model-2022-02-15-04-21-04.
#' @export
#'
fine_tunes_create_request <- function(
    api_key,
    training_file,
    validation_file = NULL,
    model =  NULL,
    n_epochs = NULL,
    batch_size = NULL,
    learning_rate_multiplier = NULL,
    prompt_loss_weight = NULL,
    compute_classification_metrics = NULL,
    classification_n_classes = NULL,
    classification_positive_class = NULL,
    classification_betas = NULL,
    suffix = NULL
) {
  # asserts
  stopifnot(
    "`training_file` must be a non-empty string" = checkmate::testString(training_file, min.chars = 1),
    "`validation_file` must be a NULL or non-empty string" = checkmate::testString(validation_file, min.chars = 1, null.ok = TRUE),
    "`model` must be a NULL or non-empty string" = checkmate::testString(model, min.chars = 1, null.ok = TRUE),
    "`n_epochs` must be a integer" = checkmate::testInt(n_epochs, null.ok = TRUE),
    "`batch_size` must be a NULL or double" = checkmate::testDouble(batch_size, null.ok = TRUE),
    "`learning_rate_multiplier` must be a NULL or double" = checkmate::testDouble(learning_rate_multiplier, null.ok = TRUE),
    "`prompt_loss_weight` must be a NULL or double" = checkmate::testDouble(prompt_loss_weight, null.ok = TRUE),
    "`compute_classification_metrics` must be a NULL or flag" = checkmate::testFlag(compute_classification_metrics, null.ok = TRUE),
    "`classification_n_classes` must be a NULL or integer" = checkmate::testInt(classification_n_classes, null.ok = TRUE),
    "`classification_positive_class` must be a NULL or non-empty string" = checkmate::testString(classification_positive_class, min.chars = 1, null.ok = TRUE),
    "`classification_betas` must be a NULL or array" = checkmate::testArray(classification_betas, null.ok = TRUE),
    "`suffix` must be a NULL or non-empty string" = checkmate::testString(suffix, min.chars = 1, null.ok = TRUE)
  )

  request("https://api.openai.com/v1/fine-tunes",
    api_key,
    body = list(
      training_file = training_file,
      validation_file = validation_file,
      model = model,
      n_epochs = n_epochs,
      batch_size = batch_size,
      learning_rate_multiplier = learning_rate_multiplier,
      prompt_loss_weight = prompt_loss_weight,
      compute_classification_metrics = compute_classification_metrics,
      classification_n_classes = classification_n_classes,
      classification_positive_class = classification_positive_class,
      classification_betas = classification_betas,
      suffix = suffix
    )
  )
}

#' API fine tunes: list request
#'
#' \url{https://platform.openai.com/docs/api-reference/fine-tunes/list}
#' @inherit fine_tunes_roxygen_tpl params return
#' @export
#'
#' @examples
#' \dontrun{
#' res_content <- fine_tunes_list_request("my-secret-api-key-string")
#' if (!is_error(res_content)) {
#'   fine_tunes_list_df <- fine_tunes_fetch_list(res_content)
#'   print(fine_tunes_list_df)
#' }
#' }
#'
fine_tunes_list_request <- function(api_key) {
  request(
    endpoint = "https://api.openai.com/v1/fine-tunes",
    api_key = api_key,
    method = "GET"
  )
}

#' Extract fine-tunes models list from response object
#'
#' Extract fine-tunes models list as data.frame from response object
#' @inherit fine_tunes_list_request examples
#' @param res_content response object returned by \link{fine_tunes_list_request}
#' @return fine-tunes list models as data.frame
#' @export
#'
fine_tunes_fetch_list <- function(res_content) {
  fine_tunes <- as.data.frame(do.call(rbind, res_content$data))
  class(fine_tunes) <- c("oaiiFineTunesDF", class(fine_tunes))
  fine_tunes
}

#' print S3 method for oaiiFilesDF class
#'
#' @inherit base::print description params return
#' @export
#'
print.oaiiFineTunesDF <- function(x, ...) {
  x %>%
    df_col_dt_format(
      c("created_at", "updated_at"),
      on_missing_col = "skip"
    ) %>%
    df_col_obj_implode(
      c("training_files", "result_files"),
      props_glue = ", ",
      on_missing_col = "skip"
    ) %>%
    df_col_obj_implode(
      "hyperparams",
      props_glue = ", ",
      nested = FALSE,
      on_missing_col = "skip"
    ) -> x
  NextMethod()
}
