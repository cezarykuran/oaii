#' Class oaii_res_se print S3 method
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_res_se <- function (x, ...) {
  cat("status_code:", x$status_code, "\n")
  cat("message:", x$message, "\n")
  cat("message_long:", x$message_long, "\n")
  invisible(x)
}

#' Class oaii_content_audio print S3 method
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_content_audio <- function(x, ...) {
  if (is_browseable()) {
    browseable_audio(x)
  }
  else NextMethod()
}

#' Class oaii_content_audio_mp3 print S3 method
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_content_audio_mp3 <- function(x, ...) {
  if (is_browseable()) {
    browseable_audio(x, "mp3")
  }
  else NextMethod()
}

#' Class oaii_content_audio_opus print S3 method
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_content_audio_opus <- function(x, ...) {
  if (is_browseable()) {
    browseable_audio(x, "opus")
  }
  else NextMethod()
}

#' Class oaii_content_audio_aac print S3 method
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_content_audio_aac <- function(x, ...) {
  if (is_browseable()) {
    browseable_audio(x, "aac")
  }
  else NextMethod()
}

#' Class oaii_content_audio_flac print S3 method
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_content_audio_flac <- function(x, ...) {
  if (is_browseable()) {
    browseable_audio(x, "flac")
  }
  else NextMethod()
}

#' Class oaii_content_images print S3 method
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_content_images <- function(x, ...) {
  if (is_browseable()) {
    images <- lapply(x$data, function(img) {
      lapply(names(img), function(type) {
        data <- img[[type]]
        src <- switch (type,
          url = data,
          b64_json = paste0("data:image/png;base64,", data)
        )
        htmltools::img(src = src, style = "max-width: 100%")
      })
    })
    print(htmltools::browsable(
      do.call(
        get("tagList", asNamespace("htmltools")),
        images
      )
    ))
  }
  else NextMethod()
}

#' print S3 method for oaii_models_df class
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_models_df <- function(x, ...) {
  x %>%
    df_col_dt_format(
      "created",
      on_missing_col = "skip"
    ) %>%
    df_col_obj_implode(
      "permission",
      props_glue = ", ",
      on_missing_col = "skip"
    ) -> x
  NextMethod()
}

#' print S3 method for oaii_fine_tuning_jobs_df class
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_fine_tuning_jobs_df <- function(x, ...) {
  x %>%
    df_col_dt_format(
      c("created_at", "finished_at"),
      on_missing_col = "skip"
    ) %>%
    df_col_obj_implode(
      c("training_file", "result_files"),
      props_glue = ", ",
      on_missing_col = "skip"
    ) %>%
    df_col_obj_implode(
      "hyperparameters",
      props_glue = ", ",
      nested = FALSE,
      on_missing_col = "skip"
    ) -> x
  NextMethod()
}

#' print S3 method for oaii_fine_tuning_events_df class
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_fine_tuning_events_df <- function(x, ...) {
  x %>%
    df_col_dt_format(
      c("created_at"),
      on_missing_col = "skip"
    ) -> x
  NextMethod()
}

#' print S3 method for oaii_fine_tuning_job class
#'
#' @inherit base::print description params return
#' @export
#'
print.oaii_fine_tuning_job <- function(x, ...) {
  x$created_at <- timestap_dt_str(x$created_at)
  x$finished_at <- timestap_dt_str(x$finished_at)
  NextMethod()
}
