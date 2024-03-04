#' Test if object belongs to "error" class
#'
#' @param x R variable
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' is_error(FALSE)
#' is_error(simpleError("test"))
#'
is_error <- function(x) {
  inherits(x, c("error", "simpleError", "oaii_res_se"))
}

#' Test if RStudio Viewer (build in browser) is available
#'
#' @return TRUE/FALSE
#' @export
#' 
is_browseable <- function() {
  identical(.Platform$GUI, "RStudio") &&
    "htmltools" %in% rownames(utils::installed.packages())  
}

#' Create browseable HTML audio
#'
#' @param data audio data
#' @param format audio format
#' @return HTML audio
#' @export
#' 
browseable_audio <- function(data, format = "mp3") {
  mime <- paste0(
    "audio/",
    switch(format,
      mp3 = "mpeg",
      opus = "opus",
      aac = "mp4",
      flac = "flac",
      format
    )
  )
  print(
    htmltools::browsable(
      htmltools::tags$audio(
        controls="controls",
        autobuffer="autobuffer",
        autoplay="autoplay",
        htmltools::tags$source(
          src = paste0("data:", mime, ";base64,", base64enc::base64encode(data))
        )
      )
    )
  )
}

#' Convert unix timestamp to formated date/time string
#'
#' @inherit base::strftime params return
#' @param timestamp int, unix timestamp value
#' @export
#' 
timestap_dt_str <- function(
    timestamp,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "",
    usetz = FALSE
  ) {
  # asserts
  stopifnot(
    "`timestamp` must be a int" = checkmate::testInt(timestamp, lower = 0),
    "`format` must be a string" = checkmate::testString(format, min.chars = 1),
    "`tz` must be a string" = checkmate::testString(tz),
    "`format` must be a non-emty string" = checkmate::testString(format, min.chars = 1),
    "`usetz` must be a flag" = checkmate::testFlag(usetz)
  )

  dt_POSIXct <- as.POSIXct(timestamp, origin="1970-01-01", tz = "GMT")
  attr(dt_POSIXct,"tzone") <- "GMT"
  strftime(dt_POSIXct, format = format, tz = tz, usetz = usetz)
}
