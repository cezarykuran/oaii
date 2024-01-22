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
