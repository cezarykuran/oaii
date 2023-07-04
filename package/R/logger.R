logger <- new.env()

# internal log functions
log_msg <- function(type, ...) {
  message(paste0(
    type,
    " [", format(Sys.time(), "%Y-%m-%d %H:%M:%S", usetz = FALSE), "] oaii :: ",
    ...,
    collapse = ""
  ))
}
log_msg_error <- function(...) log_msg("ERROR", ...)
log_msg_warning <- function(...) log_msg("WARNING", ...)
log_msg_info <- function(...) log_msg("INFO", ...)
log_msg_debug <- function(...) log_msg("DEBUG", ...)

# attach internal log functions
set_default_logger <- function() {
  logger$error <- log_msg_error
  logger$warning <- log_msg_warning
  logger$info <- log_msg_info
  logger$debug <- log_msg_debug
}

# log functions
log_error <- function(...) do.call(logger$error, list(...))
log_warning <- function(...) do.call(logger$warning, list(...))
log_info <- function(...) do.call(logger$info, list(...))
log_debug <- function(...) do.call(logger$debug, list(...))


#' Set log functions used by this package
#'
#' @param ... parameters in form log_level = function
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' logger <- log4r::logger("DEBUG")
#' log_error <- function(...) log4r::error(logger, ...)
#' log_warning <- function(...) log4r::warn(logger, ...)
#' log_info <- function(...) log4r::info(logger, ...)
#' log_debug <- function(...) log4r::debug(logger, ...)
#' oaii::set_logger(
#'   error = log_error,
#'   warning = log_warning,
#'   info = log_info,
#'   debug = log_debug
#' )
#' }
#'
set_logger <- function(...) {
  f <- list(...)
  stopifnot(
    "all parameters must be a functions" = all(unlist(lapply(f, is.function))),
    "all parameters must be nammed (error/warning/info/debug)" = all(names(f) %in% c("error", "warning", "info", "debug"))
  )
  for(type in names(f)) logger[[type]] <- f[[type]]
  invisible()
}
