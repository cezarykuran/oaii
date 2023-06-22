logger <- new.env()

# internal log functions
msg_log <- function(type, ...) {
  message(paste0(
    type,
    " [", format(Sys.time(), "%Y-%m-%d %H:%M:%S", usetz = FALSE), "] oaii :: ",
    ...
  , collapse = ""))
}
msg_log_error <- function(...) msg_log("ERROR", ...)
msg_log_warning <- function(...) msg_log("WARNING", ...)
msg_log_info <- function(...) msg_log("INFO", ...)
msg_log_debug <- function(...) msg_log("DEBUG", ...)

# attach internal log functions
set_default_logger <- function() {
  logger$error <- msg_log_error
  logger$warning <- msg_log_warning
  logger$info <- msg_log_info
  logger$debug <- msg_log_debug
}

# log functions
log_error <- function(...) do.call(logger$error, list(...))
log_warning <- function(...) do.call(logger$warning, list(...))
log_info <- function(...) do.call(logger$info, list(...))
log_debug <- function(...) do.call(logger$debug, list(...))


#' Set log functions used by this package
#'
#' @param ... parameters in form log_level = function
#'
#' @export
#'
set_logger <- function(...) {
  f <- list(...)
  stopifnot(
    "all parameters must be a functions" = all(unlist(lapply(f, is.function))),
    "all parameters must be nammed (error/warning/info/debug)" = all(names(f) %in% c("error", "warning", "info", "debug"))
  )
  for(type in names(f)) logger[[type]] <- f[[type]]
}
