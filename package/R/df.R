#' df roxygen template
#'
#' @param df data.frame, input data.frame
#' @param on_missing_col string, behavior for missing column(s):
#' "warn" - log warning, "skip" - skip missing column(s), "stop" - throw error
#' @return Modified input data.frame
#' @keywords internal
#'
df_roxygen_tpl <- function(df, on_missing_col) NULL

#' Replace all NULL values in given data.frame
#'
#' @inherit df_roxygen_tpl params return
#' @param replacement string, replacement for NULL
#' @export
#'
df_null_replace <- function(df, replacement = "") {
  # asserts
  stopifnot(
    "`df` must be a data.frame" = checkmate::testDataFrame(df),
    "`replacement` must be a string" = checkmate::testString(replacement)
  )

  df[sapply(df, is.null)] <- replacement
  df
}

#' Assert helper for the df_X functions
#' @noRd
df_test_col <- function(df, col, on_missing_col) {
  on_missing_col != "stop" || all(col %in% names(df))
}

#' Helper for the df_X functions - remove missing column name(s),
#' warn about it (if required)
#' @noRd
df_proccess_col <- function(df, col, on_missing_col) {
  names_df <- names(df)
  missed <- col[!col %in% names(df)]
  if (length(missed)) {
    if (on_missing_col == "warn") {
      log_warning(
        as.character(as.list(sys.call(-1))[[1]]),
        "() - missing column names(s) '", paste0(missed, collapse = "', '"), "' in df"
      )
    }
    col[col %in% names_df]
  }
  else col
}

#' Remove columns from data.frame
#'
#' @inherit df_roxygen_tpl params return
#' @param col character vector, column name(s) to be deleted
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:3, b = 1:3, c = 1:3)
#' df_exclude_col(df, "b")
#' df_exclude_col(df, c("a", "c"))
#'
df_exclude_col <- function(df, col, on_missing_col = "warn") {
  # asserts
  stopifnot(
    "`df` must be a data.frame" = checkmate::testDataFrame(df),
    "`col` must be a non-empty character vector" =
        checkmate::testCharacter(col, min.len = 1),
    "`on_missing_col` must be a 'warn', 'skip' or 'stop'" =
        checkmate::testString(on_missing_col, min.chars = 1) &&
        on_missing_col %in% c("warn", "skip", "stop"),
    "`col` contains a column name that does not exist in df" =
        df_test_col(df, col, on_missing_col)
  )

  col <- df_proccess_col(df, col, on_missing_col)
  if (!length(col)) return(df)
  df[, -which(names(df) %in% col)]
}

#' Sort data.frame by column name
#'
#' @inherit df_roxygen_tpl params return
#' @param col string, column name as sort source
#' @param decreasing flag, should the sort order be increasing or decreasing?
#' @export
#'
#' @examples
#' df <- data.frame(
#'   a = c("a", "b", "c"),
#'   b = c(1, 3, 2),
#'   c = c(3, 2, 1)
#' )
#' df_order_by_col(df, "b", decreasing = TRUE)
#' df_order_by_col(df, "c")
#'
df_order_by_col <- function(
    df,
    col,
    decreasing = FALSE,
    on_missing_col = "warn"
  ) {
  # asserts
  stopifnot(
    "`df` must be a data.frame" = checkmate::testDataFrame(df),
    "`col` must be a non-empty character vector" =
        checkmate::testCharacter(col, min.len = 1),
    "`decreasing` must be a flag" = checkmate::testFlag(decreasing),
    "`on_missing_col` must be a 'warn', 'skip' or 'stop'" =
        checkmate::testString(on_missing_col, min.chars = 1) &&
        on_missing_col %in% c("warn", "skip", "stop"),
    "`col` contains a column name that does not exist in df" =
        df_test_col(df, col, on_missing_col)
  )

  col <- df_proccess_col(df, col, on_missing_col)
  if (!length(col)) return(df)
  df[order(unlist(df[, col]), decreasing = decreasing), ]
}

#' Change to string nested lists in a given data.frame
#'
#' @inherit df_roxygen_tpl params return
#' @param col character vector, df column names containing objects
#' @param obj_prop NULL/character vector, object properties (NULL means all)
#' @param nested flag, whether the rows of the columns contain multiple objects
#' @param cell_header string/NULL, cell header
#' @param objs_glue string, how to combine objects
#' @param cell_footer string/NULL, cell footer
#' @param obj_header string/NULL, object header
#' @param props_glue string, how to combine properties
#' @param obj_footer string/NULL, object footer
#' @param prop_fmt string, sprintf fmt parameter with two `\%s` fields (property
#' @param null_prop_str string, value for NULL object property
#' name, value)
#' @export
#'
#' @examples
#' df <- as.data.frame(do.call(cbind, list(
#'   a = list(list(x = 1, y = 2), list(x = 3, y = 4)),
#'   b = list("z", "z")
#' )))
#' df_col_obj_implode(df, "a", c("x", "y"), nested = FALSE, props_glue = ", ")
#'
df_col_obj_implode <- function(
    df,
    col,
    obj_prop = NULL,
    nested = TRUE,
    cell_header = "",
    objs_glue = "----\n",
    cell_footer = "",
    obj_header = "",
    props_glue = "\n",
    obj_footer = "",
    prop_fmt = "%s: %s",
    null_prop_str = "[null]",
    on_missing_col = "warn"
  ) {
  # asserts
  stopifnot(
    "`df` must be a data.frame" = checkmate::testDataFrame(df),
    "`col` must be a non-empty character vector" =
        checkmate::testCharacter(col, min.len = 1),
    "`obj_prop` must be a NULL or character vectior" =
        checkmate::testCharacter(obj_prop, min.len = 1, null.ok = TRUE),
    "`nested` must be a flag" = checkmate::testFlag(nested),
    "`cell_header` must be a string" = checkmate::testString(cell_header),
    "`objs_glue` must be a string" = checkmate::testString(objs_glue),
    "`cell_footer` must be a string" = checkmate::testString(cell_footer),
    "`obj_header` must be a string" = checkmate::testString(obj_header),
    "`props_glue` must be a string" = checkmate::testString(props_glue),
    "`obj_footer` must be a string" = checkmate::testString(obj_footer),
    "`prop_fmt` must be a string" = checkmate::testString(prop_fmt),
    "`null_prop_str` must be a string" = checkmate::testString(null_prop_str),
    "`on_missing_col` must be a 'warn', 'skip' or 'stop'" =
        checkmate::testString(on_missing_col, min.chars = 1) &&
        on_missing_col %in% c("warn", "skip", "stop"),
    "`col` contains a column name that does not exist in df" =
        df_test_col(df, col, on_missing_col)
  )

  col <- df_proccess_col(df, col, on_missing_col)

  # iterate over column name(s)
  for (coln in col) {
    # update data.frame column
    df[, coln] <-
      # new data.frame column (list of lists/rows)
      list(lapply(df[, coln], function(objs) {
        # if the column's rows contain only single object, nest it
        if (!nested) objs <- list(objs)

        # cell (collapsed objects)
        paste0(
          cell_header,
          paste0(
            lapply(objs, function(obj) {
              # object (collapsed properties)
              if (is.null(obj_prop)) obj_prop <- names(obj)
              if (is.null(obj_prop)) obj_prop <- seq_along(obj)
              paste0(
                obj_header,
                paste0(
                  lapply(obj_prop, function(propn) {
                    # object property (name-value pair)
                    propv <-
                      if (is.null(obj[[propn]])) null_prop_str
                      else paste0(obj[[propn]], collapse = " ")
                    sprintf(prop_fmt, propn, propv)
                  }),
                  collapse = props_glue
                ),
                obj_footer
              )
            }),
            collapse = objs_glue
          ),
          cell_footer
        )
      }))
  }
  df
}

#' Replace unix timestamp column(s) to formated dt string
#'
#' @inherit df_roxygen_tpl params return
#' @inheritParams timestap_dt_str
#' @param col character vector, column names of the df that will be modified
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c("a", "b"),
#'   dt = c(1687868601, 1688417643)
#' )
#' df_col_dt_format(df, "dt")
#' df_col_dt_format(df, "dt", "%H:%M")
#'
df_col_dt_format <- function(
    df,
    col,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "",
    on_missing_col = "warn"
  ) {
  # asserts
  stopifnot(
    "`df` must be a data.frame" = checkmate::testDataFrame(df),
    "`col` must be a non-empty character vector" =
        checkmate::testCharacter(col, min.len = 1),
    "`on_missing_col` must be a 'warn', 'skip' or 'stop'" =
        checkmate::testString(on_missing_col, min.chars = 1) &&
        on_missing_col %in% c("warn", "skip", "stop"),
    "`col` contains a column name that does not exist in df" =
        df_test_col(df, col, on_missing_col)
  )

  col <- df_proccess_col(df, col, on_missing_col)

  # iterate over column name(s)
  for (coln in col) {
    df[, coln] <- list(lapply(
      df[, coln],
      function(timestamp) timestap_dt_str(timestamp, format, tz)
    ))
  }
  df
}
