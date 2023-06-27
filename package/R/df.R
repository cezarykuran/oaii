#' df roxygen template
#'
#' @param df data.frame, input data.frame
#' @return modified data.frame
#' @keywords internal
#'
df_roxygen_tpl <- function(df) NULL

#' Remove columns from data.frame
#'
#' @inherit df_roxygen_tpl params return
#' @param col character vector, column name(s) to be deleted
#' @export
#'
df_exclude_col <- function(df, col) {
  # asserts
  stopifnot(
    "`df` must be a data.frame" = checkmate::testDataFrame(df),
    "`col` must be a character vectior" = checkmate::testCharacter(col)
  )

  df[, -which(names(df) %in% col)]
}

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

#' Sort data.frame by column name
#'
#' @inherit df_roxygen_tpl params return
#' @param col string, column name as sort source
#' @param decreasing flag, should the sort order be increasing or decreasing?
#' @export
#'
df_order_by_col <- function(df, col, decreasing = FALSE) {
  # asserts
  stopifnot(
    "`df` must be a data.frame" = checkmate::testDataFrame(df),
    "`col` must be a non-empty string" = checkmate::testString(col, min.chars = 1),
    "`decreasing` must be a flag" = checkmate::testFlag(decreasing)
  )
  df[order(unlist(df[, col]), decreasing = decreasing), ]
}

#' Change to string nested lists in a given data.frame
#'
#' @inherit df_roxygen_tpl params return
#' @param col character vector, df column names containing objects
#' @param obj_prop character vector, object properties
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
df_col_obj_implode <- function(
    df,
    col,
    obj_prop,
    nested = TRUE,
    cell_header = "",
    objs_glue = "----\n",
    cell_footer = "",
    obj_header = "",
    props_glue = "\n",
    obj_footer = "",
    prop_fmt = "%s: %s",
    null_prop_str = "[null]"
) {
  # asserts
  stopifnot(
    "`df` must be a data.frame" = checkmate::testDataFrame(df),
    "`col` must be a character vectior" = checkmate::testCharacter(col),
    "`obj_prop` must be a character vectior" = checkmate::testCharacter(obj_prop),
    "`nested` must be a flag" = checkmate::testFlag(nested),
    "`cell_header` must be a string" = checkmate::testString(cell_header),
    "`objs_glue` must be a string" = checkmate::testString(objs_glue),
    "`cell_footer` must be a string" = checkmate::testString(cell_footer),
    "`obj_header` must be a string" = checkmate::testString(obj_header),
    "`props_glue` must be a string" = checkmate::testString(props_glue),
    "`obj_footer` must be a string" = checkmate::testString(obj_footer),
    "`prop_fmt` must be a string" = checkmate::testString(prop_fmt),
    "`null_prop_str` must be a string" = checkmate::testString(null_prop_str)
  )

  # iterate over colums
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
#' @param col character vector, column names of the df that will be modified
#' @param dt_format string, date time format
#' @export
#'
df_col_dt_format <- function(df, col, dt_format = "%Y-%m-%d %H:%M:%S") {
  # asserts
  stopifnot(
    "`df` must be a data.frame" = checkmate::testDataFrame(df),
    "`col` must be a character vectior" = checkmate::testCharacter(col),
    "`dt_format` must be a string" = checkmate::testString(dt_format)
  )

  for (coln in col) {
    df[, coln] <-
      list(lapply(
        df[, coln],
        function(dt) {
          format(
            as.POSIXct(dt, origin="1970-01-01"),
            dt_format,
            usetz = FALSE
          )
        }
      ))
  }
  df
}
