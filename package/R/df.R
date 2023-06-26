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
#' @param cols character vector, column name(s) to be deleted
#' @export
#'
df_exclude_cols <- function(df, cols) {
  df[, -which(names(df) %in% cols)]
}

#' Replace all NULL values in given data.frame
#'
#' @inherit df_roxygen_tpl params return
#' @param replacement string, replacement for NULL
#' @export
#'
df_null_replace <- function(df, replacement = "") {
  df[sapply(df, is.null)] <- replacement
  df
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
#' @param prop_fmt string, sprintf fmt parameter with two `%s` fields (property
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
    prop_fmt = "%s: %s"
) {
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
                    sprintf(prop_fmt, propn, obj[[propn]])
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

# df_obj_cols <- function(df, cols, props, nested = TRUE) {
#   for (col in cols) {
#     df[, col] <-
#       list(lapply(df[, col], function(obj) {
#         if (!nested) obj <- list(obj)
#         paste0(
#           lapply(obj, function(element) {
#             as.character(
#               htmltools::tags$table(
#                 class = "tdTable",
#                 lapply(props, function(prop) {
#                   htmltools::tags$tr(
#                     htmltools::tags$td(prop),
#                     htmltools::tags$td(element[[prop]])
#                   )
#                 })
#               )
#             )
#           }),
#           collapse = as.character(htmltools::hr())
#         )
#       }))
#   }
#   df
# }
