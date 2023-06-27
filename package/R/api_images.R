#' API images: roxygen template
#'
#' @inherit request params return
#' @param prompt string, a text description of the desired image(s).
#' The maximum length is 1000 characters.
#' @param n integer, the number of images to generate. Must be between 1 and 10.
#' @param size string, the size of the generated images. Must be one of
#' "256x256", "512x512" or "1024x1024"
#' @param response_format string,tThe format in which the generated images
#' are returned. Must be one of "url" or "b64_json".
#' @param user string a unique identifier representing your end-user,
#' which can help OpenAI to monitor and detect abuse.
#' @keywords internal
#'
images_roxygen_tpl <- function(
    api_key,
    prompt,
    n,
    size,
    response_format,
    user
) NULL

#' API images: create request
#'
#' \url{https://platform.openai.com/docs/api-reference/images/create}
#' @inherit images_roxygen_tpl params return
#' @export
#'
images_generator_request <- function(
    api_key,
    prompt,
    response_format = NULL,
    size = NULL,
    n = NULL,
    user = NULL
) {
  # asserts
  stopifnot(
    "`prompt` must be a non-empty string"= checkmate::testString(prompt, min.chars = 1),
    "`response_format` must be a NULL or non-empty string"= checkmate::testString(response_format, min.chars = 1, null.ok = TRUE),
    "`size` must be a NULL or non-empty string"= checkmate::testString(size, min.chars = 1, null.ok = TRUE),
    "`n` must be a NULL or integer" = checkmate::testInt(n, null.ok = TRUE),
    "`user` must be a NULL or non-empty string"= checkmate::testString(user, min.chars = 1, null.ok = TRUE)
  )

  request("https://api.openai.com/v1/images/generations",
    api_key,
    body = list(
      prompt = prompt,
      response_format = response_format,
      size = size,
      n = n,
      user = user
    )
  )
}

#' API images: edit request
#'
#' \url{https://platform.openai.com/docs/api-reference/images/edits}
#' @inherit images_roxygen_tpl params return
#' @param image string/raw, the image to edit. Must be a valid PNG file, less than
#' 4MB, and square. If mask is not provided, image must have transparency,
#' which will be used as the mask.
#' @param mask string/raw/null, an additional image whose fully transparent areas
#' (e.g. where alpha is zero) indicate where image should be edited. Must be
#' a valid PNG file, less than 4MB, and have the same dimensions as `image`.
#' @export
#'
images_edit_request <- function(
    api_key,
    image,
    prompt,
    mask = NULL,
    response_format = NULL,
    size = NULL,
    n = NULL,
    user = NULL
) {
  # asserts
  stopifnot(
    "`image` must be a fle path or raw content" =
      checkmate::testFileExists(image) || checkmate::testRaw(image),
    "`prompt` must be a non-empty string"= checkmate::testString(prompt, min.chars = 1),
    "`mask` must be a fle path or raw content" =
      checkmate::testNull(mask) || checkmate::testFileExists(mask) || checkmate::testRaw(mask),
    "`response_format` must be a NULL or non-empty string"= checkmate::testString(response_format, min.chars = 1, null.ok = TRUE),
    "`size` must be a NULL or non-empty string"= checkmate::testString(size, min.chars = 1, null.ok = TRUE),
    "`n` must be a NULL or integer" = checkmate::testInt(n, null.ok = TRUE),
    "`user` must be a NULL or non-empty string"= checkmate::testString(user, min.chars = 1, null.ok = TRUE)
  )

  files <- c()
  on.exit({
    unlink(files)
  })

  body <- list(
    prompt = prompt,
    response_format = response_format,
    size = size,
    n = n,
    user = user
  )

  if (is.raw(image)) {
    files['image'] <- tempfile()
    writeBin(image, files['image'])
    body$image <- httr::upload_file(files['image'])
  }
  else {
    body$image <- httr::upload_file(image)
  }

  if (is.raw(mask)) {
    files['mask'] <- tempfile()
    writeBin(mask, files['mask'])
    body$mask <- httr::upload_file(files["mask"])
  }
  else if (is.character(mask)) {
    body$mask <- httr::upload_file(mask)
  }

  res_content <- request(
    endpoint = "https://api.openai.com/v1/images/edits",
    api_key = api_key,
    body = body,
    encode = "multipart"
  )
}

#' Test if x is a image set
#'
#' @param x R variable to test
#'
is_image_set <- function(x) {
  fields <- c("data", "prompt", "size")
  is.list(x) &&
    all(names(x) %in% fields) &&
    all(fields %in% names(x)) &&
    is.list(x$data)
}

#' Fetch image set from response content
#'
#' @inherit images_roxygen_tpl params
#' @param res_content response object
#' @export
#'
images_fech_set <- function(res_content, prompt = NULL, size = NULL) {
  list(
    data = res_content$data,
    prompt = prompt,
    size = size
  )
}

#' Merge image set/sets
#'
#' @param ... images set and/or images sets
#' @export
#'
images_merge_sets <- function(...) {
  image_sets <- NULL
  for (x in list(...)) {
    if (is.list(x)) {
      if (is_image_set(x)) x <- list(x)
      image_sets <- c(image_sets, x)
    }
  }
  image_sets
}
