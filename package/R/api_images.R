#' API images: create (generator) request
#'
#' To get more details, visit https://platform.openai.com/docs/api-reference/images/create
#' @inherit request params return
#' @param prompt string, a text description of the desired image(s). The maximum length is 1000 characters for dall-e-2
#' and 4000 characters for dall-e-3.
#' @param model NULL/string, the model to use for image generation. Defaults to `dall-e-2`
#' @param n NULL/int, the number of images to generate. Must be between 1 and 10. For dall-e-3, only n=1 is supported.
#' @param quality NULL/string, the quality of the image that will be generated. `hd` creates images with finer details
#' and greater consistency across the image. This param is only supported for dall-e-3. Defaults to `standard`.
#' @param response_format NULL/string, the format in which the generated images are returned. Must be one of "url" or
#' "b64_json".
#' @param size NULL/string, the size of the generated images. Must be one of 256x256, 512x512, or 1024x1024 for
#' dall-e-2. Must be one of 1024x1024, 1792x1024, or 1024x1792 for dall-e-3 models. 1024x1024 is default.
#' @param style NULL/string, the style of the generated images. Must be one of `vivid` (default) or `natural`. Vivid
#' causes the model to lean towards generating hyper-real and dramatic images. Natural causes the model to produce more
#' natural, less hyper-real looking images. This param is only supported for dall-e-3.
#' @param user string a unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.
#' @export
#'
images_generator_request <- function(
    prompt,
    model = NULL,
    n = NULL,
    quality = NULL,
    response_format = NULL,
    size = NULL,
    style = NULL,
    user = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`prompt` must be a non-empty string" = checkmate::testString(prompt, min.chars = 1),
    "`model` must be a NULL or non-empty string" = checkmate::testString(model, min.chars = 1, null.ok = TRUE),
    "`n` must be a NULL or integer" = checkmate::testInt(n, null.ok = TRUE),
    "`quality` must be a NULL or non-empty string" = checkmate::testString(quality, min.chars = 1, null.ok = TRUE),
    "`response_format` must be a NULL or non-empty string" =
        checkmate::testString(response_format, min.chars = 1, null.ok = TRUE),
    "`size` must be a NULL or non-empty string" = checkmate::testString(size, min.chars = 1, null.ok = TRUE),
    "`style` must be a NULL or non-empty string" = checkmate::testString(style, min.chars = 1, null.ok = TRUE),
    "`user` must be a NULL or non-empty string" = checkmate::testString(user, min.chars = 1, null.ok = TRUE)
  )

  request("https://api.openai.com/v1/images/generations",
    api_key,
    body = list(
      prompt = prompt,
      model = model,
      n = n,
      quality = quality,
      response_format = response_format,
      size = size,
      style = style,
      user = user
    ),
    content_class = "oaii_content_images"
  )
}

#' API images: edit request
#'
#' Creates an edited or extended image given an original image and a prompt. To get more details, visit
#' https://platform.openai.com/docs/api-reference/images/edits
#' @inherit request params return
#' @param image string/raw, the image to edit. Must be a valid PNG file, less than 4MB, and square. If mask is not
#' provided, image must have transparency, which will be used as the mask.
#' @param prompt string, a text description of the desired image(s). The maximum length is 1000 characters.
#' @param mask NULL/string/raw, an additional image whose fully transparent areas (e.g. where alpha is zero) indicate
#' where image should be edited. Must be a valid PNG file, less than 4MB, and have the same dimensions as `image`.
#' @param model NULL/string, the model to use for image generation. Only dall-e-2 is supported at this time.
#' @param n NULL/int, the number of images to generate. Must be between 1 (default) and 10.
#' @param size NULL/string, the size of the generated images. Must be one of 256x256, 512x512, or 1024x1024 (default).
#' @param response_format NULL/string, the format in which the generated images are returned. Must be one of "url" or
#' "b64_json".
#' @param user string a unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.
#' @export
#'
images_edit_request <- function(
    image,
    prompt,
    mask = NULL,
    model = NULL,
    n = NULL,
    size = NULL,
    response_format = NULL,
    user = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`image` must be a fle path or raw content" = checkmate::testFileExists(image) || checkmate::testRaw(image),
    "`prompt` must be a non-empty string" = checkmate::testString(prompt, min.chars = 1),
    "`mask` must be a NULL or fle path, or raw content" =
        is.null(mask) || checkmate::testFileExists(mask) || checkmate::testRaw(mask),
    "`model` must be a NULL or non-empty string" = checkmate::testString(model, min.chars = 1, null.ok = TRUE),
    "`n` must be a NULL or integer" = checkmate::testInt(n, null.ok = TRUE),
    "`size` must be a NULL or non-empty string" = checkmate::testString(size, min.chars = 1, null.ok = TRUE),
    "`response_format` must be a NULL or non-empty string" =
        checkmate::testString(response_format, min.chars = 1, null.ok = TRUE),
    "`user` must be a NULL or non-empty string" = checkmate::testString(user, min.chars = 1, null.ok = TRUE)
  )

  request(
    endpoint = "https://api.openai.com/v1/images/edits",
    api_key = api_key,
    body = list(
      image = api_upload_file(image, "image/png"),
      prompt = prompt,
      mask = api_upload_file(mask, "image/png"),
      model = model,
      n = n,
      size = size,
      response_format = response_format,
      user = user
    ),
    encode = "multipart",
    content_class = "oaii_content_images"
  )
}

#' API images: create image variation request
#'
#' Creates a variation of a given image. To get more details, visit
#' https://platform.openai.com/docs/api-reference/images/createVariation
#' @inherit request params return
#' @param image string/raw, the image to edit. Must be a valid PNG file, less than 4MB, and square
#' @param model NULL/string, the model to use for image generation. Only dall-e-2 is supported at this time.
#' @param n NULL/int, the number of images to generate. Must be between 1 (default) and 10.
#' @param response_format NULL/string, the format in which the generated images are returned. Must be one of "url" or
#' "b64_json".
#' @param size NULL/string, the size of the generated images. Must be one of 256x256, 512x512, or 1024x1024 (default).
#' @param user string a unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.
#' @export
#'
images_variation_request <- function(
    image,
    model = NULL,
    n = NULL,
    response_format = NULL,
    size = NULL,
    user = NULL,
    api_key = api_get_key()
  ) {

  # asserts
  stopifnot(
    "`image` must be a fle path or raw content" = checkmate::testFileExists(image) || checkmate::testRaw(image),
    "`model` must be a NULL or non-empty string" = checkmate::testString(model, min.chars = 1, null.ok = TRUE),
    "`n` must be a NULL or integer" = checkmate::testInt(n, null.ok = TRUE),
    "`response_format` must be a NULL or non-empty string" =
        checkmate::testString(response_format, min.chars = 1, null.ok = TRUE),
    "`size` must be a NULL or non-empty string" = checkmate::testString(size, min.chars = 1, null.ok = TRUE),
    "`user` must be a NULL or non-empty string" = checkmate::testString(user, min.chars = 1, null.ok = TRUE)
  )
  
  request(
    endpoint = "https://api.openai.com/v1/images/variations",
    api_key = api_key,
    body = list(
      image = api_upload_file(image, "image/png"),
      model = model,
      n = n,
      response_format = response_format,
      size = size,
      user = user
    ),
    encode = "multipart",
    content_class = "oaii_content_images"
  )
}

#' Fetch image set from response content
#'
#' To get more details, visit https://platform.openai.com/docs/api-reference/images/create
#' https://platform.openai.com/docs/api-reference/images/edits
#' @param res_content response object returned by \link{images_generator_request} or \link{images_edit_request}
#' @param prompt NULL/string additional info put into the image set object
#' @param size NULL/string additional info put into the image set object
#' @return Image set as a list consisting of three elements: `data`, `prompt` and `size`
#' @export
#'
images_fech_set <- function(res_content, prompt = NULL, size = NULL) {
  list(
    data = res_content$data,
    prompt = prompt,
    size = size
  )
}

#' Test if x is a image set
#'
#' Test if x is a image set - a list consisting of three elements: data, prompt and size
#' @param x R variable to test
#' @return TRUE/FALSE
#'
is_image_set <- function(x) {
  fields <- c("data", "prompt", "size")
  is.list(x) &&
    all(names(x) %in% fields) &&
    all(fields %in% names(x)) &&
    is.list(x$data)
}

#' Merge image set/sets
#'
#' Merge given image set/sets into single images sets object (list with image sets). Have a look at 
#' \link{images_fech_set}.
#' @param ... images set(s), NULL also allowed
#' @return List of image set(s)
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
