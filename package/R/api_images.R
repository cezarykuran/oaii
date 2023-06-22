IMAGES_REQ_PARAMS <- list(
  n = 1:10,
  size = c("256x256", "512x512", "1024x1024"),
  response_format = c("url", "b64_json")
)

#' @export
images_req_params <- function() {
  IMAGES_REQ_PARAMS
}

#' @export
images_generator_request <- function(
    api_key,
    prompt,
    response_format = "b64_json",
    size = "256x256",
    n = 1
) {
  request("https://api.openai.com/v1/images/generations",
    api_key,
    body = list(
      prompt = prompt,
      response_format = response_format,
      size = size,
      n = n
    )
  )
}

#' @export
images_edit_request <- function(
    api_key,
    image,
    prompt,
    mask = NULL,
    response_format = "b64_json",
    size = "256x256",
    n = 1
) {

  files <- c()
  on.exit({
    unlink(files)
  })

  body <- list(
    prompt = prompt,
    response_format = response_format,
    size = size,
    n = n
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

is_image_set <- function(x) {
  fields <- c("data", "prompt", "size")
  is.list(x) &&
    all(names(x) %in% fields) &&
    all(fields %in% names(x)) &&
    is.list(x$data)
}

#' @export
images_fech_set <- function(res_content, prompt = NULL, size = NULL) {
  list(
    data = res_content$data,
    prompt = prompt,
    size = size
  )
}

#' @export
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
