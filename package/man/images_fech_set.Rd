% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_images.R
\name{images_fech_set}
\alias{images_fech_set}
\title{Fetch image set from response content}
\usage{
images_fech_set(res_content, prompt = NULL, size = NULL)
}
\arguments{
\item{res_content}{response object returned by
\link{images_generator_request} or \link{images_edit_request}}

\item{prompt}{string, a text description of the desired image(s).
The maximum length is 1000 characters.}

\item{size}{string, the size of the generated images. Must be one of
"256x256", "512x512" or "1024x1024"}
}
\value{
Image set as a list consisting of three elements: `data`, `prompt`
and `size`
}
\description{
\url{https://platform.openai.com/docs/api-reference/images/create}
\url{https://platform.openai.com/docs/api-reference/images/edits}
}
