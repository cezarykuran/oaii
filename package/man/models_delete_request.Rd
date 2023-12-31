% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_models.R
\name{models_delete_request}
\alias{models_delete_request}
\title{API models: delete request}
\usage{
models_delete_request(api_key, model)
}
\arguments{
\item{api_key}{string, OpenAI API key
(see \url{https://platform.openai.com/account/api-keys})}

\item{model}{string, the model to delete}
}
\value{
\link[httr]{content} of the httr \link[httr]{response} object
or SimpleError (\link[base]{conditions}) enhanced with
two additional fields: `status_code` (response$status_code)
and `message_long` (built on response content)
}
\description{
\url{https://platform.openai.com/docs/api-reference/fine-tunes/delete-model}
}
