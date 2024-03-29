% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_completions.R
\name{completions_fetch_text}
\alias{completions_fetch_text}
\title{Fetch completions text from response content}
\usage{
completions_fetch_text(res_content, role = "ai", ltrim = TRUE)
}
\arguments{
\item{res_content}{response object returned by \link{completions_request}}

\item{role}{string, dialog role (phrase owner)}

\item{ltrim}{flag, trim left white space character(s) from text}
}
\value{
dialog data.frame
}
\description{
Fetch completions text from response content (\link{completions_request})
as dialog data.frame
}
\examples{
\dontrun{
  prompt <- "x=1, y=2, z=x*y, z=?"
  res_content <- completions_request(
    model = "text-davinci-003",
    prompt = prompt
  )
  if (!is_error(res_content)) {
    answer <- completions_fetch_text(res_content)
    print(answer)
  }
}

}
