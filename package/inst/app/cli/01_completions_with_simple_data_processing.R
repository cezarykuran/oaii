#
# This script sends four simple queries (simultaneously)
# and then displays the answers provided by AI.
#

# prompt
now <- format(
  as.POSIXct(as.integer(Sys.time()), origin="1970-01-01"),
  "%Y/%m/%d %H:%M:%S",
  usetz = FALSE
)
x2 <- as.integer(runif(1, 0, 100))
x <- as.integer(runif(1, -10, 10))
y <- as.integer(runif(1, -10, 10))
prompt <- c(
  paste0(
    "Problem: x^2 = ", x2, ", solve for x.\nAnswer: x="),
  paste0(
    "Problem x=", x, ", y=", y,", z=x+y, solve for z.\nAnswer: z="),
  paste0(
    "Date time format YYYY/MM/DD hh:mm:ss. Now is ", now, ".\n",
    "Display date and time based on string '2 days, 3 hours, 53 minutes and 43 seconds ago'"
  )
)
cli::cat_line("Prompt:", col = "green")
cli::cli_ul(prompt)
cli::cat_line()

# send request
log <- utils::capture.output({
  res <- oaii::completions_request(
    api_key,
    "text-davinci-003",
    prompt,
    temperature = 0
  )
}, type = "message")

# process response
if (oaii::is_error(res)) {
  # display all logs
  for (n in seq_along(log)) {
    cli::cat_line(cli::col_red(log[n]))
  }
} else {
  # fetch answer from response as data.frame
  text <- oaii::completions_fetch_text(res)

  # dump answer
  cli::cat_line("Answer from AI:", col = "green")
  cli::cli_ul(text$content)
}
