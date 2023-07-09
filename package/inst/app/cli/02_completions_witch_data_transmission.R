#
# A script that takes a data frame as input and allows AI to perform
# computations on floating-point numbers. The resulting data is supplemented
# with differences relative to the expected values and displayed.
#

# input data
df <- data.frame(
  a = 101:110 + runif(10, -20, 20),
  b = 201:210 + runif(10, -20, 20)
)
cli::cat_line("Input data.frame", col = "green")
print(df)
cli::cat_line()

# prompt
json_df <- jsonlite::toJSON(df, auto_unbox = TRUE, digits = 6)
prompt <- paste0(
  "input data in JSON format: ", json_df, "\n",
  "treat columns a and b as the lengths of the legs of a right triangle ",
  "and insert column c with the lengths of the hypotenuses of that triangle ",
  "calculated using the Pythagorean theorem\n",
  "insert column `sum` as the sum of columns a and b\n",
  "display results in json format"
)
cli::cat_line("Prompt:", col = "green")
cli::cat_line(prompt, "\n")

# send request
log <- utils::capture.output({
  res <- oaii::completions_request(
    api_key,
    "text-davinci-003",
    prompt,
    max_tokens = 1000,
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
  # fetch answer(s) from response as data.frame
  text <- oaii::completions_fetch_text(res, ltrim = TRUE)
  df_out <- jsonlite::fromJSON(text$content)

  # add *_diff columns
  df_out$sum_diff <- df_out$sum - (df$a + df$b)
  df_out$c_diff <- df_out$c - sqrt(df$a*df$a + df$b*df$b)

  # dump data
  cli::cat_line(
    "data.frame from AI ",
    "(`*_diff` columns contain differences between values returned by AI and expected)",
    col = "green"
  )
  print(df_out)
}
