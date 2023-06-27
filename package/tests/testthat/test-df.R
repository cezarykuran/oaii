test_that("df_exclude_col", {
  df <- data.frame(
    a = 1:10,
    b = 11:20,
    c = 21:30
  )
  expect_equal(
    df_exclude_col(df, "b"),
    df[, c("a", "c")]
  )
})

test_that("df_null_replace", {
  expect_equal(
    df_order_by_col(
      data.frame(
        a = 1:10,
        b = 20:11
      ),
      "b"
    ),
    data.frame(
      a = 10:1,
      b = 11:20,
      row.names = 10:1
    )
  )

  expect_equal(
    df_order_by_col(
      data.frame(
        a = 1:10,
        b = 20:11
      ),
      "a",
      decreasing = TRUE
    ),
    data.frame(
      a = 10:1,
      b = 11:20,
      row.names = 10:1
    )
  )
})

test_that("df_col_obj_implode", {
  df_in <-
    as.data.frame(do.call(cbind,
      list(
        a = list(list(x = 1, y = 2), list(x = 3, y = 4)),
        b = list("z", "z")
      )
    ))
  df_out <-
    as.data.frame(do.call(cbind,
      list(
        a = list("x: 1", "x: 3"),
        b = list("z", "z")
      )
    ))

  expect_equal(
    df_col_obj_implode(df_in, "a", "x", FALSE),
    df_out
  )
})

test_that("df_col_dt_format", {
  df_in <- data.frame(
    a = 1:3,
    dt = 1687868601:1687868603
  )

  df_out <- data.frame(a = 1:3)
  df_out$dt <- list("2023.06.27 14:23:21", "2023.06.27 14:23:22", "2023.06.27 14:23:23")

  expect_equal(
    df_col_dt_format(df_in, "dt", "%Y.%m.%d %H:%M:%S"),
    df_out
  )
})
