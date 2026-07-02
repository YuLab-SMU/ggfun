test_that("geom_segment_c supports vertical segments", {
  df <- data.frame(
    x = 1,
    xend = 1,
    y = 0,
    yend = 1,
    col0 = 0,
    col1 = 1
  )

  p <- ggplot2::ggplot(df) +
    geom_segment_c(
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend, col0 = col0, col1 = col1)
    )

  built <- ggplot2::ggplot_build(p)$data[[1]]

  expect_gt(nrow(built), 1)
  expect_true(all(built$x == 1))
  expect_true(all(built$xend == 1))
  expect_false(any(is.na(built$y)))
  expect_false(any(is.na(built$yend)))
})

test_that("geom_segment_c supports reversed horizontal segments", {
  df <- data.frame(
    x = 2,
    xend = 1,
    y = 1,
    yend = 1,
    col0 = 0,
    col1 = 1
  )

  p <- ggplot2::ggplot(df) +
    geom_segment_c(
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend, col0 = col0, col1 = col1)
    )

  built <- ggplot2::ggplot_build(p)$data[[1]]

  expect_gt(nrow(built), 1)
  expect_true(all(built$y == 1))
  expect_true(all(built$yend == 1))
  expect_false(any(is.na(built$x)))
  expect_false(any(is.na(built$xend)))
})

test_that("geom_segment_c exposes nsplit to control segment expansion", {
  df <- data.frame(
    x = 0,
    xend = 1,
    y = 0,
    yend = 1,
    col0 = 0,
    col1 = 1
  )

  p <- ggplot2::ggplot(df) +
    geom_segment_c(
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend, col0 = col0, col1 = col1),
      nsplit = 5
    )

  built <- ggplot2::ggplot_build(p)$data[[1]]

  expect_equal(nrow(built), 5)
})
