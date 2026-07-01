test_that("%<+% reports a helpful error when data is missing", {
  p <- structure(list(), class = "ggtree")

  expect_error(
    `%<+%`(p),
    "Cannot use .*%<\\+%.* with a single argument"
  )
})
