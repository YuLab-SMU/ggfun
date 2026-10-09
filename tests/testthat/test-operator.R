test_that("%<+% reports a helpful error when data is missing", {
  p <- structure(list(), class = "ggtree")

  expect_error(
    `%<+%`(p),
    "Cannot use .*%<\\+%.* with a single argument"
  )
})

test_that("%<+% keeps the joined data when the plot data has no node column", {
  p <- structure(
    list(data = data.frame(label = c("a", "b"), x = 1:2)),
    class = c("ggtree", "gg")
  )

  out <- p %<+% data.frame(label = c("a", "b"), v = c(10, 20))
  expect_equal(nrow(out$data), 2)
  expect_equal(out$data$v, c(10, 20))
})

test_that("%<+% reorders node-based joins back to the plot data order", {
  p <- structure(
    list(data = data.frame(node = c(2, 1), label = c("b", "a"))),
    class = c("ggtree", "gg")
  )

  out <- p %<+% data.frame(node = c(1, 2), v = c(10, 20))
  expect_equal(out$data$node, c(2, 1))
  expect_equal(out$data$v, c(20, 10))
})
