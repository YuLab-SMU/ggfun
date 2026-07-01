test_that("element_blinds creates a valid polygon grob with default id lengths", {
  grob <- ggplot2::element_grob(element_blinds(axis = "y"), x = 0:1, y = 0:1)

  expect_s3_class(grob, "polygon")
  expect_equal(sum(grob$id.lengths), length(grob$x))
})

test_that("element_blinds requires an axis", {
  expect_error(element_blinds(), "'axis' must be specified")
})
