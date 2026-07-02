test_that("set_font returns an edited grob invisibly", {
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "mtcars")

  expect_s3_class(
    set_font(p, family = "serif", fontface = "italic", color = "firebrick"),
    "gtable"
  )
})
