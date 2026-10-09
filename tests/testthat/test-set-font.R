text_gpar <- function(node, key, out = character(0)) {
    if (inherits(node, "text")) {
        value <- node$gp[[key]]
        if (!is.null(value)) out <- c(out, as.character(value))
    }
    children <- if (length(node$children)) node$children else node$grobs
    if (!is.null(children)) {
        if (!is.list(children)) children <- as.list(children)
        for (i in seq_along(children)) out <- text_gpar(children[[i]], key, out)
    }
    out
}

plot_gpar <- function(plot, key) {
    text_gpar(ggplot2::ggplotGrob(plot), key)
}

text_nodes <- function(node, n = 0L) {
    if (inherits(node, "text")) n <- n + 1L
    children <- if (length(node$children)) node$children else node$grobs
    if (!is.null(children)) {
        if (!is.list(children)) children <- as.list(children)
        for (i in seq_along(children)) n <- text_nodes(children[[i]], n)
    }
    n
}

guide_text_nodes <- function(plot) {
    g <- ggplot2::ggplotGrob(plot)
    idx <- which(g$layout$name == "guide-box-right")
    if (!length(idx)) return(0L)
    text_nodes(g$grobs[[idx]])
}

colour_of <- function(plot) {
    unique(ggplot2::ggplot_build(plot)$data[[1]]$colour)
}

labelled_plot <- function() {
    df <- data.frame(x = 1:4, y = 1:4, lab = LETTERS[1:4])
    ggplot2::ggplot(df, ggplot2::aes(x, y)) +
        ggplot2::geom_text(ggplot2::aes(label = lab)) +
        ggplot2::labs(title = "plot title")
}

test_that("set_font returns a plot that can still be extended", {
    p <- set_font(labelled_plot(), size = 20)

    expect_s3_class(p, "ggplot")
    expect_identical(p$theme$text$size, 20)

    chained <- p + ggplot2::geom_point()
    expect_s3_class(chained, "ggplot")
    expect_length(chained$layers, 2)
})

test_that("set_font writes size as points and keeps relative sizing", {
    sizes <- as.numeric(plot_gpar(set_font(labelled_plot(), size = 20), "fontsize"))

    expect_gt(length(sizes), 0)
    expect_true(any(sizes == 20))
    # the title inherits rel(1.2), so it must grow with the base size but stay
    # in point scale; multiplying by .pt would put it above 50.
    expect_true(any(sizes > 20 & sizes < 30))
    expect_lt(max(sizes), 30)
})

test_that("set_font reaches theme text and layer text alike", {
    families <- plot_gpar(set_font(labelled_plot(), family = "mono"), "fontfamily")

    expect_gt(length(families), 0)
    expect_true(all(families == "mono"))
})

test_that("set_font keeps the legend of an aesthetic mapped with aes()", {
    df <- data.frame(x = 1:4, y = 1:4, g = c("a", "b", "a", "b"), lab = LETTERS[1:4])
    p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = g)) +
        ggplot2::geom_text(ggplot2::aes(label = lab))
    before <- guide_text_nodes(p)
    expect_gt(before, 0)

    q <- set_font(p, family = "mono", size = 20, color = "red")

    expect_identical(guide_text_nodes(q), before)
    expect_setequal(colour_of(q), colour_of(p))
    expect_true(all(plot_gpar(q, "fontfamily") == "mono"))
})

test_that("set_font does not flatten a size mapped with aes()", {
    df <- data.frame(x = 1:4, y = 1:4, g = c("a", "b", "a", "b"), lab = LETTERS[1:4])
    p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
        ggplot2::geom_text(ggplot2::aes(label = lab, size = g))

    sizes <- suppressWarnings(
        ggplot2::ggplot_build(set_font(p, size = 30))$data[[1]]$size)
    expect_true(all(sizes < 8))
})

test_that("set_font leaves the font family alone unless requested", {
    p <- labelled_plot() + ggplot2::theme(text = ggplot2::element_text(family = "serif"))

    families <- plot_gpar(set_font(p, color = "red"), "fontfamily")
    expect_false("sans" %in% families)
    expect_true(all(families %in% c("serif", "")))

    expect_true(all(plot_gpar(set_font(p, family = "mono"), "fontfamily") == "mono"))
})

test_that("set_font accepts both font face names and grid numeric codes", {
    expect_identical(set_font(labelled_plot(), fontface = "italic")$theme$text$face, "italic")
    expect_identical(set_font(labelled_plot(), fontface = 3)$theme$text$face, "italic")
})

test_that("set_font reports invalid input", {
    p <- labelled_plot()

    expect_error(set_font(p), "At least one")
    expect_error(set_font("not a plot", family = "mono"), "must be a")
})
