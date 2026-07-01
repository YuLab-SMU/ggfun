##' geom_segment_c supports coloring segment with continuous colors
##'
##'
##' @title geom_segment_c
##' @param mapping aes mapping
##' @param data data
##' @param position position
##' @param lineend lineend
##' @param na.rm logical
##' @param show.legend logical
##' @param inherit.aes logical
##' @param arrow specification for arrow heads, as created by arrow().
##' @param arrow.fill fill color to usse for the arrow head (if closed). `NULL` means use `colour` aesthetic.
##' @param ... additional parameter
##' @importFrom ggplot2 layer
##' @export
##' @seealso
##' \link[ggplot2]{geom_segment}
##' @return add segment layer
##' @examples
##' set.seed(2019-06-28)
##' d = data.frame(x = rnorm(10),
##'               xend = rnorm(10),
##'               y = rnorm(10),
##'               yend = rnorm(10),
##'               v1 = rnorm(10),
##'               v2 = rnorm(10))
##' library(ggplot2)
##' ggplot(d) + geom_segment_c(aes(x = x, xend = xend, y=y, yend =yend, col0 = v1, col1 = v2)) +
##'    scale_color_viridis_c(name = "continuous colored lines") + 
##'    theme_minimal() + theme(legend.position=c(.2, .85)) + xlab(NULL) + ylab(NULL)
##' @author Guangchuang Yu
geom_segment_c <- function(mapping = NULL, data = NULL, 
                           position = 'identity', lineend = "butt",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           arrow = NULL, arrow.fill = NULL, 
                           ...) {

    structure(list(
        data = data,
        mapping = mapping,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            arrow = arrow,
            lineend = lineend,
            na.rm = na.rm,
            ...
        )
    ), class = "segmentC")
}

Stat <- getFromNamespace("Stat", "ggplot2")

##' @importFrom ggplot2 ggplot_add
##' @method ggplot_add segmentC
##' @export
ggplot_add.segmentC <- function(object, plot, object_name, ...) {
    if (object$inherit.aes) {
        mapping <- modifyList(plot$mapping, object$mapping)
    } else {
        mapping <- object$mapping
    }

    v <- get_aes_var(mapping, "col1")

    mapping <- object$mapping
    # mapping["colour"] <- list(v)

    default_aes <- aes(colour = !!rlang::sym(v))
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    ly <- layer(
        data = object$data,
        mapping = mapping,
        stat = StatSegmentC,
        geom = "segment",
        position = object$position,
        show.legend = object$show.legend,
        inherit.aes = object$inherit.aes,
        params = object$params,
        check.aes = FALSE
    )

    ggplot_add(ly, plot, object_name, ...)
}

##' @importFrom ggplot2 Stat
StatSegmentC <- ggproto("StatSegmentC", Stat,
                        required_aes = c("x", "y", "xend", "yend", "col0", "col1"),
                        compute_group = function(data, params) {
                            data
                        },
                        compute_panel = function(self, data, scales, params, lineend, extend = 0.002) {
                            setup_data_continuous_color_df(data, nsplit = 20, extend = extend)
                        }
                        )





setup_data_continuous_color_df <- function(df, nsplit = 100, extend = 0.002, pool = FALSE) {
    if (pool) {
        rr <- range(df$x)
        if (nrow(df) == 1)
            rr <- c(df$x, df$xend)        
    }

    lapply(1:nrow(df), function(i) {
        if (!pool)
            rr <- c(df$x[i], df$xend[i])
        df2 <- setup_data_continuous_color(x = df$x[i],
                                           xend = df$xend[i],
                                           y = df$y[i],
                                           yend = df$yend[i],
                                           col = df$col0[i],
                                           col2 = df$col1[i],
                                           xrange = rr,
                                           nsplit = nsplit,
                                           extend = extend)

        res <- lapply(df[i,, drop = FALSE], rep, each = nrow(df2)) |>
            do.call(what = 'cbind') |> as.data.frame()
        res$x <- df2$x
        res$xend <- df2$xend
        res$y <- df2$y
        res$yend <- df2$yend
        res$colour <- df2$colour
        return(res)
    }) |> do.call(what = 'rbind')
}


## setup_data_continuous_color <- getFromNamespace("setup_data_continuous_color", "ggtree")


setup_data_continuous_color <- function(x, xend, y, yend, col, col2,
                                        xrange = NULL, nsplit = 100, extend = 0.002) {
    if (is.null(xrange))
        xrange <- c(x, xend)

    ydiff <- yend - y
    xdiff <- xend - x

    if (xdiff == 0 && ydiff == 0) {
        n <- 1
    } else {
        span <- if (xdiff == 0) abs(ydiff) else abs(diff(xrange))
        if (span == 0) {
            span <- if (xdiff == 0) abs(ydiff) else abs(xdiff)
        }
        n <- max(1, floor(max(abs(xdiff), abs(ydiff)) * nsplit / span))
    }

    if (n > 1) {
        t0 <- seq(0, 1, length.out = n)
        t1 <- c(t0[-1] * (1 + extend), 1)
        t1[t1 > 1] <- 1
        t1[t1 < 0] <- 0

        x0 <- x
        y0 <- y
        x <- x0 + t0 * xdiff
        xend <- x0 + t1 * xdiff
        y <- y0 + t0 * ydiff
        yend <- y0 + t1 * ydiff
    }

    n <- length(x)
    if (is.numeric(col) && is.numeric(col2)) {
        colour <- seq(col, col2, length.out = n)
    } else if (is.character(col) && is.character(col2)) {
        colour <- grDevices::colorRampPalette(c(col, col2))(n)
    } else {
        stop("col and col2 should be both numeric or character..." )
    }

    data.frame(x = x,
               xend = xend,
               y = y,
               yend = yend,
               colour = colour)
}


