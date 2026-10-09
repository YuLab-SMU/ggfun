#' Cubic Bezier edge geometry
#'
#' Draw grouped cubic Bezier paths from four control points per group.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`.
#' @param data A data frame containing `x`, `y`, and `group`.
#' @param n Number of points sampled along each Bezier path.
#' @param stat The statistical transformation used to process the layer data.
#' @param position Position adjustment used to avoid overplotting.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, they are silently removed.
#' @param show.legend Whether this layer should be drawn in the legends.
#' @param inherit.aes Whether to inherit the aesthetic mappings of the plot.
#'   Defaults to `FALSE`, since each group must supply exactly the four
#'   control points of one Bezier curve.
#' @param ... Additional parameters passed to the layer.
#' @importFrom ggplot2 layer aes draw_key_path
#' @importFrom rlang .data
#' @export
#' @examples
#' library(ggplot2)
#' edges <- data.frame(
#'     x = c(0, 0.4, 1.6, 2),
#'     y = c(0, 1.2, 1.2, 0),
#'     group = 1
#' )
#' ggplot() + geom_bezier(aes(x, y, group = group), data = edges)
geom_bezier <- function(mapping = NULL, data = NULL, n = 100,
                        stat = "identity", position = "identity",
                        na.rm = FALSE, show.legend = NA,
                        inherit.aes = FALSE, ...) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomBezier,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(n = n, na.rm = na.rm, ...)
    )
}

#' @rdname geom_bezier
#' @format NULL
#' @usage NULL
#' @export
GeomBezier <- ggplot2::ggproto("GeomBezier", ggplot2::Geom,
    required_aes = c("x", "y", "group"),
    default_aes = ggplot2::aes(
        colour = "black", linewidth = 0.5, linetype = 1,
        alpha = NA
    ),
    draw_panel = function(data, panel_params, coord, n = 100) {
        coords <- coord$transform(data, panel_params)
        groups <- split(coords, coords$group)
        grobs <- lapply(groups, function(d) {
            if (nrow(d) != 4) {
                stop("geom_bezier() requires exactly four control points per group.")
            }
            t <- seq(0, 1, length.out = n)
            b0 <- (1 - t)^3
            b1 <- 3 * (1 - t)^2 * t
            b2 <- 3 * (1 - t) * t^2
            b3 <- t^3
            x <- b0 * d$x[1] + b1 * d$x[2] + b2 * d$x[3] + b3 * d$x[4]
            y <- b0 * d$y[1] + b1 * d$y[2] + b2 * d$y[3] + b3 * d$y[4]
            grid::polylineGrob(
                x, y, default.units = "native",
                gp = grid::gpar(
                    col = scales::alpha(d$colour[1], d$alpha[1]),
                    lwd = d$linewidth[1] * ggplot2::.pt,
                    lty = d$linetype[1]
                )
            )
        })
        ggplot2:::ggname("geom_bezier", do.call(grid::grobTree, grobs))
    },
    draw_key = ggplot2::draw_key_path
)
