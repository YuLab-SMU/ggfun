


#' @title X-Spline Geometry for ggplot2
#' @description Draw an X-spline through control points with proper grouping support
#' @inheritParams ggplot2::geom_line
#' @param shape A numeric vector of values between -1 and 1, which
#'        control the shape of the spline relative to the control points.
#' @param open A logical value indicating whether the spline is an open or a
#'        closed shape.
#' @param rep_ends For open X-splines, a logical value indicating whether the
#'        first and last control points should be replicated for drawing the
#'        curve. Ignored for closed X-splines.
#' @importFrom grid xsplineGrob gpar
#' @importFrom scales alpha
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 draw_key_path
#' @export
#' @examples 
#' library(ggplot2)
#' 
#' set.seed(123)
#' df <- data.frame(
#'   x = 1:10,
#'   y = cumsum(rnorm(10))
#' )
#' 
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_xspline(color = "blue", linewidth = 1.2, shape=1)
#' 
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_xspline(color = "blue", linewidth = 1.2, shape=-1)
#' 
#' 
#' df2 <- data.frame(
#'   x = rep(1:10, 2),
#'   y = c(cumsum(rnorm(10)), cumsum(rnorm(10))),
#'   group = rep(c("A", "B"), each = 10)
#' )
#' 
#' ggplot(df2, aes(x, y, color = group, group = group)) +
#'   geom_point() +
#'   geom_xspline(linewidth = 1) +
#'   scale_color_manual(values = c("A" = "tomato", "B" = "steelblue"))
geom_xspline <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE, 
                         shape = 0, open = TRUE, rep_ends = TRUE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    geom = GeomXspline,
    mapping = mapping,
    data = data, 
    stat = stat, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      shape = shape,
      open = open,
      rep_ends = rep_ends,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_xspline
#' @format NULL
#' @usage NULL
#' @export
GeomXspline <- ggplot2::ggproto("GeomXspline", ggplot2::GeomLine,
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black", 
    linewidth = 0.5, 
    linetype = 1,
    alpha = NA,
    fill = NA,
    group = NULL
  ),
  
  draw_panel = function(data, panel_params, coord, shape = 0, open = TRUE, rep_ends = TRUE) {
    # Transform the data
    coords <- coord$transform(data, panel_params)
    
    # If no grouping, treat as one group
    if (is.null(coords$group)) {
      coords$group <- 1
    }
    
    # Split data by group
    groups <- split(coords, coords$group)
    
    # Recycle shape parameter within each group
    if (length(shape) == 1) {
      shape <- rep(shape, max(sapply(groups, nrow)))
    }
    
    # Create one grob per group
    grobs <- lapply(groups, function(group_data) {
      if (nrow(group_data) < 2) return(grid::nullGrob())
      
      # Recycle shape for this group
      group_shape <- if (length(shape) == 1) {
        rep(shape, nrow(group_data))
      } else {
        shape[1:nrow(group_data)]
      }
      
      grid::xsplineGrob(
        x = group_data$x,
        y = group_data$y,
        shape = group_shape,
        open = open,
        repEnds = rep_ends,
        default.units = "native",
        gp = grid::gpar(
          col = scales::alpha(group_data$colour, group_data$alpha)[1],
          fill = scales::alpha(group_data$fill, group_data$alpha)[1],
          lwd = group_data$linewidth[1] * .pt,
          lty = group_data$linetype[1]
        )
      )
    })
    
    # Combine all grobs
    ggplot2:::ggname("geom_xspline", do.call(grid::grobTree, grobs))
  },
  
  draw_key = draw_key_path
)

