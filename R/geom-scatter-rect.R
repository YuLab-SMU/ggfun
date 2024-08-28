#' draw rectangle boxes as scatter points
#' 
#' @title geom_scatter_rect
#' @param mapping aesthetic mapping, default is NULL
#' @param data input data, default is NULL
#' @param asp aspect ration of rectangle box (height vs width), only works for height is missing
#' @param width width of the rectangles, default is 0.8
#' @param height height of the rectangles
#' @param ... additional parameters passed to 'geom_rect'
#' @importFrom ggplot2 geom_rect
#' @importFrom rlang .data
#' @export 
#' @author Guangchuang Yu 
geom_scatter_rect <- function(
    mapping = NULL,
    data = NULL,
    asp = .6,
    width = .8,
    height = NULL,
    ...) {

    # mostly, it equivalent to geom_tile
    
    params <- list(...)

    structure(
        list(
            data = data,
            mapping = mapping,
            asp     = asp,
            width   = width,
            height  = height,
            params  = params
            ),
        class = 'scatter_rect'
    )
}


#rect <- function(data, coords) {
#    gp <- gpar()
#    if (!is.null(data$fill)) {
#        gp <- modifyList(gp, list(fill=adjustcolor(data$fill)))
#    }
#    rectGrob(
#        coords$x,
#        coords$y, 
#        width = .5, 
#        height = .5,
#        gp = gp)
#}
#
#ggplot(d, aes(x, y)) +  grid_panel(rect, aes(fill=x))


##' @importFrom ggplot2 ggplot_add
##' @method ggplot_add scatter_rect
##' @importFrom utils modifyList
##' @importFrom ggplot2 aes
##' @export
ggplot_add.scatter_rect <- function(object, plot, object_name) {
    w <- object$width
    if (is.null(object$height)) {
        h <- w * object$asp
    } else {
        h <- object$height
    }

    w <- w/2
    h <- h/2

    x <- get_aes_var(plot$mapping, 'x')
    y <- get_aes_var(plot$mapping, 'y')
    default_mapping <- aes(xmin = .data[[x]] - w, xmax = .data[[x]] + w,
                        ymin = .data[[y]] - h, ymax = .data[[y]] + h)
    
    if (!is.null(object$mapping)) {
        mapping <- modifyList(default_mapping, object$mapping)
    } else {
        mapping <- default_mapping
    }

    params <- object$params
    params$mapping <- mapping
    params$data <- object$data

    ly <- do.call("geom_rect", params)
    ggplot_add(ly, plot, object_name)
}

