##' extract legend from a plot
##'
##' 
##' @title get_legend
##' @rdname get-legend
##' @param plot a plot produce by grid, lattice, ggplot2 or composite plot
##' @return a 'gtable' object of the legend
##' @importFrom ggplot2 ggplot_gtable
##' @importFrom ggplot2 ggplot_build
##' @export
##' @author Guangchuang Yu
get_legend <- function(plot) {
    if (inherits(plot, 'gg')) {
        gt <- ggplot_gtable(ggplot_build(plot))
    } else {
        ## as.grob <- yulab.utils::get_fun_from_pkg('ggplotify', 'as.grob')
        gt <- ggplotify::as.grob(plot)
    }
    gname <- vapply(gt$grobs, function(x) x$name, FUN.VALUE = character(1))
    idx <- which(gname == "guide-box")
    legend <- gt$grobs[[idx]]
    return(legend)
}
