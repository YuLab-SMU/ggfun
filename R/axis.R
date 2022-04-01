
##' @rdname ggrange
##' @export
yrange <- function(gg, type = "limit", region = "panel") {
    ggrange(gg, var = "y", type = type, region = region)
}

##' @rdname ggrange
##' @export
xrange <- function(gg, type = "limit", region = "panel") {
    ggrange(gg, var = "x", type = type, region = region)
}

##' extract x or y ranges of a ggplot
##'
##' 
##' @title plot range of a ggplot object
##' @rdname ggrange
##' @param gg a ggplot object
##' @param var either 'x' or 'y'
##' @param type one of 'limit' or 'range', if 'region == "plot"',
##' to extract plot limit or plot data range
##' @param region one of 'panel' or 'plot' to indicate extracting range
##' based on the plot panel (scale expand will be counted) or
##' plot data (scale expand will not be counted)
##' @return range of selected axis
##' @importFrom ggplot2 ggplot_build
##' @export
##' @author Guangchuang Yu
ggrange <- function(gg, var, type = 'limit', region = 'panel') {
    ## ## https://github.com/YuLab-SMU/aplot/pull/3
    ## ## res <- layer_scales(gg)[[var]]$range$range 
    ## res <- layer_scales(gg)[[var]]$limits
    ## if (is.null(res)) {
    ##     res <- layer_scales(gg)[[var]]$range$range 
    ## }
    ## if (is.character(res)) return(res)

    ## var <- paste0(var, ".range")
    ## ggplot_build(gg)$layout$panel_params[[1]][[var]]

    type <- match.arg(type, c("limit", 'range'))
    region <- match.arg(region, c("panel", "plot"))

    ## var <- paste0("panel_scales_", var)
    ## x <- ggplot_build(gg)$layout[[var]][[1]]
    x <- ggplot_build(gg)$layout[["panel_params"]][[1]]

    if (region == "panel") {
        var2 <- paste0(var, ".range")
        return(x[[var2]])
    }

    if (type == 'limit') {
        res <- x[[var]]$limits
    } else {
        res <- x[[var]]$scale$range$range
    }

    return(res)
}
