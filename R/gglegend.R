##' add manual setting legend
##'
##' add additional legend to a ggplot
##' @title gglegend
##' @param mapping aes mapping for the 'geom'. The first mapping should be the one for the legend,
##' while others maybe needed for the 'geom' (e.g., label for geom_text). 
##' @param data input data frame. If users want to mapping 'VALUE' to 'colour',
##' the input data should contains 'VALUE' and 'colour' (actual value, e.g., 'red' and 'blue') variable.
##' @param geom a geom to plot the data for generating the legend and the geom will be plotted invisible.
##' @param p a ggplot object. If NULL, the 'last_plot()' will be used.
##' @return a ggplot object
##' @importFrom utils modifyList
##' @importFrom grid grid.draw
##' @importFrom ggplot2 guides
##' @importFrom ggplot2 guide_legend
##' @export
##' @examples
##' library(ggplot2)
##' p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
##' data <- data.frame(colour = c("red",  "blue"), VALUE = c("A", "B"))
##' gglegend(aes(colour = VALUE, label=VALUE), data, geom_text, p)
##' @author Guangchuang Yu
gglegend <- function(mapping, data, geom, p = NULL) {
    if (is.null(p)) p <- ggplot2::last_plot()

    xvar <- get_aes_var(p$mapping, 'x')
    yvar <- get_aes_var(p$mapping, 'y')


    dd <- cbind(data, p$data[1:nrow(data), c(xvar, yvar)])
    m <- names(mapping[1])

    var <- data[[m]]
    names(var) <- data[[get_aes_var(mapping,m)]]

    a <- list(var,  alpha = 1)
    names(a)[1] <- m
    leg <- guide_legend(override.aes=a)
    gleg <- list(1)
    names(gleg) <- m
    gleg[[1]] <- leg
    gleg <- do.call(guides, gleg)

    mapping <- modifyList(mapping, p$mapping[c('x', 'y')])
    p + geom(mapping, dd, alpha=0)  + gleg
}


