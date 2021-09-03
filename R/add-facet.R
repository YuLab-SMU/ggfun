##' add a facet label to a ggplot
##'
##' add a facet label to a ggplot which only contains 1 panel
##' @title add_facet
##' @param plot a ggplot object
##' @param label a string to label the plot
##' @param side to label the plot at which side, either 't' (top) or 'r' (right)
##' @param angle angle of the facet label. Default is 0 for side='t' and -90 for side='r'.
##' @return a ggplot with facet label
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 margin
##' @importFrom ggplot2 element_rect
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 rel
##' @importFrom ggplot2 facet_grid
##' @export
##' @author Guangchuang Yu
add_facet <- function(plot, label, side = 't', angle = NULL) {
    side <- match.arg(side, c('t', 'r'))
    lb <- paste0("'", eval(label), "'")
    if (side == 't') {
        lb <- paste0('~', lb)
    } else {
        lb <- paste0(lb, '~.')
        if (is.null(angle))  angle <- -90
    }

    plot + facet_grid(eval(parse(text=lb))) +
        theme(strip.background = element_rect(fill='grey85', colour = NA),
              strip.text = element_text(colour = 'grey10',
                                        size = rel(0.8),
                                        angle = angle,
                                        margin = margin(4.4, 4.4, 4.4, 4.4))
              )
}


