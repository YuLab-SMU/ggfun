##' add a facet label to a ggplot
##'
##' add a facet label to a ggplot which only contains 1 panel
##' @title add_facet
##' @param plot a ggplot object
##' @param label a string to label the plot
##' @return a ggplot with facet label
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 margin
##' @importFrom ggplot2 element_rect
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 rel
##' @importFrom ggplot2 facet_grid
##' @export
##' @author Guangchuang Yu
add_facet <- function(plot, label) {
    plot + facet_grid(eval(parse(text=paste0("~'", eval(label), "'")))) +
        theme(strip.background = element_rect(fill='grey85', colour = NA),
              strip.text = element_text(colour = 'grey10', size = rel(0.8),
                                        margin = margin(4.4, 4.4, 4.4, 4.4))
              )
}

