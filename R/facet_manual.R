##' add a facet label to a ggplot or change facet label of a ggplot
##'
##' @title facet_manual
##' @param label a character or a named vector to label the plot
##' @param side to label the plot at which side, either 't' (top) or 'r' (right)
##' @param angle angle of the facet label. Default is 0 for side='t' and -90 for side='r'.
##' @return a ggplot with facet label
##' @export
facet_manual <- function(label, side="t", angle = NULL){
    side <-  match.arg(side, c('top', 'right'))

    structure(list(
        label = label,
        side  = side,
        angle = angle
        ),
        class = "facet_manual"
    )
}
