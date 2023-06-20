##' transparent background theme
##'
##'
##' @title theme_transparent
##' @param ... additional parameter to tweak the theme
##' @return ggplot object
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_rect
##' @export
##' @author Guangchuang Yu with contributions from Hugo Gruson
theme_transparent <- function (...){
    theme(panel.background = element_rect(fill = "transparent",
        colour = NA), plot.background = element_rect(fill = "transparent",
        colour = NA), legend.key = element_rect(fill = "transparent",
        colour = NA), legend.background = element_rect(fill = "transparent",
        colour = NA), ...)
}

##' A theme that only show the plot panel
##'
##'
##' @title theme_nothing
##' @param base_size font size
##' @param base_family font family
##' @importFrom ggplot2 %+replace%
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 theme_void
##' @return ggplot2 theme
##' @export
##' @author Guangchuang Yu
theme_nothing <- function(base_size = 11, base_family = "") {
    theme_void(base_size = base_size, base_family = base_family) %+replace%
    theme(plot.margin=grid::unit(c(0,0, -.2, -.2), "lines"))
}

##' A theme that only show y-axis
##'
##'
##' @title theme_noxaxis
##' @param color color of y-axis
##' @param ... additional parameters that passed to theme()
##' @return ggplot2 theme
##' @importFrom ggplot2 element_blank element_line
##' @export
##' @author Guangchuang Yu
theme_noxaxis <- function(color = 'black', ...) {
    theme(axis.line.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.line.y = element_line(color = color), 
        axis.text.y = element_text(color = color), 
        axis.ticks.y = element_line(color = color),
        ...)
}

##' the theme of blind-like
##' @param colour the colour of rectangular, default is c('white', 'grey60'). 
##' @param axis character which grid of axis will be filled, default is 'y'. 
##' @param ... additional parameters that passed to \code{theme} function.
##' @return ggplot2 theme
##' @export
##' @examples
##' library(ggplot2)
##' iris |> tidyr::pivot_longer(
##'     cols = !Species,
##'     names_to = 'var',
##'     values_to = 'value'
##'   ) |>
##' ggplot(
##'   aes(x=var, y=Species, color=value, size=value)
##' ) +
##' geom_point() -> p
##' p +
##' theme_blinds(
##'   colour = c('grey90', 'white'),
##'   axis = 'y',
##'   axis.line.y=element_line()
##' )
##' p +
##' theme_blinds(
##'   colour = c('grey90', 'white'),
##'   axis = 'x',
##'   axis.line.x = element_line()
##' )
theme_blinds <- function(colour = c('white', 'grey'), axis = 'y', ...){
    dots <- list(...)
    if ('color' %in% names(dots)){
        colour <- dots$color  
    }
    dots[[paste0("panel.grid.major.", axis)]] <- element_blinds(colour = colour, axis = axis)
    do.call("theme", dots)
}

#' the theme of blind-like alias of theme_blinds
#' @param colour the colour of rectangular, default is c('white', 'grey60').
#' @param axis character which grid of axis will be filled, default is 'y'.
#' @param ... additional parameters that passed to \code{theme} function.
#' @export
theme_stamp <- theme_blinds
