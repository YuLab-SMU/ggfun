#' round rectangle borders and backgrounds
#' @inheritParams ggplot2::element_rect
#' @param r the radius of the rounded corners, a \code{unit} object, 
#' default is unit(0.1, 'snpc').
#' @export
#' @examples
#' library(ggplot2)
#' p <- ggplot(mpg, aes(displ, cty)) + geom_point()
#' p <- p + facet_grid(cols = vars(cyl))
#' p <- p + theme(strip.background=element_roundrect(fill="grey40", color=NA, r=0.15))
#' p
#' p2 <- ggplot(mtcars, aes(mpg, disp, color=factor(cyl), size=cyl)) + 
#'       geom_point()
#' p2 + theme(legend.background=element_roundrect(color="#808080", linetype=2))
element_roundrect <- function(fill = NULL, colour = NULL, size = NULL,
  linetype = NULL, color = NULL, r=grid::unit(0.1, "snpc"), inherit.blank = FALSE) {
  if (!is.null(color))  colour <- color
  if (!grid::is.unit(r)) r <- grid::unit(r, 'snpc')
  structure(
    list(fill = fill, 
         colour = colour, 
         size = size, 
         linetype = linetype,
         r = r,
         inherit.blank = inherit.blank),
    class = c("element_roundrect", "element_rect", "element")
  )
}

#' @importFrom ggplot2 element_grob
#' @method element_grob element_roundrect
#' @export
element_grob.element_roundrect <- function(element, 
  x = 0.5, y = 0.5, width = 1, height = 1,
  fill = NULL, colour = NULL, size = NULL, linetype = NULL, 
  ...) {

  gp <- grid::gpar(lwd = len0_null(size * .pt), 
                   col = colour, 
                   fill = fill, 
                   lty = linetype
        )
  element_gp <- grid::gpar(lwd = len0_null(element$size * .pt), 
                           col = element$colour,
                           fill = element$fill, 
                           lty = element$linetype
                )

  grid::roundrectGrob(x, y, width, height, r = element$r, gp = modify_list(element_gp, gp), ...)
}

len0_null <- function (x){
    if (length(x) == 0)
        NULL
    else x
}

modify_list <- function (old, new){
    for (i in names(new)) old[[i]] <- new[[i]]
    old
}
