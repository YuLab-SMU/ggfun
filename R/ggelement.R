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


#' this element is used to control the line color of panel.grid.major/minor.x 
#' or panel.grid.major/minor.y
#' @param colour the colour of rectangular, default is c('white', 'grey60').
#' @param axis character, require, option is \code{y} or \code{x}.
#' @param color, Color is an alias for colour
#' @param inherit.blank Should this element inherit the existence of an
#' \code{element_blank} among its parents? If \code{TRUE} the existence of
#' a blank element among its parents will cause this element to be blank as 
#' well. If \code{FALSE} any blank parent element will be ignored when 
#' calculating final element state.
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'          x = rep(c(2, 5, 7, 9, 12), 2),
#'          y = rep(c(1, 2), each = 5),
#'          z = factor(rep(1:5, each = 2)),
#'          w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
#'        )
#' ggplot(df, aes(x, y)) + geom_tile(aes(fill = z), colour = 'grey50') + 
#' theme(panel.grid.major.y = element_blinds(color= c('white', 'grey'), axis='y'))
element_blinds <- function(colour = c('white', 'grey60'), 
                           axis,
                           color = NULL, 
                           inherit.blank = FALSE){
  if (missing(axis)){
    
  }
  if (!is.null(color))  colour <- color
  structure(
    list(
         colour = colour, 
         axis = axis,
         inherit.blank = inherit.blank),
    class = c("element_blinds", "element_line", "element")
  )
}

#' @importFrom grid gpar polygonGrob
#' @method element_grob element_blinds
#' @export
element_grob.element_blinds <- function(element, x = 0:1, y = 0:1,
                                        colour = NULL, 
                                        default.units = "npc",
                                        id.lengths = NULL,
                                        ...){
  gp <- gpar(
    col = colour
  )
  element_gp <- gpar(
    col = element$colour, 
    fill = element$colour
  )

  xy.coord <- .convert_line_to_poly_coord(x, y, element$axis)
  x <- xy.coord$x
  y <- xy.coord$y

  id.lengths <- rep(4, length(id.lengths))
  
  polygonGrob(
     x, y, default.units = default.units,
     gp = modify_list(element_gp, gp),
     id.lengths = id.lengths, ...
  )

}

.convert_line_to_poly_coord <- function(x, y, axis){
  if (axis == 'x'){
     tmp <- x
     x <- y
     y <- tmp
  }
  
  x <- rep(x, each = 2)
  tmp.range <- max(diff(y)) / 2
  y <- rep(y, each = 2)
  y <- y + rep(c(-1, 1, 1, -1) * tmp.range, length(x)/4)
  y[y < 0] <- 0
  y[y > 1] <- 1

  if (axis == 'x'){
     tmp <- x
     x <- y
     y <- tmp
  }
  return(list(x = x, y = y))
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
