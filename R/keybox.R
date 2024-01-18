##' draw border for each of the ggplot legends
##'
##' 
##' @title keybox
##' @param p a ggplot object
##' @param grob one of 'rect' or 'roundrect'
##' @param gp graphic parameter
##' @return grob object
##' @export
##' @examples
##' library(ggplot2)
##' p <- ggplot(mtcars, aes(mpg, disp, color=factor(cyl), size=cyl)) + geom_point()
##' keybox(p, 'roundrect', gp = gpar(col = '#808080', lty = "dashed"))
##' @author Guangchuang Yu
keybox <- function(p, grob="roundrect", gp=NULL) {
    warning("This function is deprecated, please refer to 'element_roundrect'.")
    p + theme(legend.background = element_roundrect(colour = gp$col, linetype = gp$lty))
    # g <- ggplot2::ggplotGrob(p)
    # i <- grep("guide-box", g$layout$name)
    # g2 <- g$grob[[i]]
    # for (j in seq_along(g2)) {
    #     x <- g2[[1]][[j]]
    #     if (inherits(x, 'zeroGrob')) next
    #     if (grob == "rect") {
    #         gr <- grid::rectGrob
    #     } else if (grob == "roundrect") {
    #         gr <- grid::roundrectGrob
    #     } else {
    #         stop("grob not supported...")
    #     }

    #     x[[1]][[1]] <- gr(gp = gp)
    #     g2[[1]][[j]] <- x
    # }
    # g[[1]][[i]] <- g2
    # grid::grid.draw(g)
    # invisible(g)
}
