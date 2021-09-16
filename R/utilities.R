
##' extract aes mapping, compatible with ggplot2 < 2.3.0 & > 2.3.0
##'
##'
##' @title get_aes_var
##' @param mapping aes mapping
##' @param var variable
##' @return mapped var
##' @importFrom utils tail
##' @importFrom rlang quo_text
##' @export
##' @author guangchuang yu
get_aes_var <- function(mapping, var) {
    res <- rlang::quo_text(mapping[[var]])
    ## to compatible with ggplot2 v=2.2.2
    tail(res, 1)
}




##' convert a ggbreak object to a ggplot object
##'
##'
##' @title ggbreak2ggplot
##' @param plot a ggbreak object
##' @return a ggplot object
##' @export
##' @author Guangchuang Yu
ggbreak2ggplot <- function(plot) {
    ggplotify::as.ggplot(grid.draw(plot, recording = FALSE))
}

##' check whether a plot is a ggbreak object (including 'ggbreak', 'ggwrap' and 'ggcut' that defined in the 'ggbreak' package)
##'
##'
##' @title is.ggbreak
##' @rdname is-ggbreak
##' @param plot a plot obejct
##' @return logical value
##' @export
##' @author Guangchuang Yu
is.ggbreak <- function(plot) {
    if (inherits(plot, 'ggbreak') ||
        inherits(plot, 'ggwrap') ||
        inherits(plot, 'ggcut')
        ) return(TRUE)

    return(FALSE)
}


##' test whether input object is produced by ggtree function
##'
##'
##' @title is.ggtree
##' @param x object
##' @return TRUE or FALSE
##' @export
##' @author Guangchuang Yu
## copy from treeio
is.ggtree <- function(x) {
    if (inherits(x, 'ggtree')) return(TRUE)

    if (!inherits(x, 'gg')) return(FALSE)

    ## to compatible with user using `ggplot(tree) + geom_tree()`

    tree_layer <- vapply(x$layers,
                         function(y) {
                             any(grepl("StatTree", class(y$stat)))
                         },
                         logical(1)
                         )
    return(any(tree_layer))
}
