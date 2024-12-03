##' override point legend set by 'aes(shape = I(shape))'
##'
##'
##' @title set_point_legend_shape
##' @param plot a 'gg' plot object
##' @return an updated plot
##' @importFrom ggplot2 guides
##' @importFrom ggplot2 guide_legend
##' @export
##' @author Guangchuang Yu
set_point_legend_shape <- function(plot) {
    pshape <- get_aes_var(plot$mapping, 'shape')
    if (is.null(pshape) || pshape == "NULL") {
        return(plot)
    }

    pshape <- eval(parse(text = pshape))

    plot + guides(size = guide_legend(override.aes = list(shape = pshape)))          
}

## default point shape for enrichplot
enrichplot_point_shape <- 21


##' extract data from a 'gg' plot
##'
##'
##' @title get_plot_data
##' @param plot a 'gg' plot object
##' @param var variables to be extracted
##' @param layer specific layer to extract the data
##' @return a data frame of selected variables
##' @importFrom cli cli_alert
##' @export
##' @author Guangchuang Yu
get_plot_data <- function(plot, var = NULL, layer = NULL) {
    if (!inherits(plot, 'gg')) {
        stop("'plot' should be a 'gg' object.")
    }

    if (is.null(var)) {
        return(plot$data)
    }


    if (is.null(layer)) {
        ly <- plot
    } else if (is.numeric(layer) && length(layer) == 1) {
        ly <- plot$layers[[layer]]
    } else {
        cli::cli_alert("invalid layer, set to NULL automatically")
        ly <- plot
    }

    d <- ly$data
    if (length(d) == 0) {
        d <- plot$data
    }

    m <- ly$mapping
    
    if (is.null(m)) {
        mapping <- plot$mapping
    } else {
        mapping <- modifyList(plot$mapping, m)
    }

    if (length(d) == 0) {
        cli::cli_alert("No data found.")
        cli::cli_alert("You need to set a proper 'layer' index to locate the layer data.")

        return(NULL)
    }

    var2 <- var
    i <- which(! var2 %in% names(d))

    if (length(i) > 0 && 
        (is.null(mapping) || length(mapping) == 0)
    ) {
        cli::cli_alert("Not aes mapping found.")
        cli::cli_alert("You nedd to set a proper 'layer' index to locate the layer mapping.")

        return(NULL)
    }

    var2[i] <- vapply(X = var2[i], 
        FUN = get_aes_var, 
        FUN.VALUE = character(1),
        mapping = mapping)
    
    d[, var2, drop = FALSE]
}

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
##' @author Guangchuang Yu
get_aes_var <- function(mapping, var) {
    res <- rlang::quo_text(mapping[[var]])

    ## to compatible with ggplot2 v=2.2.2
    res <- tail(res, 1) |>
    ## to compatible with .data[[var]]
        sub('^.data\\[\\[(.*)\\]\\]$', "\\1", x=_) |>
        ## to compatible with .data$var
        sub('^.data\\$(.*)$', "\\1", x=_) |>
        ## to remove quote
        gsub('\\"', "", x=_)

    return(res)
}

#check_labeller <- utils::getFromNamespace("check_labeller", "ggplot2")

extract_strip_label <- function(facet, plot, labeller=NULL){
    layout <- facet$compute_layout(list(plot$data),
                                   c(plot$facet$params,
                                     list(.possible_columns=names(plot$data)),
                                     plot_env = plot$plot_env
                                   )
              )
    label_df <- layout[names(c(plot$facet$params$facet,
                               plot$facet$params$cols,
                               plot$facet$params$rows))]
    if (is.null(labeller)){
        labels <- lapply(plot$facet$params$labeller(label_df), cbind)
    }else{
        labels <- lapply(labeller(label_df), cbind)
    }
    labels <- do.call("cbind", labels)
    labels <- unique(as.vector(labels))
    names(labels) <- labels
    return(labels)
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
