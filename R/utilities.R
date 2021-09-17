
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

check_labeller <- utils::getFromNamespace("check_labeller", "ggplot2")

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
