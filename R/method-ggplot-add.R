#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add facet_manual
#' @export
ggplot_add.facet_manual <- function(object, plot, object_name){
    if (object$side == 'right' && is.null(object$angle)) {
        object$angle <- -90
    }
    plot <- build_new_plot(object=object, plot=plot)
    return(plot)
}


build_new_plot <- function(object, plot){
    flag.params <- TRUE
    if (!inherits(plot$facet, "FacetNull")){
        if (inherits(object$label, "labeller") || !is.null(names(object$label))){
            facet.obj <- ggplot2::ggproto(NULL, 
                                      eval(parse(text=class(plot$facet)[1])), 
                                      shrink = plot$facet$shrink,
                                      params = plot$facet$params
                         )
            if (inherits(object$label, "labeller")){
                facet.obj$params$labeller <- object$label
            }else{
                facet.obj$params$labeller <- ggplot2::as_labeller(object$label)
            }
            flag.params <- FALSE
        }else{
            plot <- ggplotify::as.ggplot(plot)
        }
    }
    if (flag.params){
        lb <- paste0("'", eval(object$label[1]), "'")
        if (object$side == 'top') {
            params <- list(paste0('~', lb))
        } else {
            params <- list(paste0(lb, '~.'))
        }
    }else{
        params <- NULL
    }
    if (!is.null(params)){
        facet.layer <- do.call("facet_grid", params)
        th <- theme(strip.background = element_rect(fill='grey85', colour = NA),  
                    strip.text = element_text(colour = 'grey10',
                                                size = rel(0.8),
                                                angle = object$angle,
                                                margin = margin(4.4, 4.4, 4.4, 4.4))
                   )
        plot <- plot + facet.layer + th
    }else{
        plot <- plot + facet.obj
    }
    return (plot)
}
