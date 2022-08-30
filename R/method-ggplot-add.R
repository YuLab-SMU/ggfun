#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add facet_set
#' @export
ggplot_add.facet_set <- function(object, plot, object_name){
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
            facet.fun <- eval(parse(text=class(plot$facet)[1]))
            facet.obj <- ggplot2::ggproto(NULL, 
                                      facet.fun, 
                                      shrink = plot$facet$shrink,
                                      params = plot$facet$params
                         )
            if (!is.null(plot$facet$strip)){
                facet.obj$strip <- plot$facet$strip
            }
            strip.labels <- extract_strip_label(facet=facet.fun, plot=plot)
            if (inherits(object$label, "labeller")){
                tmp.label <- extract_strip_label(facet=facet.fun, plot=plot, labeller=object$label)
                names(tmp.label) <- names(strip.labels)
                object$label <- tmp.label[!is.na(tmp.label)]
            }
            newnm <- intersect(names(object$label), names(strip.labels))
            if (length(newnm) > 0){
                strip.labels[match(newnm, names(strip.labels))] <- object$label[match(newnm, names(object$label))]
            }
            facet.obj$params$labeller <- ggplot2::as_labeller(strip.labels)
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

#' set the theme of ggplot object with the striped background style.
#' @param colour character the color of the striped background, 
#' default is c('grey90', 'white').
#' @param axis character which grid of axis will be filled, default is 'y'.
#' @param ... additional parameter, see also 'theme' of 'ggplot2'.
#' @export
#' @examples
#' library(ggplot2)
#' iris |> tidyr::pivot_longer(
#'     cols = !Species, 
#'     names_to = 'var', 
#'     values_to = 'value'
#'   ) |>
#' ggplot(
#'   aes(x=var, y=Species, color=value, size=value)
#' ) + 
#' geom_point() -> p
#' p +
#' theme_stamp(
#'   colour = c('grey90', 'white'), 
#'   axis = 'y',
#'   axis.line.y=element_line()
#' )
#' p +
#' theme_stamp(
#'   colour = c('grey90', 'white'),
#'   axis = 'x',
#'   axis.line.x = element_line()
#' )
theme_stamp <- function(colour=c('grey90', 'white'), axis = 'y', ...){
    params <- list(...)
    axis <- match.arg(axis, c('x', 'y'))
    if ('color' %in% names(params)){
        colour <- params$color
        params$color <- NULL
    }
    if (length(colour)!=2){
        message('The colour is not a vector contained two length.')
        #colour <- c('white', 'grey90')
    }
    structure(
      list(
        colour = colour, 
        axis = axis, 
        params = params
      ), 
      class = 'theme_stamp'
    )
}

#' @method ggplot_add theme_stamp 
#' @export
#' @importFrom ggplot2 element_line geom_tile aes element_blank 
ggplot_add.theme_stamp <- function(object, plot, object_name){
    gb <- ggplot2::ggplot_build(plot)
    axis <- paste0('panel_scales_', object$axis)
    df <- data.frame(AXIS=gb$layout[[axis]][[1]]$get_labels())
    len.ind <- length(object$colour)
    axis.num <- nrow(df)
    df$GROUP.GRID <- rep(object$colour, ceiling(axis.num/len.ind))[seq_len(axis.num)]
    if (object$axis == 'y'){
        grid.tile <- geom_tile(
                       data = df,
                       mapping = aes(x = 1, 
                                     y = !!as.symbol("AXIS"), 
                                     fill = I(!!as.symbol("GROUP.GRID")), 
                                     height = 1,
                                     width=Inf
                                ),
                       inherit.aes = FALSE
                     )
    }else{
        grid.tile <- geom_tile(
                       data = df, 
                       mapping = aes(x = !!as.symbol("AXIS"),
                                     y = 1, 
                                     fill = I(!!as.symbol("GROUP.GRID")), 
                                     height = Inf,
                                     width = 1
                                 ), 
                       inherit.aes = FALSE
                     )
    }
    plot <- plot + ggnewscale::new_scale_fill() + grid.tile
    plot$layers <- c(plot$layers[[length(plot$layers)]], plot$layers[-length(plot$layers)])
    axis.keep <- paste0('axis.line.', setdiff(c('x', 'y'), object$axis))
    default.theme <- list(element_blank(), element_line())
    names(default.theme) <- c('panel.background', axis.keep)
    if (axis.keep %in% names(object$params)){
        object$params <- c(object$params, default.theme[[-2]])
    }else{
        object$params <- c(object$params, default.theme)
    }
    th <- do.call("theme", object$params)
    plot <- plot + th
    return(plot)
}
