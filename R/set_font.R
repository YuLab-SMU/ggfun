##' setting font for ggplot (axis text, label, title, etc.)
##'
##' The requested values are written into the plot theme, so text elements that
##' inherit from \code{theme(text=)} keep their relative sizing (\code{rel()}).
##' Text drawn by a layer (\code{geom_text()}, \code{geom_label()},
##' \code{ggtree::geom_tiplab()}, ...) does not follow the plot theme, so for
##' those layers the values become layer parameters. An aesthetic that is
##' already mapped with \code{aes()}, either in the layer or in the plot, is
##' left alone: \code{ggplot2} would otherwise let the constant parameter
##' replace the mapped values and drop the corresponding legend along with it.
##' Apply it to a finished plot, because a theme added afterwards can overwrite
##' these settings.
##'
##' @title set_font
##' @param p ggplot object
##' @param family font family; \code{NULL} (the default) keeps the family
##' already used by the plot unchanged
##' @param fontface font face, e.g. \code{"bold"}, \code{"italic"} or the
##' \code{grid} numeric codes 1 to 4
##' @param size font size in points, the same unit used by
##' \code{\link[ggplot2]{element_text}}
##' @param color font color
##' @return A modified ggplot object, which can still be extended with \code{+}.
##' @importFrom ggplot2 element_text theme
##' @importFrom ggplot2 .pt
##' @importFrom utils modifyList
##' @export
##' @examples
##' library(ggplot2)
##' d <- data.frame(x=rnorm(10), y=rnorm(10), lab=LETTERS[1:10])
##' p <- ggplot(d, aes(x, y)) + geom_text(aes(label=lab), size=5)
##' set_font(p, family="Times", fontface="italic", color='firebrick')
##' @author Guangchuang Yu
set_font <- function(p, family=NULL, fontface=NULL, size=NULL, color=NULL) {
    if (is.null(family) && is.null(fontface) && is.null(size) && is.null(color)) {
        cli::cli_abort(c(
            "{.arg family}, {.arg fontface}, {.arg size} and {.arg color} are all {.code NULL}.",
            "i" = "At least one of them must be provided."))
    }
    if (!inherits(p, "ggplot")) {
        cli::cli_abort(c(
            "{.arg p} must be a {.cls ggplot} object, not {.obj_type_friendly {p}}.",
            "i" = "{.code set_font()} modifies the plot object, so pass the plot itself."))
    }

    face <- fontface
    if (is.numeric(face)) {
        face <- c("plain", "bold", "italic", "bold.italic")[face]
    }
    theme_el <- Filter(function(x) !is.null(x),
                       list(family = family, face = face, size = size, colour = color))
    ## An element that carries its own values, in the plot theme or in the theme
    ## currently in effect, wins over the inherited theme(text=); those get the
    ## same treatment explicitly. Sizes given as rel() stay relative.
    current <- modifyList(theme_get(), p$theme)
    sweep <- list()
    for (nm in names(current)) {
        el <- current[[nm]]
        if (identical(nm, "text") || !inherits(el, "element_text")) next
        vals <- theme_el
        if (!is.null(size) && (is.null(el$size) || inherits(el$size, "rel"))) {
            vals$size <- NULL
        }
        vals <- vals[!vapply(vals, is.null, logical(1))]
        if (length(vals)) sweep[[nm]] <- do.call(element_text, vals)
    }
    p <- p + do.call(theme, c(list(text = do.call(element_text, theme_el)), sweep))

    # text drawn by a layer (geom_text, geom_label, geom_tiplab, ...) does not
    # follow the plot theme, and its size is in millimetres rather than points.
    layer_par <- Filter(function(x) !is.null(x),
                        list(family = family, fontface = fontface, colour = color,
                             size = if (is.null(size)) NULL else size / .pt))
    plot_mapped <- mapped_aes(p$mapping)
    p$layers <- lapply(p$layers, function(l) {
        if ("family" %in% names(l$geom$default_aes)) {
            skip <- c(mapped_aes(l$mapping), plot_mapped)
            par <- layer_par[!names(layer_par) %in% skip]
            l$aes_params <- modifyList(l$aes_params, par)
        }
        l
    })

    p
}

## ggplot2 lets a fixed layer parameter replace a mapped aesthetic, and the
## corresponding guide is dropped along with it.
mapped_aes <- function(mapping) {
    nm <- names(mapping)
    nm <- nm[!is.na(nm) & nzchar(nm)]
    nm[nm == "color"] <- "colour"
    nm
}
