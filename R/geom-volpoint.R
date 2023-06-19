##' layer of scatter points for volcano plot to visualize differential genes
##'
##' @title geom_volpoint
##' @param mapping aesthetic mapping
##' @param data input data set
##' @param log2FC_cutoff cutoff values for log2FC
##' @param p_cutoff cutoff values p-value or adjusted p-value
##' @param ... additional paramters passed to the layer
##' @return a ggplot
##' @export
geom_volpoint <- function(mapping = NULL, data = NULL, log2FC_cutoff = 2, p_cutoff = 1e-05, ...) {
  structure(list(mapping = mapping,
      data = data,
      log2FC_cutoff = log2FC_cutoff,
      p_cutoff = p_cutoff,
      ...),
    class = "volpoint")
}

##' volcano plot 
##'
##' @title volplot
##' @param data input data set
##' @param mapping aesthetic mapping
##' @param log2FC_cutoff cutoff values for log2FC
##' @param p_cutoff cutoff values p-value or adjusted p-value
##' @param ... additional paramters passed to the 'geom_volpoint' layer
##' @return a ggplot
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 scale_color_manual
##' @importFrom ggplot2 xlab
##' @export
volplot <- function(data, mapping, log2FC_cutoff = 2, p_cutoff = 1e-05, ...) {
    yvar <- ggfun::get_aes_var(mapping, 'y')
    if (grepl("adj", yvar)) { # use adjusted p value
        ylab <- bquote(~Log[2] ~ italic(P[adj]))
        siglab <- bquote(~Log[2] ~ "FC & " ~-Log[10] ~ italic(P[adj]))
    } else {
        ylab <- bquote(~Log[2] ~ italic(P))
        siglab <- bquote(~Log[2] ~ "FC & " ~-Log[10] ~ italic(P))
    }

    ggplot(data, mapping) + 
        geom_volpoint(log2FC_cutoff =log2FC_cutoff, p_cutoff = p_cutoff) +
        scale_color_manual(values=c("red2", "royalblue", "forestgreen", "grey30"), 
                        labels = c(siglab, ylab, bquote(~Log[2] ~ "FC"), "NS"),
                        name="") +
    xlab(bquote(~Log[2] ~ "Fold Change")) +
    ylab(ylab)
}
