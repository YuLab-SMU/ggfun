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

