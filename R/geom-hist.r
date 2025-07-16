#' @importFrom graphics hist
compute_group_hist <- function(data, scales, breaks) {
    d <- hist(data$x, plot=FALSE, breaks=breaks)
    count = d$count
    data.frame(
        count = count,
        prop = count / sum(abs(count)),
        label = count,
        x = d$mid,
        y = count,
        .size = length(count)
    )
}

StatHist <- ggproto(
    `_class` = "StatHist",
    `_inherit` = ggplot2::Stat,
    compute_group = compute_group_hist,
    breaks="Sturges",
    required_aes = c("x")
)

#' @rdname geom_hist
#' @export
stat_hist <- function(
    mapping = NULL, data = NULL,
    geom = "bar", position = "identity",
    breaks = "Sturges", ..., na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE) {
    layer(mapping = mapping, data = data, 
          geom = geom, stat = StatHist,           
          position = position, show.legend = show.legend, 
          inherit.aes = inherit.aes, 
          params = rlang::list2(na.rm = na.rm, breaks = breaks,...))
}
#' Histogram for continuous data
#'
#' The output of `geom_histogram()` is mostly different from what produced by `hist()`.
#' The `geom_hist()` is designed to  use mid point breaks and counts calculated by `hist`, 
#' and thus produce identical figure with `hist()`.
#' @rdname geom_hist
#' @inheritParams ggplot2::geom_bar
#' @param breaks the number of cells or an algorithm to compute it, see also `hist()`.
#' @return ggplot layer
#' @export
#' @examples 
#' set.seed(42)
#' x <- rnorm(100)
#' dd = data.frame(x = c(x, x+2), type=rep(LETTERS[1:2], each=100))
#' 
#' require(ggplot2)
#' ggplot(dd, aes(x)) + geom_hist(breaks=15, color="steelblue") + 
#'     geom_text(stat="hist", breaks=10, vjust=-1, size=5) 
#' 
#' ggplot(dd, aes(x)) + geom_hist(breaks=15, aes(fill=type), position='dodge')
#' 
geom_hist <- function(
    mapping = NULL, data = NULL,
    stat = "hist", position = "identity",
    breaks = "Sturges", ..., na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE) {
    layer(mapping = mapping, data = data, 
          geom = "bar", stat = stat,           
          position = position, show.legend = show.legend, 
          inherit.aes = inherit.aes, 
          params = rlang::list2(na.rm = na.rm, breaks = breaks,...))
}


# for ggplot2 v4.0.0
# geom_hist <- function(breaks = "Sturges", ...) {
#    stat_manual(geom="bar", fun=compute_group_hist, args=list(breaks=breaks), ...)
#}


