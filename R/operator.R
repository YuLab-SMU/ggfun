#' @title %<+%
#' @description 
#' This operator attaches annotation data to a ggtree or ggsc graphic object
#' @rdname attacher
#' @param p ggplot2 object, such as ggtree or ggsc graphic object.
#' @param data data.frame, which must contains a column of \code{node}, 
#' or the first column of taxa labels, when \code{p} is a \code{ggtree} object.
#' Or it must contains columns of \code{.BarcodeID}, when \code{p} is a \code{ggsc}
#' object and \code{p$data} does not contain a column of \code{features}, if it 
#' contains, the \code{data} must also contains a column of \code{features}.
#' @return ggplot object with annotation data added
#' @export
`%<+%` <- function(p, data){
    if (! is.data.frame(data)) {
        cli::cli_abort("right object should be a data.frame...")
    }
    if (missing(data)){
        cli::cli_abort(c(
                "Cannot use {.code %<+%} with a single argument.",
                "i" = "Did you accidentally put {.code %<+%} on a new line?"
        ))
    }
    UseMethod("%<+%")
}


#' @method %<+% ggtree
#' @export
"%<+%.ggtree" <- function(p, data){
    p <- p %add% data
    return(p)
}

#' @method %<+% ggsc
#' @export
"%<+%.ggsc" <- function(p, data){
    if (inherits(p, 'patchwork')){
        p$patches$plots <- lapply(p$patches$plots, function(x){
                                  p <- left_join(x, data)
                                  return(p)}) |>
                           suppressMessages()
    }
    p <- left_join(p, data) |> suppressMessages()
    return(p)
}


`%add%` <- function(p, data) {
    p$data <- p$data %add2% data
    return(p)
}

##' @importFrom dplyr rename
##' @importFrom dplyr left_join
`%add2%` <- function(d1, d2) {
    if ("node" %in% colnames(d2)) {
        cn <- colnames(d2)
        ii <- which(cn %in% c("node", cn[!cn %in% colnames(d1)]))
        d2 <- d2[, ii]
        dd <- dplyr::left_join(d1, d2, by="node")
    }else{
        d2[,1] <- as.character(unlist(d2[,1])) ## `unlist` to work with tbl_df
        d2 <- dplyr::rename(d2, label = 1) ## rename first column name to 'label'
        dd <- dplyr::left_join(d1, d2, by="label")
    }
    dd <- dd[match(d1$node, dd$node),,drop=FALSE]
    return(dd)
}


#' @importFrom dplyr left_join
#' @method left_join ggsc
#' @importFrom cli cli_warn
#' @export
left_join.ggsc <- function(x, y, by = NULL, copy = FALSE, suffix=c("", ".y"), ...){
    dat <- x$data
    msg <- c("The {.arg suffix} requires a character vector containing 2 different elements,",
             "The first element must be \"\", and the second element must not be \"\",",
             "it was set {.code suffix=c(\"\", \".y\")} automatically.")
    if (all(nchar(suffix)!=0)){
        cli::cli_warn(msg)
        suffix[1] = ""
    }
    if (all(nchar(suffix)==0)){
        cli::cli_warn(msg)
        suffix[2] = ".y"
    }
    if (nchar(suffix[1])!=0 && nchar(suffix[2])==0){
        cli::cli_warn(msg)
        suffix <- rev(suffix[seq_len(2)])
    }
    if ('features' %in% names(dat) && length(unique(dat$features))>1 && !'features' %in% names(y)){
        cli::cli_abort(c("The `features` contains in the column of {.cls {class(x)[1]}}, but ",
                         "the {.cls {class(y)[1]}} does not have `features` column."), call = NULL)
    }
    da <- dplyr::left_join(dat, y, by = by, copy = copy, suffix = suffix, ...)

    x$data <- da

    return(x)
}

