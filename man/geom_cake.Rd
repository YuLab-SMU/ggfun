% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_cake.R
\name{geom_cake}
\alias{geom_cake}
\title{geom_cake}
\usage{
geom_cake(mapping = NULL, data = NULL, ...)
}
\arguments{
\item{mapping}{aes mapping}

\item{data}{data}

\item{...}{additional parameters}
}
\value{
ggplot2 layer
}
\description{
ggplot2 layer of birthday cake
}
\examples{
library(ggplot2)
ggplot(mtcars, aes(mpg, disp)) + geom_cake()
library(ggplot2)
ggplot(mtcars, aes(mpg, disp)) + geom_cake()
}
\author{
Guangchuang Yu
}
