\name{map_to_colors-ColorMapping-method}
\alias{map_to_colors,ColorMapping-method}
\alias{map_to_colors}
\title{
Map Values to Colors
}
\description{
Map Values to Colors
}
\usage{
\S4method{map_to_colors}{ColorMapping}(object, x)
}
\arguments{

  \item{object}{A \code{\link{ColorMapping-class}} object.}
  \item{x}{Input values.}

}
\details{
It maps a vector of values to a vector of colors.

This function provides a uniform way for discrete and continuous color mapping.
}
\value{
A vector of colors.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
cm = ColorMapping(colors = c("A" = "red", "B" = "black"))
map_to_colors(cm, sample(c("A", "B"), 10, replace = TRUE))
require(circlize)
col_fun = colorRamp2(c(0, 1), c("white", "red"))
cm = ColorMapping(col_fun = col_fun)
map_to_colors(cm, runif(10))
}
