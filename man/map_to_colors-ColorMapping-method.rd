\name{map_to_colors-ColorMapping-method}
\alias{map_to_colors,ColorMapping-method}
\alias{map_to_colors}
\title{
Map values to colors
}
\description{
Map values to colors
}
\usage{
\S4method{map_to_colors}{ColorMapping}(object, x)
}
\arguments{

  \item{object}{a \code{\link{ColorMapping-class}} object.}
  \item{x}{input values.}

}
\details{
It maps a vector of values to a vector of colors.
}
\value{
A vector of colors.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# discrete color mapping for characters
cm = ColorMapping(name = "test",
    colors = c("blue", "white", "red"),
    levels = c("a", "b", "c"))
map_to_colors(cm, "a")
map_to_colors(cm, c("a", "a", "b"))

# discrete color mapping for numeric values
cm = ColorMapping(name = "test",
    colors = c("blue", "white", "red"),
    levels = c(1, 2, 3))
map_to_colors(cm, 1)
map_to_colors(cm, "1")
map_to_colors(cm, c(1, 1, 2, 2))

# continuous color mapping
require(circlize)
cm = ColorMapping(name = "test",
    col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red")))
map_to_colors(cm, 0.2)
map_to_colors(cm, seq(0.2, 0.8, by = 0.1))

}
