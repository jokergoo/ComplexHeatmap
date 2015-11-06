\name{anno_density}
\alias{anno_density}
\title{
Using kernel density as annotation
}
\description{
Using kernel density as annotation
}
\usage{
anno_density(x, which = c("column", "row"), gp = gpar(fill = "#CCCCCC"),
    type = c("lines", "violin", "heatmap"), ...)
}
\arguments{

  \item{x}{a matrix or a list. If \code{x} is a matrix and if \code{which} is \code{column}, statistics for density is calculated by columns, if \code{which} is \code{row}, the calculation is by rows.}
  \item{which}{is the annotation a column annotation or a row annotation?}
  \item{gp}{graphic parameters. Note it is ignored if \code{type} equals to \code{heatmap}.}
  \item{type}{which type of graphics is used to represent density distribution.}
  \item{...}{pass to \code{\link[stats]{density}}}

}
\value{
A graphic function which can be set in \code{\link{HeatmapAnnotation}} constructor method.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(32), nrow = 4)
f = anno_density(mat)
grid.newpage(); f(1:8)

f = anno_density(mat, which = "row", type = "violin")
grid.newpage(); f(1:4)

lt = lapply(1:4, function(i) rnorm(8))
f = anno_density(lt, type = "heatmap")
grid.newpage(); f(1:4)

}
