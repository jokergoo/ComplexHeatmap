\name{anno_histogram}
\alias{anno_histogram}
\title{
Using histogram as annotation
}
\description{
Using histogram as annotation
}
\usage{
anno_histogram(x, which = c("column", "row"), gp = gpar(fill = "#CCCCCC"), ...)
}
\arguments{

  \item{x}{a matrix or a list. If \code{x} is a matrix and if \code{which} is \code{column}, statistics for histogram is calculated by columns, if \code{which} is \code{row}, the calculation is by rows.}
  \item{which}{is the annotation a column annotation or a row annotation?}
  \item{gp}{graphic parameters}
  \item{...}{pass to \code{\link[graphics]{hist}}}

}
\value{
A graphic function which can be set in \code{\link{HeatmapAnnotation}} constructor method.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(32), nrow = 4)
f = anno_histogram(mat)
grid.newpage(); f(1:8)

f = anno_histogram(mat, which = "row")
grid.newpage(); f(1:4)

lt = lapply(1:4, function(i) rnorm(8))
f = anno_histogram(lt)
grid.newpage(); f(1:4)

}
