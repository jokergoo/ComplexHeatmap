\name{draw_heatmap_body-Heatmap-method}
\alias{draw_heatmap_body,Heatmap-method}
\alias{draw_heatmap_body}
\title{
Draw the heatmap body
}
\description{
Draw the heatmap body
}
\usage{
\S4method{draw_heatmap_body}{Heatmap}(object, k = 1, ...)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object.}
  \item{k}{a matrix may be split by rows, the value identifies which row-slice.}
  \item{...}{pass to \code{\link[grid]{viewport}}, basically for defining the position of the viewport.}

}
\details{
The matrix can be split into several parts by rows if \code{km} or \code{split} is 
specified when initializing the \code{\link{Heatmap}} object. If the matrix is split, 
there will be gaps between rows to identify different row-slice.

A viewport is created which contains subset rows of the heatmap.

This function is only for internal use.
}
\value{
This function returns no value.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# no example for this internal method
NULL

}
