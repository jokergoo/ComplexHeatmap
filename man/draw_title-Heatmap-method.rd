\name{draw_title-Heatmap-method}
\alias{draw_title,Heatmap-method}
\title{
Draw heatmap title
}
\description{
Draw heatmap title
}
\usage{
\S4method{draw_title}{Heatmap}(object,
    which = c("row", "column"), k = 1, ...)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object.}
  \item{which}{is title put on the row or on the column of the heatmap?}
  \item{k}{a matrix may be split by rows, the value identifies which row-slice.}
  \item{...}{pass to \code{\link[grid]{viewport}}, basically for defining the position of the viewport.}

}
\details{
A viewport is created which contains heatmap title.

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
