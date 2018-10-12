\name{draw_dimnames-Heatmap-method}
\alias{draw_dimnames,Heatmap-method}
\alias{draw_dimnames}
\title{
Draw row names or column names
}
\description{
Draw row names or column names
}
\usage{
\S4method{draw_dimnames}{Heatmap}(object,
    which = c("row", "column"), k = 1, ...)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{which}{Are the names put on the row or on the column of the heatmap?}
  \item{k}{Slice index.}
  \item{...}{Pass to \code{\link[grid]{viewport}} which includes the complete heatmap row/column names.}

}
\details{
A viewport is created which contains row names or column names.

This function is only for internal use.
}
\value{
This function returns no value.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
