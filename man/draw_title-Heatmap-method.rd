\name{draw_title-Heatmap-method}
\alias{draw_title,Heatmap-method}
\title{
Draw Heatmap Title
}
\description{
Draw Heatmap Title
}
\usage{
\S4method{draw_title}{Heatmap}(object,
    which = c("row", "column"), k = 1, ...)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{which}{Is title put on the row or on the column of the heatmap?}
  \item{k}{Slice index.}
  \item{...}{Pass to \code{\link[grid]{viewport}} which includes the complete heatmap title.}

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
# There is no example
NULL

}
