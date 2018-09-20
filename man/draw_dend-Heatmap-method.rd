\name{draw_dend-Heatmap-method}
\alias{draw_dend,Heatmap-method}
\alias{draw_dend}
\title{
Draw Heatmap Dendrograms
}
\description{
Draw Heatmap Dendrograms
}
\usage{
\S4method{draw_dend}{Heatmap}(object,
    which = c("row", "column"), k = 1, max_height = NULL, ...)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{which}{Are the dendrograms put on the row or on the column of the heatmap?}
  \item{k}{Slice index.}
  \item{max_height}{maximal height of dendrogram.}
  \item{...}{Pass to \code{\link[grid]{viewport}} which includes the complete heatmap dendrograms.}

}
\details{
A viewport is created which contains dendrograms.

This function is only for internal use.
}
\value{
This function returns no value.
}
\seealso{
\code{\link{grid.dendrogram}}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
