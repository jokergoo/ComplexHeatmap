\name{column_dend-Heatmap-method}
\alias{column_dend,Heatmap-method}
\title{
Get Column Dendrograms from a Heatmap
}
\description{
Get Column Dendrograms from a Heatmap
}
\usage{
\S4method{column_dend}{Heatmap}(object, on_slice = FALSE)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{on_slice}{If the value is TRUE, it returns the dendrogram on the slice level.}

}
\value{
The format of the returned object depends on whether rows/columns of the heatmaps are split.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(100), 10)
ht = Heatmap(mat)
ht = draw(ht)
column_dend(ht)
ht = Heatmap(mat, column_km = 2)
ht = draw(ht)
column_dend(ht)
}
