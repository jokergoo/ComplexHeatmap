\name{row_dend-Heatmap-method}
\alias{row_dend,Heatmap-method}
\title{
Get row dendrograms from a heatmap
}
\description{
Get row dendrograms from a heatmap
}
\usage{
\S4method{row_dend}{Heatmap}(object)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object}

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
row_dend(ht)
ht = Heatmap(mat, row_km = 2)
ht = draw(ht)
row_dend(ht)
}
