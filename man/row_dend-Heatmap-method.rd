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
A list of dendrograms for which each dendrogram corresponds to a row slice
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(100), 10)
ht = Heatmap(mat)
row_dend(ht)
ht = Heatmap(mat, km = 2)
row_dend(ht)
}
