\name{row_dend-HeatmapList-method}
\alias{row_dend,HeatmapList-method}
\title{
Get row dendrograms from a heatmap list
}
\description{
Get row dendrograms from a heatmap list
}
\usage{
\S4method{row_dend}{HeatmapList}(object)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object}

}
\value{
A list of dendrograms for which each dendrogram corresponds to a row slice
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(100), 10)
ht_list = Heatmap(mat) + Heatmap(mat)
row_dend(ht_list)
ht_list = Heatmap(mat, km = 2) + Heatmap(mat)
row_dend(ht_list)
}
