\name{column_dend-HeatmapList-method}
\alias{column_dend,HeatmapList-method}
\title{
Get column dendrograms from a heatmap list
}
\description{
Get column dendrograms from a heatmap list
}
\usage{
\S4method{column_dend}{HeatmapList}(object)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object}

}
\value{
A list of dendrograms for which dendrogram corresponds to each matrix
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(100), 10)
ht_list = Heatmap(mat) + Heatmap(mat)
column_dend(ht_list)
ht_list = Heatmap(mat, km = 2) + Heatmap(mat)
column_dend(ht_list)
}
