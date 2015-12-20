\name{column_order-HeatmapList-method}
\alias{column_order,HeatmapList-method}
\title{
Get column order from a heatmap list
}
\description{
Get column order from a heatmap list
}
\usage{
\S4method{column_order}{HeatmapList}(object)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object}

}
\value{
A list contains column orders which correspond every matrix
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(100), 10)
ht_list = Heatmap(mat) + Heatmap(mat)
column_order(ht_list)
ht = Heatmap(mat, km = 2) + Heatmap(mat)
column_order(ht_list)
}
