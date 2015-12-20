\name{row_order-HeatmapList-method}
\alias{row_order,HeatmapList-method}
\title{
Get row order from a heatmap list
}
\description{
Get row order from a heatmap list
}
\usage{
\S4method{row_order}{HeatmapList}(object)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object}

}
\value{
A list contains row orders which correspond to the original matrix
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(100), 10)
ht_list = Heatmap(mat) + Heatmap(mat)
row_order(ht_list)
ht = Heatmap(mat, km = 2) + Heatmap(mat)
row_order(ht_list)
}
