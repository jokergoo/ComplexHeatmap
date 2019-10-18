\name{row_order-HeatmapList-method}
\alias{row_order,HeatmapList-method}
\title{
Get Row Order from a Heatmap List
}
\description{
Get Row Order from a Heatmap List
}
\usage{
\S4method{row_order}{HeatmapList}(object, name = NULL)
}
\arguments{

  \item{object}{A \code{\link{HeatmapList-class}} object.}
  \item{name}{Name of a specific heatmap.}

}
\value{
The format of the returned object depends on whether rows/columns of the heatmaps are split.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(100), 10)
ht_list = Heatmap(mat) + Heatmap(mat)
ht_list = draw(ht_list)
row_order(ht_list)
ht_list = Heatmap(mat, row_km = 2) + Heatmap(mat)
ht_list = draw(ht_list)
row_order(ht_list)
ht_list = Heatmap(mat, row_km = 2) \%v\% Heatmap(mat)
ht_list = draw(ht_list)
row_order(ht_list)
}
