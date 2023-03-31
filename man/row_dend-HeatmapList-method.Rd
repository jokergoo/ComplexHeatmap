\name{row_dend-HeatmapList-method}
\alias{row_dend,HeatmapList-method}
\title{
Get Row Dendrograms from a Heatmap List
}
\description{
Get Row Dendrograms from a Heatmap List
}
\usage{
\S4method{row_dend}{HeatmapList}(object, name = NULL, on_slice = FALSE)
}
\arguments{

  \item{object}{A \code{\link{HeatmapList-class}} object.}
  \item{name}{Name of a specific heatmap.}
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
ht_list = Heatmap(mat) + Heatmap(mat)
ht_list = draw(ht_list)
row_dend(ht_list)
ht_list = Heatmap(mat, row_km = 2) + Heatmap(mat)
ht_list = draw(ht_list)
row_dend(ht_list)
row_dend(ht_list, on_slice = TRUE)
ht_list = Heatmap(mat, row_km = 2) \%v\% Heatmap(mat)
ht_list = draw(ht_list)
row_dend(ht_list)
}
