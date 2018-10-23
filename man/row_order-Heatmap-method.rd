\name{row_order-Heatmap-method}
\alias{row_order,Heatmap-method}
\title{
Get Row Order from a Heatmap
}
\description{
Get Row Order from a Heatmap
}
\usage{
\S4method{row_order}{Heatmap}(object)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}

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
row_order(ht)
ht = Heatmap(mat, row_km = 2)
ht = draw(ht)
row_order(ht)
}
