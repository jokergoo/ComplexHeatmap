\name{column_order-Heatmap-method}
\alias{column_order,Heatmap-method}
\title{
Get Column Order from a Aeatmap List
}
\description{
Get Column Order from a Aeatmap List
}
\usage{
\S4method{column_order}{Heatmap}(object)
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
column_order(ht)
ht = Heatmap(mat, column_km = 2)
ht = draw(ht)
column_order(ht)
}
