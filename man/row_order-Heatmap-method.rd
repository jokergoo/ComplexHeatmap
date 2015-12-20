\name{row_order-Heatmap-method}
\alias{row_order,Heatmap-method}
\title{
Get row order from a heatmap
}
\description{
Get row order from a heatmap
}
\usage{
\S4method{row_order}{Heatmap}(object)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object}

}
\value{
A list contains row orders which correspond to the original matrix
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(100), 10)
ht = Heatmap(mat)
row_order(ht)
ht = Heatmap(mat, km = 2)
row_order(ht)
}
