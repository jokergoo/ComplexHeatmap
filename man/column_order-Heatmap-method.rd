\name{column_order-Heatmap-method}
\alias{column_order,Heatmap-method}
\title{
Get column order from a heatmap list
}
\description{
Get column order from a heatmap list
}
\usage{
\S4method{column_order}{Heatmap}(object)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object}

}
\value{
A vector containing column orders
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(100), 10)
ht = Heatmap(mat)
column_order(ht)
ht = Heatmap(mat, km = 2)
column_order(ht)
}
