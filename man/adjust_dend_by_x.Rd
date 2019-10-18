\name{adjust_dend_by_x}
\alias{adjust_dend_by_x}
\title{
Adjust the Positions of nodes/leaves in the Dendrogram
}
\description{
Adjust the Positions of nodes/leaves in the Dendrogram
}
\usage{
adjust_dend_by_x(dend, leaf_pos = 1:nobs(dend)-0.5)
}
\arguments{

  \item{dend}{A \code{\link{dendrogram}} object.}
  \item{leaf_pos}{A vector of positions of leaves. The value can also be a \code{\link[grid]{unit}} object.}

}
\details{
The positions of nodes stored as \code{x} attribute are recalculated based on the new positions of leaves.

By default, the position of leaves are at 0.5, 1.5, ..., n-0.5.
}
\examples{
m = matrix(rnorm(100), 10)
dend = as.dendrogram(hclust(dist(m)))
dend = adjust_dend_by_x(dend, sort(runif(10)))
str(dend)
dend = adjust_dend_by_x(dend, unit(1:10, "cm"))
str(dend)
}
