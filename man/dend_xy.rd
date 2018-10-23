\name{dend_xy}
\alias{dend_xy}
\title{
Coordinates of the Dendrogram
}
\description{
Coordinates of the Dendrogram
}
\usage{
dend_xy(dend)
}
\arguments{

  \item{dend}{a \code{\link{dendrogram}} object.}

}
\details{
\code{dend} will be processed by \code{\link{adjust_dend_by_x}} if it is processed yet.
}
\value{
A list of leave positions (\code{x}) and dendrogram height (\code{y}).
}
\examples{
m = matrix(rnorm(100), 10)
dend1 = as.dendrogram(hclust(dist(m)))
dend_xy(dend1)

dend1 = adjust_dend_by_x(dend1, sort(runif(10)))
dend_xy(dend1)

dend1 = adjust_dend_by_x(dend1, unit(1:10, "cm"))
dend_xy(dend1)
}
