\name{column_dend-Heatmap-method}
\alias{column_dend,Heatmap-method}
\title{
Get column dendrograms from a heatmap
}
\description{
Get column dendrograms from a heatmap
}
\usage{
\S4method{column_dend}{Heatmap}(object)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object}

}
\value{
A dendrogram object
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(100), 10)
ht = Heatmap(mat)
column_dend(ht)
ht = Heatmap(mat, km = 2)
column_dend(ht)
}
