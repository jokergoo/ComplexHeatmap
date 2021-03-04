\name{cluster_between_groups}
\alias{cluster_between_groups}
\title{
Cluster only between Groups
}
\description{
Cluster only between Groups
}
\usage{
cluster_between_groups(mat, factor)
}
\arguments{

  \item{mat}{A matrix where clustering is applied on columns.}
  \item{factor}{A categorical vector.}

}
\details{
The clustering is only applied between groups and inside a group, the order is unchanged.
}
\value{
A \code{\link{dendrogram}} object.
}
\examples{
m = matrix(rnorm(120), nc = 12)
colnames(m) = letters[1:12]
fa = rep(c("a", "b", "c"), times = c(2, 4, 6))
dend = cluster_between_groups(m, fa)
grid.dendrogram(dend, test = TRUE)
}
