\name{cluster_within_group}
\alias{cluster_within_group}
\title{
Cluster within and between Groups
}
\description{
Cluster within and between Groups
}
\usage{
cluster_within_group(mat, factor)
}
\arguments{

  \item{mat}{A matrix where clustering is applied on columns.}
  \item{factor}{A categorical vector.}

}
\details{
The clustering is firstly applied in each group, then clustering is applied
to group means. The within-group dendrograms and between-group dendrogram
are finally connected by \code{\link{merge_dendrogram}}.

In the final dendrogram, the within group dendrograms are enforced to be 
flat lines to emphasize that the within group dendrograms have no sense to 
compare to between-group dendrogram.
}
\value{
A \code{\link{dendrogram}} object. The order of columns can be retrieved by \code{\link[stats]{order.dendrogram}}.
}
\examples{
m = matrix(rnorm(120), nc = 12)
colnames(m) = letters[1:12]
fa = rep(c("a", "b", "c"), times = c(2, 4, 6))
dend = cluster_within_group(m, fa)
grid.dendrogram(dend, test = TRUE)
}
