\name{merge_dendrogram}
\alias{merge_dendrogram}
\title{
Merge Dendrograms
}
\description{
Merge Dendrograms
}
\usage{
merge_dendrogram(x, y, only_parent = FALSE, ...)
}
\arguments{

  \item{x}{The parent dendrogram.}
  \item{y}{The children dendrograms. They are connected to the leaves of the parent dendrogram. So the length of \code{y} should be as same as the number of leaves of the parent dendrogram.}
  \item{only_parent}{Whether only returns the parent dendrogram where the height and node positions have been adjusted by children dendrograms.}
  \item{...}{Other arguments.}

}
\details{
Do not retrieve the order of the merged dendrogram. It is not reliable.
}
\examples{
m1 = matrix(rnorm(100), nr = 10)
m2 = matrix(rnorm(80), nr = 8)
m3 = matrix(rnorm(50), nr = 5)
dend1 = as.dendrogram(hclust(dist(m1)))
dend2 = as.dendrogram(hclust(dist(m2)))
dend3 = as.dendrogram(hclust(dist(m3)))
dend_p = as.dendrogram(hclust(dist(rbind(colMeans(m1), colMeans(m2), colMeans(m3)))))
dend_m = merge_dendrogram(dend_p, list(dend1, dend2, dend3))
grid.dendrogram(dend_m, test = TRUE)

dend_m = merge_dendrogram(dend_p, list(dend1, dend2, dend3), only_parent = TRUE)
grid.dendrogram(dend_m, test = TRUE)

require(dendextend)
dend1 = color_branches(dend1, k = 1, col = "red")
dend2 = color_branches(dend2, k = 1, col = "blue")
dend3 = color_branches(dend3, k = 1, col = "green")
dend_p = color_branches(dend_p, k = 1, col = "orange")
dend_m = merge_dendrogram(dend_p, list(dend1, dend2, dend3))
grid.dendrogram(dend_m, test = TRUE)
}
