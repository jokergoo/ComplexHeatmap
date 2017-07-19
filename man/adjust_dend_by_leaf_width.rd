\name{adjust_dend_by_leaf_width}
\alias{adjust_dend_by_leaf_width}
\title{
Adjust dendrogram based on width of leaves
}
\description{
Adjust dendrogram based on width of leaves
}
\usage{
adjust_dend_by_leaf_width(dend, width = 1, offset = 0)
}
\arguments{

  \item{dend}{a \code{\link[stats]{dendrogram}} object.}
  \item{width}{a vector of width. The order of width SHOULD be same as the order of original elements before clustering.}
  \item{offset}{offset to x = 0}

}
\details{
In the standard \code{\link[stats]{dendrogram}} object, leaves locate at x = 0.5, 1.5, ..., n - 0.5,
which means, the width of leaves are always 1 and the distance to neighbouring leaves is always 1 as well.
Here \code{\link{adjust_dend_by_leaf_width}} adjusts the dendrogram by setting different width for leaves so that leaves
have unequal distance to other leaves.

The adjusted dendrogram can be sent to \code{\link{grid.dendrogram2}} to make the dendrogram.

For each branch as well each leaf, a new attribute of \code{x} is added which is the position of the middle point or the leaf.
For each leaf, a new attribute of \code{width} is added which is the width of current leaf.
}
\value{
A \code{\link[stats]{dendrogram}} object. The adjustment will not affect other standard dendrogram functions.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
m = matrix(rnorm(100), 10)
dend = as.dendrogram(hclust(dist(m)))
dend = adjust_dend_by_leaf_width(dend, width = 1:10)
require(dendextend)
get_leaves_attr(dend, "label")
get_leaves_attr(dend, "width")
get_leaves_attr(dend, "x")
}
