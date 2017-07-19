\name{grid.dendrogram2}
\alias{grid.dendrogram2}
\title{
Draw dendrogram under grid system
}
\description{
Draw dendrogram under grid system
}
\usage{
grid.dendrogram2(dend, facing = c("bottom", "top", "left", "right"),
    max_height = NULL, order = c("normal", "reverse"), ...)
}
\arguments{

  \item{dend}{a \code{\link[stats]{dendrogram}} object which has been adjusted by \code{\link{adjust_dend_by_leaf_width}}, or else it will be sent back to \code{\link{grid.dendrogram}}.}
  \item{facing}{same as in \code{\link{grid.dendrogram}}.}
  \item{max_height}{same as in \code{\link{grid.dendrogram}}.}
  \item{order}{same as in \code{\link{grid.dendrogram}}.}
  \item{...}{same as in \code{\link{grid.dendrogram}}.}

}
\author{
Zuguang gu <z.gu@dkfz.de>
}
\examples{
m = matrix(rnorm(100), 10)
dend = as.dendrogram(hclust(dist(m)))
dend = adjust_dend_by_leaf_width(dend, width = 1:10)
grid.dendrogram2(dend)
}
