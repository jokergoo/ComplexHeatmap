\name{dendrogramGrob}
\alias{dendrogramGrob}
\title{
Grob for Dendrogram
}
\description{
Grob for Dendrogram
}
\usage{
dendrogramGrob(dend, facing = c("bottom", "top", "left", "right"),
    order = c("normal", "reverse"), gp = gpar())
}
\arguments{

  \item{dend}{A \code{\link{dendrogram}} object.}
  \item{facing}{Facing of the dendrogram.}
  \item{order}{If it is set to \code{reverse}, the first leaf is put on the right if the dendrogram is horizontal and it is put on the top if the dendrogram is vertical.}
  \item{gp}{Graphic parameters for the dendrogram segments. If any of \code{col}, \code{lwd} or \code{lty} is set in the \code{edgePar} attribute of a node, the corresponding value defined in \code{gp} will be overwritten for this node, so \code{gp} is like global graphic parameters for dendrogram segments.}

}
\details{
If \code{dend} has not been processed by \code{\link{adjust_dend_by_x}}, internally \code{\link{adjust_dend_by_x}} is called
to add \code{x} attributes to each node/leaf.
}
\value{
A \code{\link{grob}} object which is contructed by \code{\link[grid:grid.segments]{segmentsGrob}}.
}
\examples{
# There is no example
NULL

}
