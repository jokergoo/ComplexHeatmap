\name{make_row_cluster-Heatmap-method}
\alias{make_row_cluster,Heatmap-method}
\alias{make_row_cluster}
\title{
Make cluster on rows
}
\description{
Make cluster on rows
}
\usage{
\S4method{make_row_cluster}{Heatmap}(object)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object.}

}
\details{
The function will fill or adjust \code{row_dend_list}, \code{row_order_list}, \code{row_title} and \code{matrix_param} slots.

If \code{order} is defined, no clustering will be applied.

This function is only for internal use.
}
\value{
A \code{\link{Heatmap-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# no example for this internal method
NULL

}
