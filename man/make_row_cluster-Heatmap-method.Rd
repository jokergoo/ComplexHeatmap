\name{make_row_cluster-Heatmap-method}
\alias{make_row_cluster,Heatmap-method}
\alias{make_row_cluster}
\title{
Make Cluster on Rows
}
\description{
Make Cluster on Rows
}
\usage{
\S4method{make_row_cluster}{Heatmap}(object)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}

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
# There is no example
NULL

}
