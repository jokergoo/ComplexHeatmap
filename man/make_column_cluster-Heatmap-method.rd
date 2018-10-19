\name{make_column_cluster-Heatmap-method}
\alias{make_column_cluster,Heatmap-method}
\alias{make_column_cluster}
\title{
Make Cluster on Columns
}
\description{
Make Cluster on Columns
}
\usage{
\S4method{make_column_cluster}{Heatmap}(object)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}

}
\details{
The function will fill or adjust \code{column_dend_list},
\code{column_order_list}, \code{column_title} and \code{matrix_param} slots.

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
