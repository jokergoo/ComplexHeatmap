\name{make_column_cluster-Heatmap-method}
\alias{make_column_cluster,Heatmap-method}
\alias{make_column_cluster}
\title{
Make cluster on columns  


}
\description{
Make cluster on columns  


}
\usage{
\S4method{make_column_cluster}{Heatmap}(object, order = NULL)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object.}
  \item{order}{a pre-defined order.}

}
\details{
The function will fill or adjust \code{column_hclust} and \code{column_order} slots.  

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
