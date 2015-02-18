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
\S4method{make_column_cluster}{Heatmap}(object, order = "hclust")
}
\arguments{

  \item{object}{a \code{\link{Heatmap}} object.}
  \item{order}{a single string \code{hclust} means the cluster is performed by \code{\link[stats]{hclust}}. The value can also be a pre-defined order.}

}
\details{
The function will fill or adjust \code{column_hclust} and \code{column_order} slots.  

This function is only for internal use.  


}
\value{
A \code{\link{Heatmap}} object  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
