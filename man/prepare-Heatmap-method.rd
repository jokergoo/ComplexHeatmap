\name{prepare-Heatmap-method}
\alias{prepare,Heatmap-method}
\alias{prepare}
\title{
Prepare the heatmap  


}
\description{
Prepare the heatmap  


}
\usage{
\S4method{prepare}{Heatmap}(object, row_order = NULL, split = object@matrix_param$split)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object.}
  \item{row_order}{orders of rows, pass to \code{\link{make_row_cluster,Heatmap-method}}. Because if more than one heatmaps are drawn by columns, the order of some heatmap will be adjusted by one certain heatmap, this argument is used to pass a pre-defined row order.}
  \item{split}{how to split rows in the matrix, passing to \code{\link{make_row_cluster,Heatmap-method}}.}

}
\details{
The preparation of the heatmap includes following steps:  

\itemize{
  \item making clustering on rows if specified (by calling \code{\link{make_row_cluster,Heatmap-method}})
  \item making clustering on columns if specified (by calling \code{\link{make_column_cluster,Heatmap-method}})
  \item making the layout of the heatmap (by calling \code{\link{make_layout,Heatmap-method}})
}

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
