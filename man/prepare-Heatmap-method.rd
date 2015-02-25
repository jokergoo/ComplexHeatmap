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

  \item{object}{a \code{\link{Heatmap}} object.}
  \item{row_order}{orders of rows, pass to \code{\link{make_row_cluster,Heatmap-method}}.}
  \item{split}{how to split rows in the matrix, passing to \code{\link{make_row_cluster,Heatmap-method}}.}

}
\details{
The preparation of the heatmap includes following steps:  

\itemize{
  \item making clustering on rows if specified
  \item making clustering on columns if specified
  \item set row title to a empty string if specified
  \item makeing the layout of the heatmap
}

This function is only for internal use.  


}
\value{
A \code{\link{Heatmap}} object  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
