\name{initialize-SingleAnnotation-method}
\alias{initialize,SingleAnnotation-method}
\title{
Constructor method for SingleAnnotation class  


}
\description{
Constructor method for SingleAnnotation class  


}
\usage{
\S4method{initialize}{SingleAnnotation}(.Object, name, value, col, fun, which = c("column", "row"),
    show_legend = TRUE)
}
\arguments{

  \item{.Object}{private object.}
  \item{name}{name for this annotation}
  \item{value}{A vector of annotation}
  \item{col}{colors corresponding to \code{value}. If the mapping is discrete mapping, the value of \code{col} should be a vector; If the mapping is continuous mapping, the value of \code{col} should be  a color mapping function. }
  \item{fun}{a self-defined function. The argument of this function should be a vector of index.}
  \item{which}{is the annotation a row annotation or a column annotation?}
  \item{show_legend}{if it is a simple annotation, whether show legend when making the complete heatmap.}

}
\details{
The most simple annotation is one row or one column grids in which different colors represent different classes of the data. Here the function use \code{\link{ColorMapping}} class to process such information. \code{value} and \code{col} arguments are used to construct a  \code{\link{ColorMapping}} object.  

\code{fun} is used to construct a more complex annotation. The only input argument of \code{fun} is a index of rows or columns which is already adjusted by the clustering, so that graphics can be  corresponded to the correct rows or columns.  


}
\value{
A \code{\link{SingleAnnotation}} object.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
