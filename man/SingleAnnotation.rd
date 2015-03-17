\name{SingleAnnotation}
\alias{SingleAnnotation}
\title{
Constructor method for SingleAnnotation class  


}
\description{
Constructor method for SingleAnnotation class  


}
\usage{
SingleAnnotation(name, value, col, fun, which = c("column", "row"),
    show_legend = TRUE, gp = gpar(col = NA))
}
\arguments{

  \item{name}{name for this annotation.}
  \item{value}{A vector of annotation.}
  \item{col}{colors corresponding to \code{value}. If the mapping is discrete mapping, the value of \code{col} should be a vector; If the mapping is continuous mapping, the value of \code{col} should be  a color mapping function. }
  \item{fun}{a self-defined function to add annotation graphics. The argument of this function should only  be a vector of index that corresponds to rows or columns.}
  \item{which}{is the annotation a row annotation or a column annotation?}
  \item{show_legend}{if it is a simple annotation, whether show legend when making the complete heatmap.}
  \item{gp}{graphic parameters for simple annotations.}

}
\details{
The most simple annotation is one row or one column grids in which different colors represent different classes of the data. Here the function use \code{\link{ColorMapping-class}} to process such simple annotation. \code{value} and \code{col} arguments controls values and colors of the simple annotation and a \code{\link{ColorMapping-class}} object will be constructed based on \code{value} and \code{col}.  

\code{fun} is used to construct a more complex annotation. Users can add any type of annotation graphics by implementing a function. The only input argument of \code{fun} is a index of rows or columns which is already adjusted by the clustering. In the package, there are already several annotation graphic function generators: \code{\link{anno_points}}, \code{\link{anno_histogram}} and \code{\link{anno_boxplot}}.  

One thing that users should be careful is the difference of coordinates when the annotation is a row annotation or a column annotation.   


}
\value{
A \code{\link{SingleAnnotation-class}} object.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
# discrete character
SingleAnnotation(name = "test", value = c("a", "a", "a", "b", "b", "b"))
SingleAnnotation(name = "test", value = c("a", "a", "a", "b", "b", "b"), 
    which = "row")

# with defined colors
SingleAnnotation(value = c("a", "a", "a", "b", "b", "b"), 
    col = c("a" = "red", "b" = "blue"))

# continuous numbers
require(circlize)
SingleAnnotation(value = 1:10)
SingleAnnotation(value = 1:10, col = colorRamp2(c(1, 10), c("blue", "red")))

# self-defined graphic function
SingleAnnotation(fun = anno_points(1:10))
}
