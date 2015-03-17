\name{anno_simple}
\alias{anno_simple}
\title{
Function to add simple graphics as annotations  


}
\description{
Function to add simple graphics as annotations  


}
\usage{
anno_simple(x, type = c("p", "histogram", "boxplot"),
    which = c("column", "row"), gp = gpar(fill = "#CCCCCC"), ...)
}
\arguments{

  \item{x}{a vector of values.}
  \item{type}{\code{p} for points, \code{histogram} for histogram. More graphics to be added in the future.}
  \item{which}{is the annotation a column annotation or a row annotation?}
  \item{gp}{graphic parameters.}
  \item{...}{pass to corresponding annotation graphic functions.}

}
\details{
It is a shortcut function for \code{\link{anno_points}} and \code{\link{anno_histogram}}. It is designed to support more graphics in future versions.  


}
\value{
A graphic function which can be set in \code{\link{HeatmapAnnotation}} constructor method.  


}
\seealso{
\code{\link{anno_points}}, \code{\link{anno_histogram}}  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
f = anno_simple(rnorm(10), type = "p")
grid.newpage(); f(1:10)

f = anno_simple(rnorm(10), type = "histogram")
grid.newpage(); f(1:10)

f = anno_simple(rnorm(10), type = "histogram", which = "row")
grid.newpage(); f(1:10)
}
