\name{anno_simple}
\alias{anno_simple}
\title{
Function to add simple graphics as annotations  


}
\description{
Function to add simple graphics as annotations  


}
\usage{
anno_simple(x, type = c("p", "histogram"), which = c("column", "row"),
    gp = gpar(), pch = 16, size = unit(2, "mm"))
}
\arguments{

  \item{x}{a vector}
  \item{type}{\code{p} for points.}
  \item{which}{column annotation or row annotation}
  \item{gp}{graphic parameters}
  \item{pch}{point type}
  \item{size}{point size}

}
\details{
Short cut function.  


}
\value{
A function  


}
\section{See}{
\code{\link{anno_points}}, \code{\link{anno_histogram}}  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
f = anno_simple(rnorm(10), type = "p")
f(1:10)

f = anno_simple(rnorm(10), type = "histogram")
f(1:10)

f = anno_simple(rnorm(10), type = "histogram", which = "row")
f(1:10)

}
