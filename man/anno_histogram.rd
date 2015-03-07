\name{anno_histogram}
\alias{anno_histogram}
\title{
Using histogram as annotation  


}
\description{
Using histogram as annotation  


}
\usage{
anno_histogram(x, which = c("column", "row"),
    gp = gpar(fill = "#CCCCCC"), ...)
}
\arguments{

  \item{x}{a vector of values.}
  \item{which}{is the annotation a column annotation or a row annotation?}
  \item{gp}{graphic parameters.}
  \item{...}{for future use.}

}
\value{
A graphic function which can be set in \code{\link{HeatmapAnnotation}} constructor method.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
f = anno_histogram(rnorm(10))
f(1:10)

f = anno_histogram(rnorm(10), which = "row")
f(1:10)
}
