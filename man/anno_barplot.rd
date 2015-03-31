\name{anno_barplot}
\alias{anno_barplot}
\title{
Using barplot as annotation  


}
\description{
Using barplot as annotation  


}
\usage{
anno_barplot(x, which = c("column", "row"),
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
f = anno_barplot(rnorm(10))
grid.newpage(); f(1:10)

f = anno_barplot(rnorm(10), which = "row")
grid.newpage(); f(1:10)
}
