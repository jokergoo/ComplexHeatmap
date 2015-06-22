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
    gp = gpar(fill = "#CCCCCC"), axis = FALSE, axis_side = NULL,
    axis_gp = gpar(fontsize = 8), ...)
}
\arguments{

  \item{x}{a vector of values.}
  \item{which}{is the annotation a column annotation or a row annotation?}
  \item{gp}{graphic parameters.}
  \item{axis}{whether add axis}
  \item{axis_side}{value in "left", "right", "bottom" and "top"}
  \item{axis_gp}{graphic parameters for axis}
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
