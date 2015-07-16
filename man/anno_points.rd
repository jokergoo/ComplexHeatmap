\name{anno_points}
\alias{anno_points}
\title{
Using points as annotation

}
\description{
Using points as annotation

}
\usage{
anno_points(x, which = c("column", "row"), gp = gpar(), pch = 16,
    size = unit(2, "mm"), axis = FALSE, axis_side = NULL,
    axis_gp = gpar(fontsize = 8), ...)}
\arguments{

  \item{x}{a vector of values.}
  \item{which}{is the annotation a column annotation or a row annotation?}
  \item{gp}{graphic parameters.}
  \item{pch}{point type.}
  \item{size}{point size.}
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

f = anno_points(rnorm(10))
grid.newpage(); f(1:10)}
