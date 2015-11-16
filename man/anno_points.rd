\name{anno_points}
\alias{anno_points}
\title{
Using points as annotation
}
\description{
Using points as annotation
}
\usage{
anno_points(x, which = c("column", "row"), border = TRUE, gp = gpar(), pch = 16,
    size = unit(2, "mm"), ylim = NULL, axis = FALSE, axis_side = NULL,
    axis_gp = gpar(fontsize = 8), axis_direction = c("normal", "reverse"), ...)
}
\arguments{

  \item{x}{a vector of numeric values.}
  \item{which}{is the annotation a column annotation or a row annotation?}
  \item{border}{whether show border of the annotation compoment}
  \item{gp}{graphic parameters.}
  \item{pch}{point type.}
  \item{size}{point size.}
  \item{ylim}{data ranges.}
  \item{axis}{whether add axis.}
  \item{axis_side}{if it is placed as column annotation, value can only be "left" or "right". If it is placed as row annotation, value can only be "bottom" or "top".}
  \item{axis_gp}{graphic parameters for axis}
  \item{axis_direction}{if the annotation is row annotation, should the axis be from left to right (default) or follow the reversed direction?}
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
grid.newpage(); f(1:10)

}
