\name{draw_annotation-Heatmap-method}
\alias{draw_annotation,Heatmap-method}
\alias{draw_annotation}
\title{
Draw column annotations
}
\description{
Draw column annotations
}
\usage{
\S4method{draw_annotation}{Heatmap}(object, which = c("top", "bottom"))
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object.}
  \item{which}{are the annotations put on the top or bottom of the heatmap?}

}
\details{
A viewport is created which contains column annotations.

Since the column annotations is a \code{\link{HeatmapAnnotation-class}} object, the function
calls \code{\link{draw,HeatmapAnnotation-method}} to draw the annotations.

This function is only for internal use.
}
\value{
This function returns no value.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# no example for this internal method
NULL

}
