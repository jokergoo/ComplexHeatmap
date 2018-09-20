\name{draw_annotation-Heatmap-method}
\alias{draw_annotation,Heatmap-method}
\alias{draw_annotation}
\title{
Draw Heatmap Annotations on the Heatmap
}
\description{
Draw Heatmap Annotations on the Heatmap
}
\usage{
\S4method{draw_annotation}{Heatmap}(object, which = c("top", "bottom", "left", "right"), k = 1, ...)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{which}{The position of the heamtap annotation.}
  \item{k}{Slice index.}
  \item{...}{Pass to \code{\link[grid]{viewport}} which includes the complete heatmap annotation.}

}
\details{
A viewport is created which contains column/top annotations.

The function calls \code{\link{draw,HeatmapAnnotation-method}} to draw the annotations.

This function is only for internal use.
}
\value{
This function returns no value.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
