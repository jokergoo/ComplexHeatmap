\name{add_heatmap-HeatmapAnnotation-method}
\alias{add_heatmap,HeatmapAnnotation-method}
\title{
Add Annotations or Heatmaps as a Heatmap List
}
\description{
Add Annotations or Heatmaps as a Heatmap List
}
\usage{
\S4method{add_heatmap}{HeatmapAnnotation}(object, x, direction = c("horizontal", "vertical"))
}
\arguments{

  \item{object}{A \code{\link{HeatmapAnnotation-class}} object.}
  \item{x}{A \code{\link{Heatmap-class}} object, a \code{\link{HeatmapAnnotation-class}} object or a \code{\link{HeatmapList-class}} object.}
  \item{direction}{Whether it is horizontal list or a vertical list?}

}
\details{
Normally we directly use \code{+} for horizontal concatenation and \code{\link[=pct_v_pct]{\%v\%}} for vertical concatenation.
}
\value{
A \code{\link{HeatmapList-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
