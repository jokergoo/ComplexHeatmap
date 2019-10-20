\name{add_heatmap-Heatmap-method}
\alias{add_heatmap,Heatmap-method}
\title{
Add Heatmap to the Heatmap List
}
\description{
Add Heatmap to the Heatmap List
}
\usage{
\S4method{add_heatmap}{Heatmap}(object, x, direction = c("horizontal", "vertical"))
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{x}{a \code{\link{Heatmap-class}} object, a \code{\link{HeatmapAnnotation-class}} object or a \code{\link{HeatmapList-class}} object.}
  \item{direction}{Whether the heatmap is added horizontal or vertically?}

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
