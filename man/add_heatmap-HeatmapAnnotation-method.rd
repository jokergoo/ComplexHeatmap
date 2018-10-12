\name{add_heatmap-HeatmapAnnotation-method}
\alias{add_heatmap,HeatmapAnnotation-method}
\title{
Add row annotations or heatmaps as a heatmap list
}
\description{
Add row annotations or heatmaps as a heatmap list
}
\usage{
\S4method{add_heatmap}{HeatmapAnnotation}(object, x, direction = c("horizontal", "vertical"))
}
\arguments{

  \item{object}{A \code{\link{HeatmapAnnotation-class}} object.}
  \item{x}{A \code{\link{Heatmap-class}} object, a \code{\link{HeatmapAnnotation-class}} object or a \code{\link{HeatmapList-class}} object.}
  \item{direction}{Whether it is a horizontal add or a vertical add?}

}
\details{
There is a helper function \code{+.AdditiveUnit} for horizontal add or \code{\link[=pct_v_pct]{\%v\%}} for vertical add.
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
