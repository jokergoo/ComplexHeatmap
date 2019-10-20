\name{add_heatmap-HeatmapList-method}
\alias{add_heatmap,HeatmapList-method}
\title{
Add heatmaps and row annotations to the heatmap list
}
\description{
Add heatmaps and row annotations to the heatmap list
}
\usage{
\S4method{add_heatmap}{HeatmapList}(object, x, direction = c("horizontal", "vertical"))
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object.}
  \item{x}{a \code{\link{Heatmap-class}} object or a \code{\link{HeatmapAnnotation-class}} object or a \code{\link{HeatmapList-class}} object.}
  \item{direction}{direction of the concatenation.}

}
\details{
There is a shortcut function \code{+.AdditiveUnit}.
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
