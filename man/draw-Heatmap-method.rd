\name{draw-Heatmap-method}
\alias{draw,Heatmap-method}
\title{
Draw a Single Heatmap
}
\description{
Draw a Single Heatmap
}
\usage{
\S4method{draw}{Heatmap}(object, internal = FALSE, test = FALSE, ...)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{internal}{If \code{TRUE}, it is only used inside the calling of \code{\link{draw,HeatmapList-method}}.  It only draws the heatmap without legends where the legend will be drawn by \code{\link{draw,HeatmapList-method}}. }
  \item{test}{Only for testing. If it is \code{TRUE}, the heatmap body is directly drawn.}
  \item{...}{Pass to \code{\link{draw,HeatmapList-method}}.}

}
\details{
The function creates a \code{\link{HeatmapList-class}} object which only contains a single heatmap
and call \code{\link{draw,HeatmapList-method}} to make the final heatmap.

There are some arguments which control the some settings of the heatmap such as legends.
Please go to \code{\link{draw,HeatmapList-method}} for these arguments.
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
