\name{draw_heatmap_legend-HeatmapList-method}
\alias{draw_heatmap_legend,HeatmapList-method}
\alias{draw_heatmap_legend}
\title{
Draw legends for All Heatmaps
}
\description{
Draw legends for All Heatmaps
}
\usage{
\S4method{draw_heatmap_legend}{HeatmapList}(object, legend_list = list(), ...)
}
\arguments{

  \item{object}{A \code{\link{HeatmapList-class}} object.}
  \item{legend_list}{A list of self-defined legends, should be wrapped into \code{\link[grid:grid.grob]{grob}} objects. It is normally constructed by \code{\link{Legend}}.}
  \item{...}{Other arguments.}

}
\details{
Actually we call the "heatmap legends" as the main legends.
For horizontal heatmap list, the legends are those from heamtap/row annotation/left/right annotation.
For vertical heatmap list, the legends are those from heamtap/column annotation/top/bottom annotation.
if \code{merge_legends} is true in \code{\link{draw,HeatmapList-method}}, then it contains all legends shown on the plot.

A viewport is created which contains heatmap legends.

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
