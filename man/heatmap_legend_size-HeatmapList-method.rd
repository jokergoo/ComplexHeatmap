\name{heatmap_legend_size-HeatmapList-method}
\alias{heatmap_legend_size,HeatmapList-method}
\alias{heatmap_legend_size}
\title{
Size of the Heatmap Legends
}
\description{
Size of the Heatmap Legends
}
\usage{
\S4method{heatmap_legend_size}{HeatmapList}(object, legend_list = list(), ...)
}
\arguments{

  \item{object}{A \code{\link{HeatmapList-class}} object.}
  \item{legend_list}{A list of self-defined legend, should be wrapped into \code{\link[grid:grid.grob]{grob}} objects. It is normally constructed by \code{\link{Legend}}.}
  \item{...}{Other arguments.}

}
\details{
Internally, all heatmap legends are packed by \code{\link{packLegend}} as a single \code{\link[grid:grid.grob]{grob}} object.

This function is only for internal use.
}
\value{
A \code{\link[grid]{unit}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
