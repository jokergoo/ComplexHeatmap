\name{draw_heatmap_legend-HeatmapList-method}
\alias{draw_heatmap_legend,HeatmapList-method}
\alias{draw_heatmap_legend}
\title{
Draw legends for all heatmaps
}
\description{
Draw legends for all heatmaps
}
\usage{
\S4method{draw_heatmap_legend}{HeatmapList}(object, legend_list = list(), ...)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object}
  \item{legend_list}{a list of self-defined legend, should be wrapped into \code{\link[grid]{grob}} objects.}
  \item{...}{graphic parameters passed to \code{\link{color_mapping_legend,ColorMapping-method}}.}

}
\details{
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
# no example for this internal method
NULL

}
