\name{draw_annotation_legend-HeatmapList-method}
\alias{draw_annotation_legend,HeatmapList-method}
\alias{draw_annotation_legend}
\title{
Draw legends for All Annotations
}
\description{
Draw legends for All Annotations
}
\usage{
\S4method{draw_annotation_legend}{HeatmapList}(object, legend_list = list(), ...)
}
\arguments{

  \item{object}{A \code{\link{HeatmapList-class}} object.}
  \item{legend_list}{A list of self-defined legends, should be wrapped into \code{\link[grid:grid.grob]{grob}} objects. It is normally constructed by \code{\link{Legend}}.}
  \item{...}{Other arguments.}

}
\details{
We call the "annotation legends" as the secondary legends.
For horizontal heamtap list, the legends are those from all top/bottom annotations, and for vertical heatmap list, 
the legends are those from all left/right annotations.

A viewport is created which contains annotation legends.

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
