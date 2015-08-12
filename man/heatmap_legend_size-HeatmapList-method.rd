\name{heatmap_legend_size-HeatmapList-method}
\alias{heatmap_legend_size,HeatmapList-method}
\alias{heatmap_legend_size}
\title{
Size of the heatmap legend viewport
}
\description{
Size of the heatmap legend viewport
}
\usage{
\S4method{heatmap_legend_size}{HeatmapList}(object, legend_list = list(), ...)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object}
  \item{legend_list}{a list of self-defined legend, should be wrapped into \code{\link[grid]{grob}} objects.}
  \item{...}{graphic parameters passed to \code{\link{color_mapping_legend,ColorMapping-method}}.}

}
\details{
This function is only for internal use.
}
\value{
A \code{\link[grid]{unit}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# no example for this internal method
NULL

}
