\name{annotation_legend_size-HeatmapList-method}
\alias{annotation_legend_size,HeatmapList-method}
\alias{annotation_legend_size}
\title{
Size of the Annotation Legends
}
\description{
Size of the Annotation Legends
}
\usage{
\S4method{annotation_legend_size}{HeatmapList}(object, legend_list = list(), ...)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object.}
  \item{legend_list}{A list of self-defined legend, should be wrapped into \code{\link[grid:grid.grob]{grob}} objects. It is normally constructed by \code{\link{Legend}}.}
  \item{...}{Other arguments.}

}
\details{
Internally, all annotation legends are packed by \code{\link{packLegend}} as a single \code{\link[grid:grid.grob]{grob}} object.

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
