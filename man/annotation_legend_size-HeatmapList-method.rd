\name{annotation_legend_size-HeatmapList-method}
\alias{annotation_legend_size,HeatmapList-method}
\alias{annotation_legend_size}
\title{
Size of the annotation legend viewport
}
\description{
Size of the annotation legend viewport
}
\usage{
\S4method{annotation_legend_size}{HeatmapList}(object, legend_list = list(), ...)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object.}
  \item{legend_list}{a list of self-defined legend, should be wrapped into \code{\link[grid]{grob}} objects.}
  \item{...}{graphic parameters passed to \code{\link{color_mapping_legend,ColorMapping-method}}.}

}
\details{
Legends for all heatmaps or legends for all annotations will be put in one viewport. This function
calculates the size of such viewport. Note graphic parameters for legends will affect the size.

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
