\name{get_color_mapping_list-HeatmapAnnotation-method}
\alias{get_color_mapping_list,HeatmapAnnotation-method}
\alias{get_color_mapping_list}
\title{
Get a list of color mapping objects
}
\description{
Get a list of color mapping objects
}
\usage{
\S4method{get_color_mapping_list}{HeatmapAnnotation}(object)
}
\arguments{

  \item{object}{a \code{\link{HeatmapAnnotation-class}} object.}

}
\details{
Color mapping for visible simple annotations are only returned.

This function is only for internal use.
}
\value{
A list of \code{\link{ColorMapping-class}} objects or an empty list.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# no example for this internal method
NULL

}
