\name{get_color_mapping_list-HeatmapAnnotation-method}
\alias{get_color_mapping_list,HeatmapAnnotation-method}
\alias{get_color_mapping_list}
\title{
Get a List of ColorMapping objects
}
\description{
Get a List of ColorMapping objects
}
\usage{
\S4method{get_color_mapping_list}{HeatmapAnnotation}(object)
}
\arguments{

  \item{object}{A \code{\link{HeatmapAnnotation-class}} object.}

}
\details{
Color mappings for visible simple annotations are only returned.

This function is only for internal use.
}
\value{
A list of \code{\link{ColorMapping-class}} objects or an empty list.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
