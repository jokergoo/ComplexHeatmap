\name{show-HeatmapList-method}
\alias{show,HeatmapList-method}
\title{
Draw a list of heatmaps with default parameters
}
\description{
Draw a list of heatmaps with default parameters
}
\usage{
\S4method{show}{HeatmapList}(object)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object.}

}
\details{
Actually it calls \code{\link{draw,HeatmapList-method}}, but only with default parameters. If users want to customize the heatmap,
they can pass parameters directly to \code{\link{draw,HeatmapList-method}}.
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
