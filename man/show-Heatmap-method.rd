\name{show-Heatmap-method}
\alias{show,Heatmap-method}
\title{
Draw the Single Heatmap with Defaults
}
\description{
Draw the Single Heatmap with Defaults
}
\usage{
\S4method{show}{Heatmap}(object)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}

}
\details{
It actually calls \code{\link{draw,Heatmap-method}}, but only with default parameters. If users want to customize the heatmap,
they can pass parameters directly to \code{\link{draw,Heatmap-method}}.
}
\value{
The \code{\link{HeatmapList-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
