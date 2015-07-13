\name{print.HeatmapList}
\alias{print.HeatmapList}
\title{
Draw a list of heatmaps with default parameters

}
\description{
Draw a list of heatmaps with default parameters

}
\usage{
\method{print}{HeatmapList}(x, ...)}
\arguments{

  \item{x}{a \code{\link{HeatmapList-class}} object.}
  \item{...}{additional arguments}
}
\details{
Actually it calls \code{\link{draw,HeatmapList-method}}, but only with default parameters. If users want to customize the heatmap,
they can pass parameters directly to \code{\link{draw,HeatmapList-method}}.

}
\value{
This function returns no value.

}
\examples{
# no example 
NULL}
