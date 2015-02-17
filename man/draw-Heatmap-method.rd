\name{draw-Heatmap-method}
\alias{draw,Heatmap-method}
\title{
Draw a single heatmap  


}
\description{
Draw a single heatmap  


}
\usage{
\S4method{draw}{Heatmap}(object, internal = FALSE, ...)
}
\arguments{

  \item{object}{a \code{\link{Heatmap}} object.}
  \item{internal}{only for internal use.}
  \item{...}{pass to \code{\link{HeatmapList}}.}

}
\details{
The function creates a \code{\link{HeatmapList}} object, add a single heatmap and call \code{\link{draw,HeatmapList-method}} to make the final heatmap.  


}
\value{
This function returns no value.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
