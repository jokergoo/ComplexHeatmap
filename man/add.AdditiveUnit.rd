\name{+.AdditiveUnit}
\alias{+.AdditiveUnit}
\title{
Add two heatmaps or add row annotations as a heatmap list  


}
\description{
Add two heatmaps or add row annotations as a heatmap list  


}
\usage{
\method{+}{AdditiveUnit}(x, y)
}
\arguments{

  \item{x}{a \code{\link{Heatmap}} object, a \code{\link{HeatmapAnnotation}} object or a \code{\link{HeatmapList}} object.}
  \item{y}{a \code{\link{Heatmap}} object, a \code{\link{HeatmapAnnotation}} object or a \code{\link{HeatmapList}} object.}

}
\details{
It is only a shortcut function. It actually calls \code{\link{add_heatmap,Heatmap-method}}, \code{\link{add_heatmap,HeatmapList-method}} or \code{\link{add_heatmap,HeatmapAnnotation-method}} depending on the class of the input values.  


}
\value{
a \code{\link{HeatmapList}} object.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
