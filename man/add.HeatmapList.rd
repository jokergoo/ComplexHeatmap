\name{+.HeatmapList}
\alias{+.HeatmapList}
\title{
Add heatmaps or row annotations to the list  


}
\description{
Add heatmaps or row annotations to the list  


}
\usage{
\method{+}{HeatmapList}(x, y)
}
\arguments{

  \item{x}{a \code{\link{HeatmapList}} object.}
  \item{y}{a \code{\link{Heatmap}} object, a \code{\link{HeatmapAnnotation}} object or a \code{\link{HeatmapList}} object.}

}
\details{
It is only a shortcut function. It actually calls \code{\link{add_heatmap,HeatmapList-method}}.  


}
\value{
A \code{\link{HeatmapList}} object.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
