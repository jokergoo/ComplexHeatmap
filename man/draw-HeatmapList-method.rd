\name{draw-HeatmapList-method}
\alias{draw,HeatmapList-method}
\title{
Draw a list of heatmaps  


}
\description{
Draw a list of heatmaps  


}
\usage{
\S4method{draw}{HeatmapList}(object, ..., newpage= TRUE)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList}} object}
  \item{...}{pass to \code{\link{make_layout,HeatmapList-method}}}
  \item{newpage}{whether to create a new page}

}
\details{
The function first calls \code{\link{make_layout,HeatmapList-method}} to calculate the layout of the heatmap list and the layout of every singl heatmap, then makes the plot by re-calling the graphic functions which are recorded in the layout.  


}
\value{
This function returns no value.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
