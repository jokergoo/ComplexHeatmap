\name{annotation_legend_size-HeatmapList-method}
\alias{annotation_legend_size,HeatmapList-method}
\title{
Size of the annotation legend viewport  


}
\description{
Size of the annotation legend viewport  


}
\usage{
\S4method{annotation_legend_size}{HeatmapList}(object, side = c("right", "left", "top", "bottom"),
    vp_width = unit(1, "npc"), vp_height = unit(1, "npc"))
}
\arguments{

  \item{object}{a \code{\link{HeatmapList}} object}
  \item{side}{side}
  \item{vp_width}{vp_width}
  \item{vp_height}{vp_height}

}
\value{
A \code{\link[grid]{unit}} object.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\alias{annotation_legend_size}
