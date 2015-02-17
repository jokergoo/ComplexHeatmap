\name{draw_heatmap_body-Heatmap-method}
\alias{draw_heatmap_body,Heatmap-method}
\alias{draw_heatmap_body}
\title{
Draw the heatmap body  


}
\description{
Draw the heatmap body  


}
\usage{
\S4method{draw_heatmap_body}{Heatmap}(object, k = 1, gp = object@gp_list$rect_gp, ...)
}
\arguments{

  \item{object}{a \code{\link{Heatmap}} object.}
  \item{k}{which matrix in the matrix list}
  \item{gp}{graphic parameters for drawing rectangles.}
  \item{...}{pass to \code{\link[grid]{viewport}}.}

}
\details{
A viewport is created which contains grids.  


}
\value{
This function returns no value.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
