\name{draw_hclust-Heatmap-method}
\alias{draw_hclust,Heatmap-method}
\alias{draw_hclust}
\title{
Draw dendrogram on row or column  


}
\description{
Draw dendrogram on row or column  


}
\usage{
\S4method{draw_hclust}{Heatmap}(object, which = c("row", "column"),
    side = ifelse(which == "row", "left", "top"), k = 1, gp = NULL,
    max_height = NULL, ...)
}
\arguments{

  \item{object}{a \code{\link{Heatmap}} object.}
  \item{which}{should the dendrogram be drawn on row or column.}
  \item{side}{side of the dendrogram.}
  \item{k}{which matrix in the matrix list}
  \item{gp}{graphic parameters for drawing lines.}
  \item{max_height}{maximum height of the dendrogram.}
  \item{...}{pass to \code{\link[grid]{viewport}}.}

}
\details{
A viewport is created which contains dendrogram.  


}
\value{
This function returns no value.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
