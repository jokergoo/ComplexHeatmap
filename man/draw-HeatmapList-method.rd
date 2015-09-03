\name{draw-HeatmapList-method}
\alias{draw,HeatmapList-method}
\title{
Draw a list of heatmaps
}
\description{
Draw a list of heatmaps
}
\usage{
\S4method{draw}{HeatmapList}(object,
    padding = unit(c(2, 2, 2, 2), "mm"),
    newpage = TRUE,
    ...)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object}
  \item{padding}{padding of the plot. Elements correspond to bottom, left, top, right paddings.}
  \item{newpage}{whether create a new page for the graphics.}
  \item{...}{pass to \code{\link{make_layout,HeatmapList-method}}}

}
\details{
The function first calls \code{\link{make_layout,HeatmapList-method}} to calculate
the layout of the heatmap list and the layout of every single heatmap,
then makes the plot by re-calling the graphic functions which are already recorded
in the layout.
}
\value{
This function returns a list of row dendrograms and column dendrogram.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(80, 2), 8, 10)
mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
rownames(mat) = letters[1:12]
colnames(mat) = letters[1:10]

ht = Heatmap(mat)
ht_list = ht + ht
draw(ht_list)
draw(ht_list, row_title = "row title", column_title = "column title", 
	heatmap_legend_side = "top")

}
