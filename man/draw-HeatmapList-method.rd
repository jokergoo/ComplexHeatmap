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
    row_title = character(0),
    row_title_side = c("left", "right"),
    row_title_gp = gpar(fontsize = 14),
    column_title = character(0),
    column_title_side = c("top", "bottom"),
    column_title_gp = gpar(fontsize = 14),
    heatmap_legend_side = c("right", "left", "bottom", "top"),
    show_heatmap_legend = TRUE,
    heatmap_legend_list = list(),
    annotation_legend_side = c("right", "left", "bottom", "top"),
    show_annotation_legend = TRUE,
    annotation_legend_list = list(),
    gap = unit(3, "mm"),
    main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
    row_dend_side = c("original", "left", "right"),
    row_sub_title_side = c("original", "left", "right"), ...)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object}
  \item{padding}{padding of the plot. Elements correspond to bottom, left, top, right paddings.}
  \item{newpage}{whether create a new page for the graphics.}
  \item{row_title}{title on the row.}
  \item{row_title_side}{will the title be put on the left or right of the heatmap.}
  \item{row_title_gp}{graphic parameters for drawing text.}
  \item{column_title}{title on the column.}
  \item{column_title_side}{will the title be put on the top or bottom of the heatmap.}
  \item{column_title_gp}{graphic parameters for drawing text.}
  \item{heatmap_legend_side}{side of the heatmap legend.}
  \item{show_heatmap_legend}{whether show heatmap legend.}
  \item{heatmap_legend_list}{a list of self-defined legend, should be wrapped into \code{\link[grid]{grob}} objects.}
  \item{annotation_legend_side}{side of annotation legend.}
  \item{show_annotation_legend}{whether show annotation legend.}
  \item{annotation_legend_list}{a list of self-defined legend, should be wrapped into \code{\link[grid]{grob}} objects.}
  \item{gap}{gap between heatmaps, should be a \code{\link[grid]{unit}} object.}
  \item{main_heatmap}{name or index for the main heatmap}
  \item{row_dend_side}{if auto adjust, where to put the row dendrograms for the main heatmap}
  \item{row_sub_title_side}{if auto adjust, where to put sub row titles for the main heatmap}
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
