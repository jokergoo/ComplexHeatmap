\name{anno_zoom}
\alias{anno_zoom}
\title{
Zoom annotation
}
\description{
Zoom annotation
}
\usage{
anno_zoom(align_to, panel_fun = function(index, nm = NULL) { grid.rect() },
    which = c("column", "row"), side = ifelse(which == "column", "top", "right"),
    size = NULL, gap = unit(1, "mm"),
    link_width = unit(5, "mm"), link_height = link_width, link_gp = gpar(),
    extend = unit(0, "mm"), width = NULL, height = NULL, internal_line = TRUE)
}
\arguments{

  \item{align_to}{It defines how the boxes correspond to the rows or the columns in the heatmap. If the value is a list of indices, each box corresponds to the rows or columns with indices in one vector in the list. If the value is a categorical variable (e.g. a factor or a character vector) that has the same length as the rows or columns in the heatmap, each box corresponds to the rows/columns in each level in the categorical variable.}
  \item{panel_fun}{A self-defined function that defines how to draw graphics in the box. The function must have a \code{index} argument which is the indices for the rows/columns that the box corresponds to. It can  have second argument \code{nm} which is the "name" of the selected part in the heatmap. The corresponding value for \code{nm} comes from \code{align_to} if it is specified as a categorical variable or a list with names.}
  \item{which}{Whether it is a column annotation or a row annotation?}
  \item{side}{Side of the boxes If it is a column annotation, valid values are "top" and "bottom"; If it is a row annotation, valid values are "left" and "right".}
  \item{size}{The size of boxes. It can be pure numeric that they are treated as relative fractions of the total height/width of the heatmap. The value of \code{size} can also be absolute units.}
  \item{gap}{Gaps between boxes.}
  \item{link_gp}{Graphic settings for the segments.}
  \item{link_width}{Width of the segments.}
  \item{link_height}{Similar as \code{link_width}, used for column annotation.}
  \item{extend}{By default, the region for the labels has the same width (if it is a column annotation) or same height (if it is a row annotation) as the heatmap. The size can be extended by this options. The value can be a proportion number or  a \code{\link[grid]{unit}} object. The length can be either one or two.}
  \item{width}{Width of the annotation. The value should be an absolute unit. Width is not allowed to be set for column annotation.}
  \item{height}{Height of the annotation. The value should be an absolute unit. Height is not allowed to be set for row annotation.}
  \item{internal_line}{Internally used.}

}
\details{
\code{\link{anno_zoom}} creates several plotting regions (boxes) which can be corresponded to subsets of rows/columns in the
heatmap.
}
\value{
An annotation function which can be used in \code{\link{HeatmapAnnotation}}.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#zoom-annotation}
}
\examples{
set.seed(123)
m = matrix(rnorm(100*10), nrow = 100)
subgroup = sample(letters[1:3], 100, replace = TRUE, prob = c(1, 5, 10))
rg = range(m)
panel_fun = function(index, nm) {
	pushViewport(viewport(xscale = rg, yscale = c(0, 2)))
	grid.rect()
	grid.xaxis(gp = gpar(fontsize = 8))
	grid.boxplot(m[index, ], pos = 1, direction = "horizontal")
	grid.text(paste("distribution of group", nm), mean(rg), y = 1.9, 
		just = "top", default.units = "native", gp = gpar(fontsize = 10))
	popViewport()
}
anno = anno_zoom(align_to = subgroup, which = "row", panel_fun = panel_fun, 
	size = unit(2, "cm"), gap = unit(1, "cm"), width = unit(4, "cm"))
Heatmap(m, right_annotation = rowAnnotation(foo = anno), row_split = subgroup)
}
