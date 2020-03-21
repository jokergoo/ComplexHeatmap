\name{gt_render}
\alias{gt_render}
\title{
Mark the text for the rendering by gridtext package
}
\description{
Mark the text for the rendering by gridtext package
}
\usage{
gt_render(x, ...)
}
\arguments{

  \item{x}{Text labels. The value can be a vector.}
  \item{...}{Other parameters passed to \code{\link[gridtext]{richtext_grob}}.}

}
\details{
Text marked by \code{\link{gt_render}} will be rendered by \code{\link[gridtext]{richtext_grob}} function.
}
\examples{
if(requireNamespace("gridtext")) {
mat = matrix(rnorm(100), 10)
rownames(mat) = letters[1:10]
ht = Heatmap(mat, 
	column_title = gt_render("Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>And some <span style='font-size:18pt; color:black'>large</span> text.", r = unit(2, "pt"), padding = unit(c(2, 2, 2, 2), "pt")),
	column_title_gp = gpar(box_fill = "orange"),
	row_labels = gt_render(letters[1:10], padding = unit(c(2, 10, 2, 10), "pt")),
	row_names_gp = gpar(box_col = "red"),
	row_km = 2, 
	row_title = gt_render(c("title1", "title2")), 
	row_title_gp = gpar(box_fill = "yellow"),
	heatmap_legend_param = list(
		title = gt_render("<span style='color:orange'>**Legend title**</span>"), 
		title_gp = gpar(box_fill = "grey"),
		at = c(-3, 0, 3), 
		labels = gt_render(c("*negative* three", "zero", "*positive* three"))
	))
ht = rowAnnotation(
	foo = anno_text(gt_render(sapply(LETTERS[1:10], strrep, 10), align_widths = TRUE), 
	                gp = gpar(box_col = "blue", box_lwd = 2), 
	                just = "right", 
	                location = unit(1, "npc")
	)) + ht
draw(ht)

}
}
