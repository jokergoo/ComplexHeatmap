\name{anno_textbox}
\alias{anno_textbox}
\title{
Text box annotations
}
\description{
Text box annotations
}
\usage{
anno_textbox(align_to, text, background_gp = gpar(fill = "#DDDDDD", col = "#AAAAAA"),
    which = c("row", "column"), by = "anno_link", side = c("right", "left"), ...)
}
\arguments{

  \item{align_to}{It controls how the text boxes are aligned to the heatmap rows. The value can be a categorical vector which have the same length as heatmap rows, or a list of row indices. It does not necessarily include all row indices.}
  \item{text}{The corresponding texts. The value should be a list of texts. To control graphics parameters of texts in the boxes, The value of \code{text} can also be set as a list of data frames where the first column contains the text, from the second column contains graphics parameters for each text. The column names should be "col", "fontsize", "fontfamily" and "fontface".}
  \item{background_gp}{Graphics for the background.}
  \item{which}{Only "row" is allowed.}
  \item{by}{Are text boxed arranged by \code{\link{anno_link}} or by \code{\link{anno_block}}?}
  \item{side}{Side of the annotation to the heatmap.}
  \item{...}{Pass to \code{\link{textbox_grob}}.}

}
\examples{
require(circlize)
mat = matrix(rnorm(100*10), nrow = 100)

split = sample(letters[1:10], 100, replace = TRUE)
text = lapply(unique(split), function(x) {
	data.frame(month.name, col = rand_color(12, friendly = TRUE), fontsize = runif(12, 6, 14))
})
names(text) = unique(split)

Heatmap(mat, cluster_rows = FALSE, row_split = split,
    right_annotation = rowAnnotation(wc = anno_textbox(split, text))
)
}
