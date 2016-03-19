\name{anno_link}
\alias{anno_link}
\title{
Link annotation with labels
}
\description{
Link annotation with labels
}
\usage{
anno_link(at, labels, which = c("column", "row"), side = ifelse(which == "column", "top", "right"),
    lines_gp = gpar(), labels_gp = gpar(), padding = 0.25, link_width = NULL)
}
\arguments{

  \item{at}{numeric index in the original matrix}
  \item{labels}{corresponding labels}
  \item{which}{column annotaiton or row annotation}
  \item{side}{side of the labels. If it is a column annotation, permitted values are "top" and "bottom"; If it is a row annotation, permitted values are "left" and "right".}
  \item{lines_gp}{graphic settings for the segments}
  \item{labels_gp}{graphic settings for the labels}
  \item{padding}{padding between labels if they are attached to each other}
  \item{link_width,}{width of the segments.}

}
\details{
Sometimes there are many rows or columns in the heatmap and we want to mark some of the rows.
This annotation function is used to mark these rows and connect labels and corresponding rows
with links.
}
\value{
A graphic function which can be set in \code{\link{HeatmapAnnotation}} constructor method.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(10000), nr = 1000)
labels = sample(letters, 20, replace = TRUE)
Heatmap(mat, show_row_dend = FALSE, show_column_dend = FALSE) + 
rowAnnotation(link = row_anno_link(at = sample(1000, 20), labels = labels),
    width = unit(1, "cm") + max_text_width(labels))

}
