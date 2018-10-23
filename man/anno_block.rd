\name{anno_block}
\alias{anno_block}
\title{
Block annotation
}
\description{
Block annotation
}
\usage{
anno_block(gp = gpar(), labels = NULL, labels_gp = gpar(), labels_rot = ifelse(which == "row", 90, 0),
    which = c("column", "row"), width = NULL, height = NULL)
}
\arguments{

  \item{gp}{Graphic parameters.}
  \item{labels}{Labels put on blocks.}
  \item{labels_gp}{Graphic parameters for labels.}
  \item{labels_rot}{Rotation for labels.}
  \item{which}{Is it a row annotation or a column annotation?}
  \item{width}{Width of the annotation. The value should be an absolute unit. Width is not allowed to be set for column annotation.}
  \item{height}{Height of the annotation. The value should be an absolute unit. Height is not allowed to be set for row annotation.}

}
\details{
The block annotation is used for representing slices. The length of all arguments should be 1 or the number of slices.
}
\value{
An annotation function which can be used in \code{\link{HeatmapAnnotation}}.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#block-annotation}
}
\examples{
Heatmap(matrix(rnorm(100), 10), 
    top_annotation = HeatmapAnnotation(foo = anno_block(gp = gpar(fill = 2:4),
        labels = c("group1", "group2", "group3"), labels_gp = gpar(col = "white"))),
    column_km = 3,
    left_annotation = rowAnnotation(foo = anno_block(gp = gpar(fill = 2:4),
        labels = c("group1", "group2", "group3"), labels_gp = gpar(col = "white"))),
    row_km = 3)
}
