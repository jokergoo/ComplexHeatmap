\name{HeatmapAnnotation}
\alias{HeatmapAnnotation}
\title{
Constructor Method for HeatmapAnnotation class
}
\description{
Constructor Method for HeatmapAnnotation class
}
\usage{
HeatmapAnnotation(...,
    df = NULL, name, col, na_col = "grey",
    annotation_legend_param = list(),
    show_legend = TRUE,
    which = c("column", "row"),
    gp = gpar(col = NA),
    border = FALSE,
    gap = unit(1, "points"),
    
    show_annotation_name = TRUE,
    annotation_label = NULL,
    annotation_name_gp = gpar(),
    annotation_name_offset = NULL,
    annotation_name_side = ifelse(which == "column", "right", "bottom"),
    annotation_name_rot = NULL,
    annotation_name_align = FALSE,
    
    annotation_height = NULL,
    annotation_width = NULL,
    height = NULL,
    width = NULL,
    simple_anno_size = ht_opt$simple_anno_size,
    simple_anno_size_adjust = FALSE)
}
\arguments{

  \item{...}{Name-value pairs where the names correspond to annotation names and values can be a vector, a matrix and an annotation function. Each pair is sent to \code{\link{SingleAnnotation}} to contruct a single annotation.}
  \item{df}{A data frame. Each column will be treated as a simple annotation. The data frame must have column names.}
  \item{name}{Name of the heatmap annotation, optional.}
  \item{col}{A list of colors which contain color mapping to \code{df} or simple annotations defined in \code{...}.  See \code{\link{SingleAnnotation}} for how to set colors.}
  \item{na_col}{Color for \code{NA} values in simple annotations.}
  \item{annotation_legend_param}{A list which contains parameters for annotation legends. See \code{\link{color_mapping_legend,ColorMapping-method}} for all possible options.}
  \item{show_legend}{Whether show annotation legends. The value can be one single value or a vector.}
  \item{which}{Are these row annotations or column annotations?}
  \item{gp}{Graphic parameters for simple annotations (with \code{fill} parameter ignored).}
  \item{border}{border of single annotations.}
  \item{gap}{Gap between annotations. It can be a single value or a vector of \code{\link[grid]{unit}} objects.}
  \item{show_annotation_name}{Whether show annotation names? For column annotation, annotation names are drawn either on the left or the right, and for row annotations, names are draw either on top or at the bottom. The value can be a vector.}
  \item{annotation_label}{Labels for the annotations. By default it is the same as individual annotation names.}
  \item{annotation_name_gp}{Graphic parameters for anntation names. Graphic paramters can be vectors.}
  \item{annotation_name_offset}{Offset to the annotation names, a \code{\link[grid]{unit}} object. The value can be a vector.}
  \item{annotation_name_side}{Side of the annotation names.}
  \item{annotation_name_rot}{Rotation of the annotation names. The value can be a vector.}
  \item{annotation_name_align}{Whether to align the annotation names.}
  \item{annotation_height}{Height of each annotation if annotations are column annotations.}
  \item{annotation_width}{Width of each annotation if annotations are row annotations.}
  \item{height}{Height of the whole column annotations.}
  \item{width}{Width of the whole heatmap annotations.}
  \item{simple_anno_size}{Size of the simple annotation.}
  \item{simple_anno_size_adjust}{Whether also adjust the size of simple annotations when adjusting the whole heatmap annotation.}

}
\details{
For arguments \code{show_legend}, \code{border}, \code{annotation_name_offset}, \code{annotation_name_side}, \code{annotation_name_rot},
\code{show_annotation_name}, they can be set as named vectors to modify values for some of the annotations,
e.g. assuming you have an annotation with name \code{foo}, you can specify \code{border = c(foo = TRUE)} in \code{\link{HeatmapAnnotation}}.

There are three ways to specify heatmap annotations:

1. If the annotation is simply a vector or a matrix, it can be specified like \code{HeatmapAnnotation(foo = 1:10)}.
2. If the annotations are already stored as a data frame, it can be specified like \code{HeatmapAnnotation(df = df)}.
3. For complex annotations, users can use the pre-defined annotation functions such as \code{\link{anno_points}}: \code{HeatmapAnnotation(foo = anno_points(1:10))}.

For more details and examples, please check \url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html.}
}
\value{
A \code{\link{HeatmapAnnotation-class}} object.
}
\seealso{
There are two helper functions: \code{\link{rowAnnotation}} and \code{\link{columnAnnotation}}.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
