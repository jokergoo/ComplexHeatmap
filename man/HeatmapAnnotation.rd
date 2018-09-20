\name{HeatmapAnnotation}
\alias{HeatmapAnnotation}
\title{
Constructor method for HeatmapAnnotation class
}
\description{
Constructor method for HeatmapAnnotation class
}
\usage{
HeatmapAnnotation(...,
    df, name, col, na_col = "grey",
    annotation_legend_param = list(),
    show_legend = TRUE,
    which = c("column", "row"),
    annotation_height = NULL,
    annotation_width = NULL,
    height = NULL,   # total height
    width = NULL,    # total width
    gp = gpar(col = NA),
    gap = unit(0, "mm"),
    show_annotation_name = TRUE,
    annotation_name_gp = gpar(),
    annotation_name_offset = unit(1, "mm"),
    annotation_name_side = ifelse(which == "column", "right", "bottom"),
    annotation_name_rot = ifelse(which == "column", 0, 90))
}
\arguments{

  \item{...}{Name-value pairs where the names correspond to annotation names and values can be a vector, a matrix and an annotation function. Each pair is sent to \code{\link{SingleAnnotation}} to contruct a single annotation.}
  \item{df}{A data frame. Each column will be treated as a simple annotation. The data frame must have column names.}
  \item{name}{Name of the heatmap annotation, optional.}
  \item{col}{A list of colors which contain color mapping to columns in \code{df} and simple annotations define din \code{...}.  See \code{\link{SingleAnnotation}} for how to set colors.}
  \item{na_col}{Color for \code{NA} values in simple annotations.}
  \item{annotation_legend_param}{A list which contains parameters for annotation legends. See \code{\link{color_mapping_legend,ColorMapping-method}} for all possible options.}
  \item{show_legend}{Whether show annotation legend. The value can be one single value or a vector which corresponds to the simple annotations.}
  \item{which}{Are the annotations row annotations or column annotations?}
  \item{annotation_height}{Height of each annotation if annotations are column annotations.}
  \item{annotation_width}{Width of each annotation if annotations are row annotations.}
  \item{height}{Height of the complete column annotations.}
  \item{width}{Width of the complete heatmap annotations.}
  \item{gp}{Graphic parameters for simple annotations (with \code{fill} parameter ignored).}
  \item{gap}{Gap between each two annotation. It can be a single value or a vector of \code{\link[grid]{unit}} objects.}
  \item{show_annotation_name}{Whether show annotation names? For column annotation, annotation names are drawn either on the left or the right, and for row annotations, names are draw either on top to at bottom. The value can be a vector.}
  \item{annotation_name_gp}{Graphic parameters for anntation names. Graphic paramters can be vectors.}
  \item{annotation_name_offset}{Offset to the annotations, \code{\link[grid]{unit}} object. The value can be a vector.}
  \item{annotation_name_side}{Side of the annotation names.}
  \item{annotation_name_rot}{Rotation of the annotation names, can only take values in \code{c(00, 90, 180, 270)}. The value can be a vector.}

}
\details{
There are three ways to specify heatmap annotations:

1. If the annotation is simply a vector or a matrix, it can be specified as \code{HeatmapAnnotation(foo = 1:10)}.
2. If the annotations are already stored as a data frame, it can be specified as \code{HeatmapAnnotation(df = df)}.
3. For complex annotation, users can use the pre-defined annotation functions such as \code{\link{anno_points}}: \code{HeatmapAnnotation(foo = anno_points(1:10))}.
}
\value{
A \code{\link{HeatmapAnnotation-class}} object.
}
\seealso{
There are two shortcut functions: \code{\link{rowAnnotation}} and \code{\link{columnAnnotation}}.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL
}
