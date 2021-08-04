\name{upset_left_annotation}
\alias{upset_left_annotation}
\title{
UpSet Left Annotation
}
\description{
UpSet Left Annotation
}
\usage{
upset_left_annotation(m,
    gp = gpar(fill = "black"),
    axis_param = list(direction = "reverse"),
    width = unit(ifelse(set_on_rows, 2, 3), "cm"),
    show_annotation_name = TRUE,
    annotation_name_gp = gpar(),
    annotation_name_offset = NULL,
    annotation_name_side = "bottom",
    annotation_name_rot = NULL,
    ...)
}
\arguments{

  \item{m}{A combination matrix which is as same as the one for \code{\link{UpSet}}.}
  \item{gp}{Graphic parameters for bars.}
  \item{axis_param}{Parameters for axis.}
  \item{width}{Width of the left annotation.}
  \item{show_annotation_name}{Whether show annotation names?}
  \item{annotation_name_gp}{Graphic parameters for anntation names.}
  \item{annotation_name_offset}{Offset to the annotation name, a \code{\link[grid]{unit}} object.}
  \item{annotation_name_side}{Side of the annotation name.}
  \item{annotation_name_rot}{Rotation of the annotation name, it can only take values in \code{c(00, 90, 180, 270)}.}
  \item{...}{Passed to \code{\link{anno_barplot}}, e.g. to set \code{add_numbers}.}

}
\examples{
# There is no example
NULL

}
