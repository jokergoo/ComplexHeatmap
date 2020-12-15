\name{upset_top_annotation}
\alias{upset_top_annotation}
\title{
Default UpSet Top Annotation
}
\description{
Default UpSet Top Annotation
}
\usage{
upset_top_annotation(m,
    gp = gpar(fill = "black"),
    height = unit(ifelse(set_on_rows, 3, 2), "cm"),
    show_annotation_name = TRUE,
    annotation_name_gp = gpar(),
    annotation_name_offset = NULL,
    annotation_name_side = "left",
    annotation_name_rot = 0,
    ...)
}
\arguments{

  \item{m}{A combination matrix which is as same as the one for \code{\link{UpSet}}.}
  \item{gp}{Graphic parameters for bars.}
  \item{height}{The height of the top annotation.}
  \item{show_annotation_name}{Whether show annotation names?}
  \item{annotation_name_gp}{Graphic parameters for anntation names.}
  \item{annotation_name_offset}{Offset to the annotation name, a \code{\link[grid]{unit}} object.}
  \item{annotation_name_side}{Side of the annotation name.}
  \item{annotation_name_rot}{Rotation of the annotation name, it can only take values in \code{c(00, 90, 180, 270)}.}
  \item{...}{Passed to \code{\link{anno_barplot}}.}

}
\details{
The default top annotation is actually barplot implemented by \code{\link{anno_barplot}}. For
how to set the top annotation or bottom annotation in \code{\link{UpSet}}, please refer to \code{\link{UpSet}}.

If you want to use \code{\link{decorate_annotation}} function, the annotation name for the "sets"
is \code{set_size} and the annotation name for the "intersection sets" are \code{intersection_size}
and if under the union mode, it is \code{union_size}.
}
\examples{
# There is no example
NULL

}
