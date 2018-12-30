\name{re_size-HeatmapAnnotation-method}
\alias{re_size,HeatmapAnnotation-method}
\title{
Resize the Width or Height of Heatmap Annotations
}
\description{
Resize the Width or Height of Heatmap Annotations
}
\usage{
\S4method{re_size}{HeatmapAnnotation}(object,
    annotation_height = NULL,
    annotation_width = NULL,
    height = NULL,
    width = NULL,
    anno_simple_size = ht_opt$anno_simple_size,
    simple_anno_size_adjust = FALSE)
}
\arguments{

  \item{object}{A \code{\link{HeatmapAnnotation-class}} object.}
  \item{annotation_height}{A vector of of annotation heights in \code{\link[grid]{unit}} class.}
  \item{annotation_width}{A vector of of annotation widths in \code{\link[grid]{unit}} class.}
  \item{height}{The height of the complete heatmap annotation.}
  \item{width}{The width of the complete heatmap annotation.}
  \item{anno_simple_size}{The size of one line of the simple annotation.}
  \item{simple_anno_size_adjust}{Whether adjust the size of the simple annotation?}

}
\details{
The function only adjust height for column annotations and width for row annotations.

The basic rules are (take \code{height} and \code{annotation_height} for example:

1. If \code{annotation_height} is set and all
   \code{annotation_height} are absolute units, \code{height} is ignored.
2. If \code{annotation_height} contains non-absolute units, \code{height} also need to be set and the
   non-absolute units should be set in a simple form such as 1:10 or \code{unit(1, "null")}.
3. \code{anno_simple_size} is only used when \code{annotation_height} is NULL.
4. If only \code{height} is set, non-simple annotation is adjusted while keeps simple anntation unchanged.
5. If only \code{height} is set and all annotations are simple annotations, all anntations are adjusted,
     and \code{anno_simple_size} is disabled.
6. If \code{simple_anno_size_adjust} is \code{FALSE}, the size of the simple annotations will not change.
}
\alias{re_size}
\examples{
# There is no example
NULL
}
