\name{default_axis_param}
\alias{default_axis_param}
\title{
The Default Parameters for Annotation Axis
}
\description{
The Default Parameters for Annotation Axis
}
\usage{
default_axis_param(which)
}
\arguments{

  \item{which}{Whether it is for column annotation or row annotation?}

}
\details{
There are following parameters for the annotation axis:

\describe{
  \item{at}{The breaks of axis. By default it is automatically inferred.}
  \item{labels}{The corresponding axis labels.}
  \item{labels_rot}{The rotation of the axis labels.}
  \item{gp}{Graphc parameters of axis labels. The value should be a \code{\link[grid]{unit}} object.}
  \item{side}{If it is for column annotation, the value should only be one of \code{left} and \code{right}. If it is for row annotation, the value should only be one of \code{top} and \code{bottom}.}
  \item{facing}{Whether the axis faces to the outside of the annotation region or inside. Sometimes when appending more than one heatmaps, the axes of column annotations of one heatmap might overlap to the neighbouring heatmap, setting \code{facing} to \code{inside} may invoild it.}
  \item{direction}{The direction of the axis. Value should be "normal" or "reverse".}
}

All the parameters are passed to \code{\link{annotation_axis_grob}} to construct an axis grob.
}
\examples{
default_axis_param("column")
default_axis_param("row")
}
