\name{grid.annotation_axis}
\alias{grid.annotation_axis}
\title{
Draw Annotation Axis
}
\description{
Draw Annotation Axis
}
\usage{
grid.annotation_axis(at = NULL, labels = at, labels_rot = 0, gp = gpar(),
    side = "left", facing = "outside", direction = "normal")
}
\arguments{

  \item{at}{Break values. If it is not specified, it is inferred from data scale in current viewport.}
  \item{labels}{Corresponding labels.}
  \item{labels_rot}{Rotations of labels.}
  \item{gp}{Graphic parameters.}
  \item{side}{side of the axis of the annotation viewport.}
  \item{facing}{Facing of the axis.}
  \item{direction}{direction of the axis. Value should be "normal" or "reverse".}

}
\details{
It uses \code{\link{annotation_axis_grob}} to construct the grob object, then use \code{\link[grid]{grid.draw}}
to draw the axis.
}
\examples{
# See examples in `annotation_axis_grob`
NULL
}
