\name{size.AnnotationFunction}
\alias{size.AnnotationFunction}
\title{
Size of the AnnotationFunction Object
}
\description{
Size of the AnnotationFunction Object
}
\usage{
\method{size}{AnnotationFunction}(x, ...)
}
\arguments{

  \item{x}{The \code{\link{AnnotationFunction-class}} object.}
  \item{...}{Other arguments.}

}
\details{
It returns the width if it is a row annotation and the height if it is a column annotation.

Internally used.
}
\examples{
anno = anno_points(1:10)
ComplexHeatmap:::size(anno)
anno = anno_points(1:10, which = "row")
ComplexHeatmap:::size(anno)
}
