\name{sizeAssign.AnnotationFunction}
\alias{sizeAssign.AnnotationFunction}
\alias{size<-.AnnotationFunction}
\title{
Assign the Size to the AnnotationFunction Object
}
\description{
Assign the Size to the AnnotationFunction Object
}
\usage{
\method{size}{AnnotationFunction}(x, ...) <- value
}
\arguments{

  \item{x}{The \code{\link{AnnotationFunction-class}} object.}
  \item{value}{A \code{\link[grid]{unit}} object.}
  \item{...}{Other arguments.}

}
\details{
It assigns to the width if it is a row annotation and the height if it is a column annotation.

Internally used.
}
\examples{
anno = anno_points(1:10)
ComplexHeatmap:::size(anno) = unit(4, "cm")
ComplexHeatmap:::size(anno)
}
