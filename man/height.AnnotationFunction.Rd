\name{height.AnnotationFunction}
\alias{height.AnnotationFunction}
\title{
Height of the AnnotationFunction Object
}
\description{
Height of the AnnotationFunction Object
}
\usage{
\method{height}{AnnotationFunction}(x, ...)
}
\arguments{

  \item{x}{The \code{\link{AnnotationFunction-class}} object.}
  \item{...}{Other arguments.}

}
\details{
Internally used.
}
\examples{
anno = anno_points(1:10)
ComplexHeatmap:::height(anno)
anno = anno_points(1:10, which = "row")
ComplexHeatmap:::height(anno)
}
