\name{height.AnnotationFunction}
\alias{height.AnnotationFunction}
\title{
Height of the AnnotationFunction x
}
\description{
Height of the AnnotationFunction x
}
\usage{
\method{height}{AnnotationFunction}(x, ...)
}
\arguments{

  \item{x}{The \code{\link{AnnotationFunction-class}} x.}
  \item{...}{other arguments}

}
\examples{
anno = anno_points(1:10)
height(anno)
anno = anno_points(1:10, which = "row")
height(anno)
}
