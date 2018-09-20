\name{width.AnnotationFunction}
\alias{width.AnnotationFunction}
\title{
Width of the AnnotationFunction x
}
\description{
Width of the AnnotationFunction x
}
\usage{
\method{width}{AnnotationFunction}(x, ...)
}
\arguments{

  \item{x}{The \code{\link{AnnotationFunction-class}} x.}
  \item{...}{other arguments}

}
\examples{
anno = anno_points(1:10)
width(anno)
anno = anno_points(1:10, which = "row")
width(anno)
}
