\name{size<-.AnnotationFunction}
\alias{size<-.AnnotationFunction}
\title{
Assign the Size to the AnnotationFunction x
}
\description{
Assign the Size to the AnnotationFunction x
}
\usage{
\method{size}{AnnotationFunction}(x, ...) <- value
}
\arguments{

  \item{x}{The \code{\link{AnnotationFunction-class}} x.}
  \item{value}{A \code{\link[grid]{unit}} x.}
  \item{...}{other arguments}

}
\details{
It assigns the width if it is a row annotation and the height if it is a column annotation.
}
\examples{
anno = anno_points(1:10)
size(anno) = unit(4, "cm")
size(anno)
}
