\name{[.AnnotationFunction}
\alias{Extract.AnnotationFunction}
\alias{[.AnnotationFunction}
\title{
Subset an AnnotationFunction Object
}
\description{
Subset an AnnotationFunction Object
}
\usage{
\method{[}{AnnotationFunction}(x, i)
}
\arguments{

  \item{x}{An \code{\link{AnnotationFunction-class}} object.}
  \item{i}{A vector of indices.}

}
\details{
One good thing for designing the \code{\link{AnnotationFunction-class}} is it can be subsetted,
and this is the base for the splitting of the annotations.
}
\examples{
anno = anno_simple(1:10)
anno[1:5]
draw(anno[1:5], test = "subset of column annotation")
}
