\name{[.SingleAnnotation}
\alias{Extract.SingleAnnotation}
\alias{[.SingleAnnotation}
\title{
Subset an SingleAnnotation Object
}
\description{
Subset an SingleAnnotation Object
}
\usage{
\method{[}{SingleAnnotation}(x, i)
}
\arguments{

  \item{x}{An \code{\link{SingleAnnotation-class}} object.}
  \item{i}{A vector of indices.}

}
\details{
The SingleAnnotation class object is subsettable only if the containing \code{\link{AnnotationFunction-class}}
object is subsettable. All the \code{anno_*} functions are subsettable, so if the SingleAnnotation object
is constructed by one of these functions, it is also subsettable.
}
\examples{
ha = SingleAnnotation(value = 1:10)
ha[1:5]
draw(ha[1:5], test = "ha[1:5]")
}
