\name{sizeAssign.SingleAnnotation}
\alias{sizeAssign.SingleAnnotation}
\alias{size<-.SingleAnnotation}
\title{
Assign the Size to the SingleAnnotation Object
}
\description{
Assign the Size to the SingleAnnotation Object
}
\usage{
\method{size}{SingleAnnotation}(x, ...) <- value
}
\arguments{

  \item{x}{The \code{\link{SingleAnnotation-class}} object.}
  \item{value}{A \code{\link[grid]{unit}} object.}
  \item{...}{Other arguments.}

}
\details{
It assigns to the width if it is a row annotation and the height if it is a column annotation.

Internally used.
}
\examples{
# There is no example
NULL

}
