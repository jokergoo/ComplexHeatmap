\name{draw-AnnotationFunction-method}
\alias{draw,AnnotationFunction-method}
\title{
Draw the AnnotationFunction Object
}
\description{
Draw the AnnotationFunction Object
}
\usage{
\S4method{draw}{AnnotationFunction}(object, index, k = 1, n = 1, test = FALSE, ...)
}
\arguments{

  \item{object}{The \code{\link{AnnotationFunction-class}} object.}
  \item{index}{Index of observations.}
  \item{k}{Current slice index.}
  \item{n}{Total number of slices.}
  \item{test}{Is it in test mode? The value can be logical or a text which is plotted as the title of plot.}
  \item{...}{Pass to \code{\link[grid]{viewport}}.}

}
\details{
Normally it is called internally by the \code{\link{SingleAnnotation-class}}.

When \code{test} is set to \code{TRUE}, the annotation graphic is directly drawn,
which is generally for testing purpose.
}
\examples{
# There is no example
NULL

}
