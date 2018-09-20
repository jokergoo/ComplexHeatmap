\name{copy_all-SingleAnnotation-method}
\alias{copy_all,SingleAnnotation-method}
\title{
Copy the SingleAnnotation object
}
\description{
Copy the SingleAnnotation object
}
\usage{
\S4method{copy_all}{SingleAnnotation}(object)
}
\arguments{

  \item{object}{The \code{\link{SingleAnnotation-class}} object.}

}
\details{
Since the SingleAnnotation object always contains an \code{\link{AnnotationFunction-class}} object,
it calls \code{\link{copy_all,AnnotationFunction-method}} to hard copy the variable environment.
}
\examples{
# There is no example
NULL

}
