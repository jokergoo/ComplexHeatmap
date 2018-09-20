\name{nobs.AnnotationFunction}
\alias{nobs.AnnotationFunction}
\title{
Number of Observations
}
\description{
Number of Observations
}
\usage{
\method{nobs}{AnnotationFunction}(object, ...)
}
\arguments{

  \item{object}{The \code{\link{AnnotationFunction-class}} object.}
  \item{...}{other arguments}

}
\details{
It returns the \code{n} slot in the object. If there does not exist, it returns \code{NA}.
}
\examples{
anno = anno_points(1:10)
nobs(anno)
}
