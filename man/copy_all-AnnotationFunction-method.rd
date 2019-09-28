\name{copy_all-AnnotationFunction-method}
\alias{copy_all,AnnotationFunction-method}
\title{
Copy the AnnotationFunction Object
}
\description{
Copy the AnnotationFunction Object
}
\usage{
\S4method{copy_all}{AnnotationFunction}(object)
}
\arguments{

  \item{object}{The \code{\link{AnnotationFunction-class}} object.}

}
\details{
In \code{\link{AnnotationFunction-class}}, there is an environment which
stores some external variables for the annotation function (specified by the
\code{var_import} argument when constructing the \code{\link{AnnotationFunction-class}}
object. This \code{\link{copy_all,AnnotationFunction-method}} hard copies all the
variables into a new isolated environment.

The environment is at \code{object@var_env}.
}
\examples{
# There is no example
NULL

}
