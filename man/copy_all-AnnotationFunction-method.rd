\name{copy_all-AnnotationFunction-method}
\alias{copy_all,AnnotationFunction-method}
\title{
Copy the AnnotationFunction object
}
\description{
Copy the AnnotationFunction object
}
\usage{
\S4method{copy_all}{AnnotationFunction}(object)
}
\arguments{

  \item{object}{The \code{\link{AnnotationFunction-class}} object.}

}
\details{
In \code{\link{AnnotationFunction-class}}, there is an environment which stores some external variables
for the annotation function. This \code{\link{copy_all,AnnotationFunction-method}} hard copy all the variables
in that environment to a new environment.

The environment is at \code{object@var_env}.
}
\examples{
# There is no example
NULL

}
