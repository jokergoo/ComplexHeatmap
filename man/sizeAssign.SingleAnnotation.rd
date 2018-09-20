\name{size<-.SingleAnnotation}
\alias{size<-.SingleAnnotation}
\title{
Assign the Size of the SingleAnnotation x
}
\description{
Assign the Size of the SingleAnnotation x
}
\usage{
\method{size}{SingleAnnotation}(x, ...) <- value
}
\arguments{

  \item{x}{The \code{\link{SingleAnnotation-class}} x.}
  \item{value}{A \code{\link[grid]{unit}} x.}
  \item{...}{other arguments}

}
\details{
It assigns the width if it is a row annotation and the height if it is a column annotation.
}
\examples{
# There is no example
NULL
}
