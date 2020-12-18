\name{sizeAssign.HeatmapAnnotation}
\alias{sizeAssign.HeatmapAnnotation}
\alias{size<-.HeatmapAnnotation}
\title{
Assign the Size to the HeatmapAnnotation Object
}
\description{
Assign the Size to the HeatmapAnnotation Object
}
\usage{
\method{size}{HeatmapAnnotation}(x, ...) <- value
}
\arguments{

  \item{x}{The \code{\link{HeatmapAnnotation-class}} object.}
  \item{value}{A \code{\link[grid]{unit}} object.}
  \item{...}{Other arguments.}

}
\details{
It assigns the width if it is a row annotation and the height if it is a column annotation.

Internally used.
}
\examples{
# There is no example
NULL

}
