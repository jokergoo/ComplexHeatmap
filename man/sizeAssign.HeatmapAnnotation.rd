\name{size<-.HeatmapAnnotation}
\alias{size<-.HeatmapAnnotation}
\title{
Assign the Size to the HeatmapAnnotation x
}
\description{
Assign the Size to the HeatmapAnnotation x
}
\usage{
\method{size}{HeatmapAnnotation}(x, ...) <- value
}
\arguments{

  \item{x}{The \code{\link{HeatmapAnnotation-class}} x.}
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
