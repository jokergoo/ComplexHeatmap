\name{[.HeatmapAnnotation}
\alias{Extract.HeatmapAnnotation}
\alias{[.HeatmapAnnotation}
\title{
Subset the HeatmapAnnotation object
}
\description{
Subset the HeatmapAnnotation object
}
\usage{
\method{[}{HeatmapAnnotation}(x, i, j)
}
\arguments{

  \item{x}{A \code{\link{HeatmapAnnotation-class}} object.}
  \item{i}{Index of observations.}
  \item{j}{Index of annotations.}

}
\examples{
ha = HeatmapAnnotation(foo = 1:10, bar = anno_points(10:1),
	sth = cbind(1:10, 10:1))
ha[1:5, ]
ha[, c("foo", "bar")]
ha[, 1:2]
ha[1:5, c("foo", "sth")]
}
