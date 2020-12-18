\name{namesAssign.HeatmapAnnotation}
\alias{namesAssign.HeatmapAnnotation}
\alias{names<-.HeatmapAnnotation}
\title{
Assign Annotation Names
}
\description{
Assign Annotation Names
}
\usage{
\method{names}{HeatmapAnnotation}(x) <- value
}
\arguments{

  \item{x}{A \code{\link{HeatmapAnnotation-class}} object.}
  \item{value}{A vector of new names.}

}
\examples{
ha = HeatmapAnnotation(foo = 1:10, bar = anno_points(10:1))
names(ha) = c("A", "B")
names(ha)
}
