\name{names.HeatmapAnnotation}
\alias{names.HeatmapAnnotation}
\title{
Annotation Names
}
\description{
Annotation Names
}
\usage{
\method{names}{HeatmapAnnotation}(x)
}
\arguments{

  \item{x}{A \code{\link{HeatmapAnnotation-class}} object.}

}
\examples{
ha = HeatmapAnnotation(foo = 1:10, bar = anno_points(10:1))
names(ha)
}
