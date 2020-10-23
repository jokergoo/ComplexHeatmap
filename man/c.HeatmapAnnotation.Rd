\name{c.HeatmapAnnotation}
\alias{c.HeatmapAnnotation}
\title{
Concatenate Heatmap Annotations
}
\description{
Concatenate Heatmap Annotations
}
\usage{
\method{c}{HeatmapAnnotation}(..., gap = unit(1, "points"))
}
\arguments{

  \item{...}{\code{\link{HeatmapAnnotation-class}} objects.}
  \item{gap}{Gap between the groups of annotations.}

}
\details{
The heatmap annotations should have same number of observations.
}
\examples{
ha1 = HeatmapAnnotation(foo = 1:10)
ha2 = HeatmapAnnotation(bar = anno_points(10:1))
ha = c(ha1, ha2)
ha
ha3 = HeatmapAnnotation(sth = cbind(1:10, 10:1))
ha = c(ha1, ha2, ha3, gap = unit(c(1, 4), "mm"))
ha
}
