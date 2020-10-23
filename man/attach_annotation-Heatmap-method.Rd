\name{attach_annotation-Heatmap-method}
\alias{attach_annotation,Heatmap-method}
\alias{attach_annotation}
\title{
Attach heatmap annotations to the heatmap
}
\description{
Attach heatmap annotations to the heatmap
}
\usage{
\S4method{attach_annotation}{Heatmap}(object, ha, side = c("top", "bottom", "left", "right"),
    gap = unit(1, "points"))
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{ha}{A \code{\link{HeatmapAnnotation-class}} object.}
  \item{side}{Which side of the heatmap. Value should be in "top", "bottom", "left", "right".}
  \item{gap}{Space between the two heatmap annotations.}

}
\examples{
m = matrix(rnorm(100), 10)
ht = Heatmap(m)
ha = HeatmapAnnotation(foo = 1:10)
ht = attach_annotation(ht, ha)
ht
ha2 = HeatmapAnnotation(bar = letters[1:10])
ht = attach_annotation(ht, ha2)
ht
}
