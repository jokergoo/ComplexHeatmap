\name{draw-HeatmapAnnotation-method}
\alias{draw,HeatmapAnnotation-method}
\title{
Draw the Heatmap Annotations
}
\description{
Draw the Heatmap Annotations
}
\usage{
\S4method{draw}{HeatmapAnnotation}(object, index, k = 1, n = 1, ...,
    test = FALSE)
}
\arguments{

  \item{object}{A \code{\link{HeatmapAnnotation-class}} object.}
  \item{index}{A vector of indices.}
  \item{k}{The current slice index for the annotation if it is split.}
  \item{n}{Total number of slices.}
  \item{...}{Pass to \code{\link[grid]{viewport}} which contains all the annotations.}
  \item{test}{Is it in test mode? The value can be logical or a text which is plotted as the title of plot.}

}
\value{
No value is returned.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL
}
