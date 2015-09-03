\name{+.AdditiveUnit}
\alias{+.AdditiveUnit}
\title{
Add heatmaps or row annotations to a heatmap list
}
\description{
Add heatmaps or row annotations to a heatmap list
}
\usage{
\method{+}{AdditiveUnit}(x, y)
}
\arguments{

  \item{x}{a \code{\link{Heatmap-class}} object, a \code{\link{HeatmapAnnotation-class}} object or a \code{\link{HeatmapList-class}} object.}
  \item{y}{a \code{\link{Heatmap-class}} object, a \code{\link{HeatmapAnnotation-class}} object or a \code{\link{HeatmapList-class}} object.}

}
\details{
It is only a helper function. It actually calls \code{\link{add_heatmap,Heatmap-method}}, \code{\link{add_heatmap,HeatmapList-method}}
or \code{\link{add_heatmap,HeatmapAnnotation-method}} depending on the class of the input objects.

The \code{\link{HeatmapAnnotation-class}} object to be added should only be row annotations.
}
\value{
A \code{\link{HeatmapList-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(80, 2), 8, 10)
mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
rownames(mat) = letters[1:12]
colnames(mat) = letters[1:10]

ht = Heatmap(mat)
ht + ht
ht + ht + ht

ht_list = ht + ht
ht + ht_list

ha = rowAnnotation(points = row_anno_points(1:12))
ht + ha
ht_list + ha

ha + ha + ht

}
