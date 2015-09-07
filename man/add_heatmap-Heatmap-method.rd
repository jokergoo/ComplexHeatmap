\name{add_heatmap-Heatmap-method}
\alias{add_heatmap,Heatmap-method}
\title{
Add heatmaps or row annotations as a heatmap list
}
\description{
Add heatmaps or row annotations as a heatmap list
}
\usage{
\S4method{add_heatmap}{Heatmap}(object, x)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object.}
  \item{x}{a \code{\link{Heatmap-class}} object, a \code{\link{HeatmapAnnotation-class}} object or a \code{\link{HeatmapList-class}} object.}

}
\details{
There is a shortcut function \code{+.AdditiveUnit}.
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
add_heatmap(ht, ht)

ha = HeatmapAnnotation(points = anno_points(1:12, which = "row"), 
    which = "row")
add_heatmap(ht, ha)

}
