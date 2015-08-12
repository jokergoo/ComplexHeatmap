\name{add_heatmap-HeatmapList-method}
\alias{add_heatmap,HeatmapList-method}
\title{
Add heatmaps and row annotations to the heatmap list
}
\description{
Add heatmaps and row annotations to the heatmap list
}
\usage{
\S4method{add_heatmap}{HeatmapList}(object, x)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object.}
  \item{x}{a \code{\link{Heatmap-class}} object or a \code{\link{HeatmapAnnotation-class}} object or a \code{\link{HeatmapList-class}} object.}

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
ht_list = ht + ht
add_heatmap(ht_list, ht)

ha = HeatmapAnnotation(points = anno_points(1:12, which = "row"), 
    which = "row")
add_heatmap(ht_list, ha)

}
