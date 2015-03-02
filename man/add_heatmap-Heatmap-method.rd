\name{add_heatmap-Heatmap-method}
\alias{add_heatmap,Heatmap-method}
\title{
Add two heatmaps or add row annotations as a heatmap list  


}
\description{
Add two heatmaps or add row annotations as a heatmap list  


}
\usage{
\S4method{add_heatmap}{Heatmap}(object, x)
}
\arguments{

  \item{object}{a \code{\link{Heatmap}} object.}
  \item{x}{a \code{\link{Heatmap}} object, a \code{\link{HeatmapAnnotation}} object or a \code{\link{HeatmapList}} object.}

}
\details{
There is a shortcut function \code{+.Heatmap}.  


}
\value{
A \code{\link{HeatmapList}} object.  


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

ha = HeatmapAnnotation(points = anno_simple(1:12, type = "p", which = "row"), which = "row")
add_heatmap(ht, ha)
}
