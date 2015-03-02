\name{+.Heatmap}
\alias{+.Heatmap}
\title{
Add two heatmaps or add row annotations as a heatmap list  


}
\description{
Add two heatmaps or add row annotations as a heatmap list  


}
\usage{
\method{+}{Heatmap}(x, y)
}
\arguments{

  \item{x}{a \code{\link{Heatmap}} object.}
  \item{y}{a \code{\link{Heatmap}} object, a \code{\link{HeatmapAnnotation}} object or a \code{\link{HeatmapList}} object.}

}
\details{
It is only a shortcut function. It actually calls \code{\link{add_heatmap,Heatmap-method}}.  


}
\value{
a \code{\link{HeatmapList}} object.  


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

ha = HeatmapAnnotation(points = anno_simple(1:12, type = "p", which = "row"), which = "row")
ht + ha
}
