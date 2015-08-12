\name{draw-Heatmap-method}
\alias{draw,Heatmap-method}
\title{
Draw a single heatmap
}
\description{
Draw a single heatmap
}
\usage{
\S4method{draw}{Heatmap}(object, internal = FALSE, test = FALSE, ...)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object.}
  \item{internal}{only used inside the calling of \code{\link{draw,HeatmapList-method}}. Only heatmap without legends will be drawn.}
  \item{test}{only for testing}
  \item{...}{pass to \code{\link{draw,HeatmapList-method}}.}

}
\details{
The function creates a \code{\link{HeatmapList-class}} object which only contains a single heatmap
and call \code{\link{draw,HeatmapList-method}} to make the final heatmap.
}
\value{
This function returns no value.
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
draw(ht, heatmap_legend_side = "left")

}
