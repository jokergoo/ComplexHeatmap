\name{draw-HeatmapAnnotation-method}
\alias{draw,HeatmapAnnotation-method}
\title{
Draw the heatmap annotations  


}
\description{
Draw the heatmap annotations  


}
\usage{
\S4method{draw}{HeatmapAnnotation}(object, index, ...)
}
\arguments{

  \item{object}{a \code{\link{HeatmapAnnotation}} object}
  \item{index}{the index of rows or columns}
  \item{...}{pass to \code{\link[grid]{viewport}} which contains all annotations.}

}
\details{
A viewport is created.  


}
\value{
No value is returned.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
df = data.frame(type = c("a", "a", "a", "b", "b", "b"))
ha = HeatmapAnnotation(df = df)
draw(ha, 1:6)
draw(ha, 6:1)

ha = HeatmapAnnotation(df = df, col = list(type = c("a" =  "red", "b" = "blue")))
draw(ha, 1:6)

ha = HeatmapAnnotation(df = df, col = list(type = c("a" =  "red", "b" = "blue")), which = "row")
draw(ha, 1:6)

ha = HeatmapAnnotation(points = anno_simple(1:6, type = "p"))
draw(ha, 1:6)

ha = HeatmapAnnotation(histogram = anno_simple(1:6, type = "histogram"))
draw(ha, 1:6)

mat = matrix(rnorm(36), 6)
ha = HeatmapAnnotation(boxplot = anno_boxplot(mat))
draw(ha, 1:6)

}