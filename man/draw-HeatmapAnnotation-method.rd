\name{draw-HeatmapAnnotation-method}
\alias{draw,HeatmapAnnotation-method}
\title{
Draw the heatmap annotations
}
\description{
Draw the heatmap annotations
}
\usage{
\S4method{draw}{HeatmapAnnotation}(object, index, k = NULL, n = NULL, align_to = "bottom", ...)
}
\arguments{

  \item{object}{a \code{\link{HeatmapAnnotation-class}} object.}
  \item{index}{a vector of order.}
  \item{k}{if row annotation is splitted, the value identifies which row slice.}
  \item{n}{total number of row slices.}
  \item{align_to}{if the allocated space is more than than the column annotation itself, should the viewport be aligned to the top or bottom?}
  \item{...}{pass to \code{\link[grid]{viewport}} which contains all annotations.}

}
\details{
A viewport is created. Mostly, this method is used inside \code{\link{draw,HeatmapList-method}}.
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
grid.newpage(); draw(ha, 1:6)
grid.newpage(); draw(ha, 6:1)

ha = HeatmapAnnotation(df = df, col = list(type = c("a" =  "red", "b" = "blue")))
grid.newpage(); draw(ha, 1:6)

ha = HeatmapAnnotation(df = df, col = list(type = c("a" =  "red", "b" = "blue")), 
    which = "row")
grid.newpage(); draw(ha, 1:6)

ha = HeatmapAnnotation(points = anno_points(1:6))
grid.newpage(); draw(ha, 1:6)

ha = HeatmapAnnotation(histogram = anno_barplot(1:6))
grid.newpage(); draw(ha, 1:6)

mat = matrix(rnorm(36), 6)
ha = HeatmapAnnotation(boxplot = anno_boxplot(mat))
grid.newpage(); draw(ha, 1:6)

}
