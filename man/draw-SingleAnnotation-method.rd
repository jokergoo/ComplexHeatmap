\name{draw-SingleAnnotation-method}
\alias{draw,SingleAnnotation-method}
\title{
Draw the single annotation
}
\description{
Draw the single annotation
}
\usage{
\S4method{draw}{SingleAnnotation}(object, index, k = NULL, n = NULL)
}
\arguments{

  \item{object}{a \code{\link{SingleAnnotation-class}} object.}
  \item{index}{a vector of orders}
  \item{k}{if row annotation is splitted, the value identifies which row slice. It is only used for the naems of the viewport which contains the annotation graphics.}
  \item{n}{total number of row slices}

}
\details{
A viewport is created.

The graphics would be different depending the annotation is a row annotation or a column annotation.
}
\value{
No value is returned.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
anno = SingleAnnotation(name = "test", value = c("a", "a", "a", "b", "b", "b"))
grid.newpage(); draw(anno, 1:5)
grid.newpage(); draw(anno, c(1, 4, 3, 5, 2))

anno = SingleAnnotation(value = c("a", "a", "a", "b", "b", "b"), 
    col = c("a" = "red", "b" = "blue"))
grid.newpage(); draw(anno, 1:5)
grid.newpage(); draw(anno, c(1, 4, 3, 5, 2))

anno = SingleAnnotation(value = c("a", "a", "a", "b", "b", "b"), 
    col = c("a" = "red", "b" = "blue"), which = "row")
grid.newpage(); draw(anno, 1:5)

anno = SingleAnnotation(value = 1:10)
grid.newpage(); draw(anno, 1:10)

require(circlize)
anno = SingleAnnotation(value = 1:10, col = colorRamp2(c(1, 10), c("blue", "red")))
grid.newpage(); draw(anno, 1:10)

anno = SingleAnnotation(fun = anno_points(1:10))
grid.newpage(); draw(anno, 1:10)

}
