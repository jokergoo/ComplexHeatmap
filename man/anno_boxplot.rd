\name{anno_boxplot}
\alias{anno_boxplot}
\title{
Using boxplot as annotation
}
\description{
Using boxplot as annotation
}
\usage{
anno_boxplot(x, which = c("column", "row"), border = TRUE,
    gp = gpar(fill = "#CCCCCC"), ylim = NULL, outline = TRUE,
    pch = 16, size = unit(2, "mm"), axis = FALSE, axis_side = NULL,
    axis_gp = gpar(fontsize = 8), axis_direction = c("normal", "reverse"))
}
\arguments{

  \item{x}{a matrix or a list. If \code{x} is a matrix and if \code{which} is \code{column}, statistics for boxplot is calculated by columns, if \code{which} is \code{row}, the calculation is by rows.}
  \item{which}{is the annotation a column annotation or a row annotation?}
  \item{border}{whether show border of the annotation compoment}
  \item{gp}{graphic parameters}
  \item{ylim}{data ranges.}
  \item{outline}{whether draw outliers}
  \item{pch}{point type}
  \item{size}{point size}
  \item{axis}{whether add axis}
  \item{axis_side}{if it is placed as column annotation, value can only be "left" or "right". If it is placed as row annotation, value can only be "bottom" or "top".}
  \item{axis_gp}{graphic parameters for axis}
  \item{axis_direction}{if the annotation is row annotation, should the axis be from left to right (default) or follow the reversed direction?}

}
\value{
A graphic function which can be set in \code{\link{HeatmapAnnotation}} constructor method.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(32), nrow = 4)
f = anno_boxplot(mat)
grid.newpage(); f(1:8)

f = anno_boxplot(mat, which = "row")
grid.newpage(); f(1:4)

lt = lapply(1:4, function(i) rnorm(8))
f = anno_boxplot(lt)
grid.newpage(); f(1:4)

}
