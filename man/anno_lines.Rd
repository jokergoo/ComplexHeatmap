\name{anno_lines}
\alias{anno_lines}
\title{
Lines Annotation
}
\description{
Lines Annotation
}
\usage{
anno_lines(x, which = c("column", "row"), border = TRUE, gp = gpar(),
    add_points = smooth, smooth = FALSE, pch = 16, size = unit(2, "mm"), pt_gp = gpar(), ylim = NULL,
    extend = 0.05, axis = TRUE, axis_param = default_axis_param(which),
    width = NULL, height = NULL)
}
\arguments{

  \item{x}{The value vector. The value can be a vector or a matrix. The length of the vector or the number of rows of the matrix is taken as the number of the observations of the annotation.}
  \item{which}{Whether it is a column annotation or a row annotation?}
  \item{border}{Wether draw borders of the annotation region?}
  \item{gp}{Graphic parameters for lines. The length of each graphic parameter can be 1, or number of columns of \code{x} is \code{x} is a matrix.}
  \item{add_points}{Whether to add points on the lines?}
  \item{smooth}{If it is \code{TRUE}, smoothing by \code{\link[stats]{loess}} is performed. If it is \code{TRUE}, \code{add_points} is set to \code{TRUE} by default.}
  \item{pch}{Point type. The length setting is the same as \code{gp}.}
  \item{size}{Point size, the value should be a \code{\link[grid]{unit}} object. The length setting is the same as \code{gp}.}
  \item{pt_gp}{Graphic parameters for points. The length setting is the same as \code{gp}.}
  \item{ylim}{Data ranges. By default it is \code{range(x)}.}
  \item{extend}{The extension to both side of \code{ylim}. The value is a percent value corresponding to \code{ylim[2] - ylim[1]}.}
  \item{axis}{Whether to add axis?}
  \item{axis_param}{parameters for controlling axis. See \code{\link{default_axis_param}} for all possible settings and default parameters.}
  \item{width}{Width of the annotation. The value should be an absolute unit. Width is not allowed to be set for column annotation.}
  \item{height}{Height of the annotation. The value should be an absolute unit. Height is not allowed to be set for row annotation.}

}
\value{
An annotation function which can be used in \code{\link{HeatmapAnnotation}}.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#lines-annotation}
}
\examples{
anno = anno_lines(runif(10))
draw(anno, test = "anno_lines")
anno = anno_lines(cbind(c(1:5, 1:5), c(5:1, 5:1)), gp = gpar(col = 2:3))
draw(anno, test = "matrix")
anno = anno_lines(cbind(c(1:5, 1:5), c(5:1, 5:1)), gp = gpar(col = 2:3),
	add_points = TRUE, pt_gp = gpar(col = 5:6), pch = c(1, 16))
draw(anno, test = "matrix")
}
