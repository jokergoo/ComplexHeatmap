\name{anno_boxplot}
\alias{anno_boxplot}
\title{
Boxplot Annotation
}
\description{
Boxplot Annotation
}
\usage{
anno_boxplot(x, which = c("column", "row"), border = TRUE,
    gp = gpar(fill = "#CCCCCC"), ylim = NULL, extend = 0.05, outline = TRUE, box_width = 0.6,
    add_points = FALSE, pch = 16, size = unit(4, "pt"), pt_gp = gpar(), axis = TRUE,
    axis_param = default_axis_param(which), width = NULL, height = NULL, ...)
}
\arguments{

  \item{x}{A matrix or a list. If \code{x} is a matrix and if \code{which} is \code{column}, statistics for boxplots are calculated by columns, if \code{which} is \code{row}, the calculation is done by rows.}
  \item{which}{Whether it is a column annotation or a row annotation?}
  \item{border}{Wether draw borders of the annotation region?}
  \item{gp}{Graphic parameters for the boxes. The length of the graphic parameters should be one or the number of observations.}
  \item{ylim}{Data ranges.}
  \item{extend}{The extension to both side of \code{ylim}. The value is a percent value corresponding to \code{ylim[2] - ylim[1]}.}
  \item{outline}{Whether draw outline of boxplots?}
  \item{box_width}{Relative width of boxes. The value should be smaller than one.}
  \item{add_points}{Whether add points on top of the boxes?}
  \item{pch}{Point style.}
  \item{size}{Point size.}
  \item{pt_gp}{Graphics parameters for points.}
  \item{axis}{Whether to add axis?}
  \item{axis_param}{parameters for controlling axis. See \code{\link{default_axis_param}} for all possible settings and default parameters.}
  \item{width}{Width of the annotation. The value should be an absolute unit. Width is not allowed to be set for column annotation.}
  \item{height}{Height of the annotation. The value should be an absolute unit. Height is not allowed to be set for row annotation.}
  \item{...}{Other arguments.}

}
\value{
An annotation function which can be used in \code{\link{HeatmapAnnotation}}.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#box-annotation}
}
\examples{
set.seed(123)
m = matrix(rnorm(100), 10)
anno = anno_boxplot(m, height = unit(4, "cm"))
draw(anno, test = "anno_boxplot")
anno = anno_boxplot(m, height = unit(4, "cm"), gp = gpar(fill = 1:10))
draw(anno, test = "anno_boxplot with gp")
}
