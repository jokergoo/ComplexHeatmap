\name{anno_histogram}
\alias{anno_histogram}
\title{
Histogram Annotation
}
\description{
Histogram Annotation
}
\usage{
anno_histogram(x, which = c("column", "row"), n_breaks = 11,
    border = FALSE, gp = gpar(fill = "#CCCCCC"),
    axis = TRUE, axis_param = default_axis_param(which),
    width = NULL, height = NULL)
}
\arguments{

  \item{x}{A matrix or a list. If \code{x} is a matrix and if \code{which} is \code{column}, statistics for boxplots are calculated by columns, if \code{which} is \code{row}, the calculation is done by rows.}
  \item{which}{Whether it is a column annotation or a row annotation?}
  \item{n_breaks}{Number of breaks for calculating histogram.}
  \item{border}{Wether draw borders of the annotation region?}
  \item{gp}{Graphic parameters for the boxes. The length of the graphic parameters should be one or the number of observations.}
  \item{axis}{Whether to add axis?}
  \item{axis_param}{parameters for controlling axis. See \code{\link{default_axis_param}} for all possible settings and default parameters.}
  \item{width}{Width of the annotation. The value should be an absolute unit. Width is not allowed to be set for column annotation.}
  \item{height}{Height of the annotation. The value should be an absolute unit. Height is not allowed to be set for row annotation.}

}
\value{
An annotation function which can be used in \code{\link{HeatmapAnnotation}}.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#histogram-annotation}
}
\examples{
m = matrix(rnorm(1000), nc = 10)
anno = anno_histogram(t(m), which = "row")
draw(anno, test = "row histogram")
anno = anno_histogram(t(m), which = "row", gp = gpar(fill = 1:10))
draw(anno, test = "row histogram with color")
anno = anno_histogram(t(m), which = "row", n_breaks = 20)
draw(anno, test = "row histogram with color")
}
