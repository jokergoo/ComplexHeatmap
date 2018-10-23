\name{anno_horizon}
\alias{anno_horizon}
\title{
Horizon chart Annotation
}
\description{
Horizon chart Annotation
}
\usage{
anno_horizon(x, which = c("column", "row"),
    gp = gpar(pos_fill = "#D73027", neg_fill = "#313695"),
    n_slice = 4, slice_size = NULL, negative_from_top = FALSE,
    normalize = TRUE, gap = unit(0, "mm"),
    axis = TRUE, axis_param = default_axis_param(which),
    width = NULL, height = NULL)
}
\arguments{

  \item{x}{A matrix or a list. If \code{x} is a matrix or a data frame, columns correspond to observations.}
  \item{which}{Whether it is a column annotation or a row annotation?}
  \item{gp}{Graphic parameters for the boxes. The length of the graphic parameters should be one or the number of observations. There are two unstandard parameters specificly for horizon chart: \code{pos_fill} and \code{neg_fill} controls the filled color for positive values and negative values.}
  \item{n_slice}{Number of slices on y-axis.}
  \item{slice_size}{Height of the slice. If the value is not \code{NULL}, \code{n_slice} will be recalculated. }
  \item{negative_from_top}{Whether the areas for negative values start from the top or the bottom of the plotting region?}
  \item{normalize}{Whether normalize \code{x} by max(abs(x)).}
  \item{gap}{Gap size of neighbouring horizon chart.}
  \item{axis}{Whether to add axis?}
  \item{axis_param}{parameters for controlling axis. See \code{\link{default_axis_param}} for all possible settings and default parameters.}
  \item{width}{Width of the annotation. The value should be an absolute unit. Width is not allowed to be set for column annotation.}
  \item{height}{Height of the annotation. The value should be an absolute unit. Height is not allowed to be set for row annotation.}

}
\details{
Horizon chart as row annotation is only supported.
}
\value{
An annotation function which can be used in \code{\link{HeatmapAnnotation}}.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#horizon-chart-annotation}
}
\examples{
lt = lapply(1:20, function(x) cumprod(1 + runif(1000, -x/100, x/100)) - 1)
anno = anno_horizon(lt, which = "row")
draw(anno, test = "horizon chart")
anno = anno_horizon(lt, which = "row", 
    gp = gpar(pos_fill = "orange", neg_fill = "darkgreen"))
draw(anno, test = "horizon chart, col")
anno = anno_horizon(lt, which = "row", negative_from_top = TRUE)
draw(anno, test = "horizon chart + negative_from_top")
anno = anno_horizon(lt, which = "row", gap = unit(1, "mm"))
draw(anno, test = "horizon chart + gap")
anno = anno_horizon(lt, which = "row", 
    gp = gpar(pos_fill = rep(c("orange", "red"), each = 10),
    neg_fill = rep(c("darkgreen", "blue"), each = 10)))
draw(anno, test = "horizon chart, col")
}
