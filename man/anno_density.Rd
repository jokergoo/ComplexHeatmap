\name{anno_density}
\alias{anno_density}
\title{
Density Annotation
}
\description{
Density Annotation
}
\usage{
anno_density(x, which = c("column", "row"),
    type = c("lines", "violin", "heatmap"), xlim = NULL, max_density = NULL,
    heatmap_colors = rev(brewer.pal(name = "RdYlBu", n = 11)),
    joyplot_scale = 1, border = TRUE, gp = gpar(fill = "#CCCCCC"),
    axis = TRUE, axis_param = default_axis_param(which),
    width = NULL, height = NULL)
}
\arguments{

  \item{x}{A matrix or a list. If \code{x} is a matrix and if \code{which} is \code{column}, statistics for boxplots are calculated by columns, if \code{which} is \code{row}, the calculation is done by rows.}
  \item{which}{Whether it is a column annotation or a row annotation?}
  \item{type}{Type of graphics to represent density distribution. "lines" for normal density plot; "violine" for violin plot and "heatmap" for heatmap visualization of density distribution.}
  \item{xlim}{Range on x-axis.}
  \item{max_density}{Maximal density values in the plot. Normally you don't need to manually set it, but when you have multiple density annotations and you want to compare between them, you should manually set this argument to make density distributions are in a same scale.}
  \item{heatmap_colors}{A vector of colors for interpolating density values.}
  \item{joyplot_scale}{Relative height of density distribution. A value higher than 1 increases the height of the density distribution and the plot will represented as so-called "joyplot".}
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
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#density-annotation}
}
\examples{
m = matrix(rnorm(100), 10)
anno = anno_density(m, which = "row")
draw(anno, test = "normal density")
anno = anno_density(m, which = "row", type = "violin")
draw(anno, test = "violin")
anno = anno_density(m, which = "row", type = "heatmap")
draw(anno, test = "heatmap")
anno = anno_density(m, which = "row", type = "heatmap", 
    heatmap_colors = c("white", "orange"))
draw(anno, test = "heatmap, colors")
}
