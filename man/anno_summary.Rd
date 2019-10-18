\name{anno_summary}
\alias{anno_summary}
\title{
Summary Annotation
}
\description{
Summary Annotation
}
\usage{
anno_summary(which = c("column", "row"), border = TRUE, bar_width = 0.8,
    axis = TRUE, axis_param = default_axis_param(which),
    ylim = NULL, extend = 0.05, outline = TRUE, box_width = 0.6,
    pch = 1, size = unit(2, "mm"), gp = gpar(),
    width = NULL, height = NULL)
}
\arguments{

  \item{which}{Whether it is a column annotation or a row annotation?}
  \item{border}{Wether draw borders of the annotation region?}
  \item{bar_width}{Relative width of the bars. The value should be smaller than one.}
  \item{axis}{Whether to add axis?}
  \item{axis_param}{parameters for controlling axis. See \code{\link{default_axis_param}} for all possible settings and default parameters.}
  \item{ylim}{Data ranges. \code{ylim} for barplot is enforced to be \code{c(0, 1)}.}
  \item{extend}{The extension to both side of \code{ylim}. The value is a percent value corresponding to \code{ylim[2] - ylim[1]}. This argument is only for boxplot.}
  \item{outline}{Whether draw outline of boxplots?}
  \item{box_width}{Relative width of boxes. The value should be smaller than one.}
  \item{pch}{Point style.}
  \item{size}{Point size.}
  \item{gp}{Graphic parameters.}
  \item{width}{Width of the annotation. The value should be an absolute unit. Width is not allowed to be set for column annotation.}
  \item{height}{Height of the annotation. The value should be an absolute unit. Height is not allowed to be set for row annotation.}

}
\details{
\code{anno_summary} is a special annotation function that it only works for one-column or one-row heatmap. 
It shows the summary of the values in the heatmap. If the values in the heatmap is discrete, 
the proportion of each level (the sum is normalized to 1) is visualized as stacked barplot. If the heatmap
is split into multiple slices, multiple bars are put in the annotation. If the value is continuous, boxplot is used.

In the barplot, the color schema is used as the same as the heatmap, while for the boxplot, the color needs
to be controlled by \code{gp}.
}
\value{
An annotation function which can be used in \code{\link{HeatmapAnnotation}}.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#summary-annotation}
}
\examples{
ha = HeatmapAnnotation(summary = anno_summary(height = unit(4, "cm")))
v = sample(letters[1:2], 50, replace = TRUE)
split = sample(letters[1:2], 50, replace = TRUE)
Heatmap(v, top_annotation = ha, width = unit(1, "cm"), split = split)

ha = HeatmapAnnotation(summary = anno_summary(gp = gpar(fill = 2:3), height = unit(4, "cm")))
v = rnorm(50)
Heatmap(v, top_annotation = ha, width = unit(1, "cm"), split = split)
}
