\name{anno_zoom}
\alias{anno_zoom}
\title{
Zoom annotation
}
\description{
Zoom annotation
}
\usage{
anno_zoom(align_to, panel_fun = function(index, nm = NULL) { grid.rect() },
    which = c("column", "row"), side = ifelse(which == "column", "top", "right"),
    size = NULL, gap = unit(1, "mm"),
    link_width = unit(5, "mm"), link_height = link_width, link_gp = gpar(),
    extend = unit(0, "mm"), width = NULL, height = NULL)
}
\arguments{

  \item{align_to}{-align_to}
  \item{panel_fun}{-panel_fun}
  \item{which}{Whether it is a column annotation or a row annotation?}
  \item{side}{Side of the boxes If it is a column annotation, valid values are "top" and "bottom"; If it is a row annotation, valid values are "left" and "right".}
  \item{size}{-size}
  \item{gap}{-gap}
  \item{link_gp}{Graphic settings for the segments.}
  \item{link_width}{Width of the segments.}
  \item{link_height}{Similar as \code{link_width}, used for column annotation.}
  \item{extend}{By default, the region for the labels has the same width (if it is a column annotation) or same height (if it is a row annotation) as the heatmap. The size can be extended by this options. The value can be a proportion number or  a \code{\link[grid]{unit}} object. The length can be either one or two.}
  \item{width}{Width of the annotation. The value should be an absolute unit. Width is not allowed to be set for column annotation.}
  \item{height}{Height of the annotation. The value should be an absolute unit. Height is not allowed to be set for row annotation.}

}
\details{

}
\examples{
# There is no example
NULL

}
