\name{anno_text}
\alias{anno_text}
\title{
Text Annotation
}
\description{
Text Annotation
}
\usage{
anno_text(x, which = c("column", "row"), gp = gpar(),
    rot = guess_rot(), just = guess_just(),
    offset = guess_location(), location = guess_location(),
    width = NULL, height = NULL, show_name = FALSE)
}
\arguments{

  \item{x}{A vector of text.}
  \item{which}{Whether it is a column annotation or a row annotation?}
  \item{gp}{Graphic parameters.}
  \item{rot}{Rotation of the text, pass to \code{\link[grid]{grid.text}}.}
  \item{just}{Justification of text, pass to \code{\link[grid]{grid.text}}.}
  \item{offset}{Depracated, use \code{location} instead.}
  \item{location}{Position of the text. By default \code{rot}, \code{just} and \code{location} are automatically inferred according to whether it is a row annotation or column annotation. The value of \code{location} should be a \code{\link[grid]{unit}} object, normally in \code{npc} unit. E.g. \code{unit(0, 'npc')} means the most left of the annotation region and \code{unit(1, 'npc')} means the most right of the annotation region. }
  \item{width}{Width of the annotation. The value should be an absolute unit. Width is not allowed to be set for column annotation.}
  \item{height}{Height of the annotation. The value should be an absolute unit. Height is not allowed to be set for row annotation.}
  \item{show_name}{Whether to show the annotation name.}

}
\value{
An annotation function which can be used in \code{\link{HeatmapAnnotation}}.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#text-annotation}
}
\examples{
anno = anno_text(month.name)
draw(anno, test = "month names")
anno = anno_text(month.name, gp = gpar(fontsize = 16))
draw(anno, test = "month names with fontsize")
anno = anno_text(month.name, gp = gpar(fontsize = 1:12+4))
draw(anno, test = "month names with changing fontsize")
anno = anno_text(month.name, which = "row")
draw(anno, test = "month names on rows")
anno = anno_text(month.name, location = 0, rot = 45, 
    just = "left", gp = gpar(col = 1:12))
draw(anno, test = "with rotations")
anno = anno_text(month.name, location = 1, 
    rot = 45, just = "right", gp = gpar(fontsize = 1:12+4))
draw(anno, test = "with rotations")
}
