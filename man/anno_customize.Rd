\name{anno_customize}
\alias{anno_customize}
\title{
Customized annotation
}
\description{
Customized annotation
}
\usage{
anno_customize(x, graphics = list(), which = c("column", "row"),
    border = TRUE, width = NULL, height = NULL, verbose = TRUE)
}
\arguments{

  \item{x}{A categorical variable.}
  \item{graphics}{A list of functions that define graphics for each level in \code{x}.}
  \item{which}{Is it a row annotation or a column annotation?}
  \item{width}{Width of the annotation. The value should be an absolute unit. Width is not allowed to be set for column annotation.}
  \item{height}{Height of the annotation. The value should be an absolute unit. Height is not allowed to be set for row annotation.}
  \item{border}{Whether to draw border.}
  \item{verbose}{Whether to print messages.}

}
\details{
Functions in \code{graphics} define simple graphics drawn in each annotation cell. The function takes four arguments:

\describe{
  \item{x,y}{Center of the annotation cell.}
  \item{w,h}{Width and height of the annotation cell.}
}
}
\value{
An annotation function which can be used in \code{\link{HeatmapAnnotation}}.
}
\examples{
x = sort(sample(letters[1:3], 10, replace = TRUE))
graphics = list(
    "a" = function(x, y, w, h) grid.points(x, y, pch = 16),
    "b" = function(x, y, w, h) grid.rect(x, y, w*0.8, h*0.8, gp = gpar(fill = "red")),
    "c" = function(x, y, w, h) grid.segments(x - 0.5*w, y - 0.5*h, x + 0.5*w, y + 0.5*h, gp = gpar(lty = 2))
)

anno = anno_customize(x, graphics = graphics)

m = matrix(rnorm(100), 10)
Heatmap(m, top_annotation = HeatmapAnnotation(bar = x, foo = anno))

# Add legends for `foo`
ht = Heatmap(m, top_annotation = HeatmapAnnotation(bar = x, foo = anno))
lgd = Legend(title = "foo", at = names(graphics), graphics = graphics)
draw(ht, annotation_legend_list = list(lgd))
}
