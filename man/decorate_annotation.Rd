\name{decorate_annotation}
\alias{decorate_annotation}
\title{
Decorate Heatmap Annotation
}
\description{
Decorate Heatmap Annotation
}
\usage{
decorate_annotation(annotation, code, slice = 1, envir = new.env(parent = parent.frame()))
}
\arguments{

  \item{annotation}{Name of the annotation.}
  \item{code}{Code that adds graphics in the selected heatmap annotation.}
  \item{slice}{Index of the row slices or the column slice in the heatmap.}
  \item{envir}{Where to look for variables inside \code{code}.}

}
\details{
There is a viewport for every column annotation and row annotation.
This function contructs the name of the viewport,
goes to the viewport by \code{\link[grid:viewports]{seekViewport}}, runs code
to that viewport, and finally goes back to the original viewport.
}
\value{
The function returns no value.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-decoration.html}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
set.seed(123)
ha1 = HeatmapAnnotation(df = data.frame(type = rep(letters[1:2], 5)))
ha2 = rowAnnotation(point = anno_points(runif(10), which = "row"))
Heatmap(matrix(rnorm(100), 10), name = "mat", km = 2,
    top_annotation = ha1) + ha2
decorate_annotation("type", {
    grid.circle(x = unit(c(0.2, 0.4, 0.6, 0.8), "npc"), 
        gp = gpar(fill = "#FF000080"))
})
decorate_annotation("point", {
    grid.rect(gp = gpar(fill = "#FF000080"))
}, slice = 2)
}
