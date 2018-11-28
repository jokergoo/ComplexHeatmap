\name{decorate_heatmap_body}
\alias{decorate_heatmap_body}
\title{
Decorate Heatmap Bodies
}
\description{
Decorate Heatmap Bodies
}
\usage{
decorate_heatmap_body(heatmap, code,
    slice = 1, row_slice = slice, column_slice = 1,
    envir = new.env(parent = parent.frame()))
}
\arguments{

  \item{heatmap}{Name of the heatmap which is set as \code{name} argument in \code{\link{Heatmap}} function.}
  \item{code}{Code that adds graphics in the selected heatmap body.}
  \item{slice}{Index of the row slice in the heatmap.}
  \item{row_slice}{Index of the row slice in the heatmap.}
  \item{column_slice}{Index of the column slice in the heatmap.}
  \item{envir}{Where to look for variables inside \code{code}.}

}
\details{
There is a viewport for each slice in each heatmap.
This function contructs the name of the viewport,
goes to the viewport by \code{\link[grid:viewports]{seekViewport}}, runs the code
to that viewport and finally goes back to the original viewport.
}
\value{
This function returns no value.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-decoration.html}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
set.seed(123)
Heatmap(matrix(rnorm(100), 10), name = "mat")
decorate_heatmap_body("mat", {
    grid.circle(gp = gpar(fill = "#FF000080"))
})
}
