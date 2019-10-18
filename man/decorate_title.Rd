\name{decorate_title}
\alias{decorate_title}
\title{
Decorate Heatmap Titles
}
\description{
Decorate Heatmap Titles
}
\usage{
decorate_title(heatmap, code, slice = 1, which = c("column", "row"),
    envir = new.env(parent = parent.frame()))
}
\arguments{

  \item{heatmap}{Name of the heatmap.}
  \item{code}{Code that adds graphics in the selected viewport.}
  \item{slice}{Index of the row slice or column slice in the heatmap.}
  \item{which}{Is it a row title or a column title?}
  \item{envir}{Where to look for variables inside \code{code}.}

}
\details{
There is a viewport for row titles and column title in the heatmap.
This function contructs the name of the viewport,
goes to the viewport by \code{\link[grid:viewports]{seekViewport}} , runs code
to that viewport and finally goes back to the original viewport.
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
Heatmap(matrix(rnorm(100), 10), name = "mat", km = 2)
decorate_title("mat", {
    grid.rect(gp = gpar(fill = "#FF000080"))
}, which = "row", slice = 2)
}
