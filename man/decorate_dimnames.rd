\name{decorate_dimnames}
\alias{decorate_dimnames}
\title{
Decorate the heatmap dimension names
}
\description{
Decorate the heatmap dimension names
}
\usage{
decorate_dimnames(heatmap, code, slice = 1, which = c("column", "row"))}
\arguments{

  \item{heatmap}{name of the heatmap}
  \item{code}{code that is executed in the heatmap body}
  \item{slice}{index of row slices in the heatmap}
  \item{which}{on rows or on columns?}
}
\details{
This simple function actually contructs the name of the viewport,
goes to the viewport by \code{\link[grid]{seekViewport}} and applies code
to that viewport.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
set.seed(123)
mat = matrix(rnorm(100), 10)
rownames(mat) = letters[1:10]
colnames(mat) = LETTERS[1:10]
Heatmap(mat, name = "mat", km = 2)
decorate_dimnames("mat", {
    grid.rect(gp = gpar(fill = "#FF000080"))
}, which = "row", slice = 2)
}
