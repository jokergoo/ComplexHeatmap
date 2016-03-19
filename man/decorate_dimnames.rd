\name{decorate_dimnames}
\alias{decorate_dimnames}
\title{
Decorate the heatmap dimension names
}
\description{
Decorate the heatmap dimension names
}
\usage{
decorate_dimnames(heatmap, code, slice = 1, which = c("column", "row"))
}
\arguments{

  \item{heatmap}{name of the heatmap}
  \item{code}{code that adds graphics in the selected heatmap body}
  \item{slice}{index of row slices in the heatmap}
  \item{which}{on rows or on columns?}

}
\details{
There is a viewport for row names and column names in the heatmap.
This function contructs the name of the viewport,
goes to the viewport by \code{\link[grid]{seekViewport}} and applies code
to that viewport.

If you know the dimensions of the matrix, it is
simple to calculate the position of every row name or column name in the heatmap.
E.g., for the column column, the i^th name is located at:

  \preformatted{
    # assume nc is the number of columns 
    unit((i-0.5)/nc, "npc")  }
}
\value{
The function returns no value.
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
