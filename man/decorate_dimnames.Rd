\name{decorate_dimnames}
\alias{decorate_dimnames}
\title{
Decorate Heatmap Dimension Names
}
\description{
Decorate Heatmap Dimension Names
}
\usage{
decorate_dimnames(heatmap, code, slice = 1, which = c("column", "row"),
    envir = new.env(parent = parent.frame()))
}
\arguments{

  \item{heatmap}{Name of the heatmap.}
  \item{code}{Code that adds graphics in the selected viewport.}
  \item{slice}{Index of the row slice or column slice in the heatmap.}
  \item{which}{on rows or on columns?}
  \item{envir}{where to look for variables inside \code{code}.}

}
\details{
If you know the dimensions of the matrix, it is
simple to calculate the position of every row name or column name in the heatmap.
E.g., for the column column, the i^th name is located at:

  \preformatted{
    # assume nc is the number of columns in the column slice
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
