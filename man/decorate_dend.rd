\name{decorate_dend}
\alias{decorate_dend}
\title{
Decorate Heatmap Dendrograms
}
\description{
Decorate Heatmap Dendrograms
}
\usage{
decorate_dend(heatmap, code, slice = 1, which = c("column", "row"),
    envir = new.env(parent = parent.frame()))
}
\arguments{

  \item{heatmap}{Name of the heatmap.}
  \item{code}{Code that adds graphics in the selected heatmap dendrogram.}
  \item{slice}{Index of the row slice or column slice in the heatmap.}
  \item{which}{Is the dendrogram on rows or on columns?}
  \item{envir}{Where to look for variables inside \code{code}.}

}
\details{
If you know the number of leaves in the dendrogram, it is
simple to calculate the position of every leave in the dendrogram.
E.g., for the column dendrogram, the i^th leave is located at:

  \preformatted{
    # assume nc is the number of columns in the column slice
    unit((i-0.5)/nc, "npc")  }
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
Heatmap(matrix(rnorm(100), 10), name = "mat", km = 2)
decorate_dend("mat", {
    grid.rect(gp = gpar(fill = "#FF000080"))
}, which = "row", slice = 2)
}
