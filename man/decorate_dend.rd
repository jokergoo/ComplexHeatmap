\name{decorate_dend}
\alias{decorate_dend}
\title{
Decorate the heatmap dendrogram
}
\description{
Decorate the heatmap dendrogram
}
\usage{
decorate_dend(heatmap, code, slice = 1, which = c("column", "row"))
}
\arguments{

  \item{heatmap}{name of the heatmap}
  \item{code}{code that adds graphics in the selected heatmap body}
  \item{slice}{index of row slices in the heatmap}
  \item{which}{on rows or on columns?}

}
\details{
There is a viewport for each dendrogram in the heatmap.
This function contructs the name of the viewport,
goes to the viewport by \code{\link[grid]{seekViewport}} and applies code
to that viewport.

If you know the number of leaves in the dendrogram, it is
simple to calculate the position of every leave in the dendrogram.
E.g., for the column dendrogram, the i^th leave is located at:

  \preformatted{
    # assume nc is the number of columns 
    unit((i-0.5)/nc, "npc")  }
}
\value{
This function returns no value.
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
