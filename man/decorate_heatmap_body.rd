\name{decorate_heatmap_body}
\alias{decorate_heatmap_body}
\title{
Decorate the heatmap body
}
\description{
Decorate the heatmap body
}
\usage{
decorate_heatmap_body(heatmap, code, slice = 1)
}
\arguments{

  \item{heatmap}{name of the heatmap which is set as \code{name} option in \code{\link{Heatmap}} function}
  \item{code}{code that adds graphics in the selected heatmap body}
  \item{slice}{index of row slices in the heatmap if it is split by rows}

}
\details{
There is a viewport for each row slice in each heatmap.
This function contructs the name of the viewport,
goes to the viewport by \code{\link[grid]{seekViewport}} and applies code
to that viewport.

If you know the number of rows and columns for that row slice, it is
simple to calculate the position of every small grid in the row slice.
E.g., the position for the grid in i^th row and j^th column is:

  \preformatted{
    # assume nc is the number of columns 
    # and nr is the number of rows in that row slice
    unit((i-0.5)/nc, "npc")
    unit((j-0.5)/nr, "npc")

    # the width is
    unit(1/nc, "npc")

    # the height is
    unit(1/nr, "npc")  }
}
\value{
This function returns no value.
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
