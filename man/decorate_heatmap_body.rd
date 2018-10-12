\name{decorate_heatmap_body}
\alias{decorate_heatmap_body}
\title{
Decorate the heatmap body
}
\description{
Decorate the heatmap body
}
\usage{
decorate_heatmap_body(heatmap, code,
    slice = 1, row_slice = slice, column_slice = 1,
    envir = new.env(parent = parent.frame()))
}
\arguments{

  \item{heatmap}{name of the heatmap which is set as \code{name} argument in \code{\link{Heatmap}} function.}
  \item{code}{code that adds graphics in the selected heatmap body.}
  \item{slice}{index of row slices in the heatmap.}
  \item{row_slice}{index of row slices in the heatmap.}
  \item{column_slice}{index of column slices in the heatmap.}
  \item{envir}{where to look for variables inside \code{code}}

}
\details{
There is a viewport for each row slice and each column slice in each heatmap.
This function contructs the name of the viewport,
goes to the viewport by \code{\link[grid]{seekViewport}} and applies code
to that viewport.
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
