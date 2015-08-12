\name{decorate_heatmap_body}
\alias{decorate_heatmap_body}
\title{
Decorate the heatmap body
}
\description{
Decorate the heatmap body
}
\usage{
decorate_heatmap_body(heatmap, code = {}, slice = 1)
}
\arguments{

  \item{heatmap}{name of the heatmap}
  \item{code}{code that is executed in the heatmap body}
  \item{slice}{index of row slices in the heatmap}

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
Heatmap(matrix(rnorm(100), 10), name = "mat")
decorate_heatmap_body("mat", {
    grid.circle(gp = gpar(fill = "#FF000080"))
})
}
