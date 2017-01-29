\name{decorate_title}
\alias{decorate_title}
\title{
Decorate the heatmap title
}
\description{
Decorate the heatmap title
}
\usage{
decorate_title(heatmap, code, slice = 1, which = c("column", "row"),
    envir = new.env(parent = parent.frame()))
}
\arguments{

  \item{heatmap}{name of the heatmap}
  \item{code}{code that adds graphics in the selected heatmap body}
  \item{slice}{index of row slices in the heatmap}
  \item{which}{on rows or on columns?}
  \item{envir}{where to look for variables inside \code{code}}

}
\details{
There is a viewport for row titles and column title in the heatmap.
This function contructs the name of the viewport,
goes to the viewport by \code{\link[grid]{seekViewport}} and applies code
to that viewport.
}
\value{
The function returns no value.
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
