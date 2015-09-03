\name{grid.dendrogram}
\alias{grid.dendrogram}
\title{
Draw dendrogram under grid system
}
\description{
Draw dendrogram under grid system
}
\usage{
grid.dendrogram(dend, facing = c("bottom", "top", "left", "right"),
    max_height = NULL, order = c("normal", "reverse"), ...)
}
\arguments{

  \item{dend}{a \code{\link[stats]{dendrogram}} object.}
  \item{facing}{facing of the dendrogram.}
  \item{max_height}{maximum height of the dendrogram. It is useful to make dendrograms comparableif you want to plot more than one dendrograms.}
  \item{order}{should leaves of dendrogram be put in the normal order (1, ..., n) or reverse order (n, ..., 1)?}
  \item{...}{pass to \code{\link[grid]{viewport}} which contains the dendrogram.}

}
\details{
The dendrogram can be renderred (e.g. by \code{dendextend} package).

A viewport is created which contains the dendrogram.

This function only plots the dendrogram without adding labels. The leaves of the dendrogram
locates at \code{unit(c(0.5, 1.5, ...(n-0.5))/n, "npc")}.
}
\value{
No value is returned.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
hc = hclust(dist(USArrests[1:5, ]))
dend = as.dendrogram(hc)

grid.newpage()
layout = grid.layout(nrow = 2, ncol = 2)
pushViewport(viewport(layout = layout))
grid.dendrogram(dend, layout.pos.row = 1, layout.pos.col = 1)
grid.dendrogram(dend, facing = "top", layout.pos.row = 1, layout.pos.col = 2)
grid.dendrogram(dend, facing = "top", order = "reverse", layout.pos.row = 2, 
    layout.pos.col = 1)
grid.dendrogram(dend, facing = "left", layout.pos.row = 2, layout.pos.col = 2)
upViewport()

}
