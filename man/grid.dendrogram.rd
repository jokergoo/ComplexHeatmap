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
  \item{max_height}{maximum height of the dendrogram. It is useful if you want to plot more than one dendrograms.}
  \item{order}{should leaves of dendrogram be put in the normal order or reverse order?}
  \item{...}{pass to \code{\link[grid]{viewport}} that contains the dendrogram.}

}
\details{
The dendrogram tree can be renderred (e.g. by \code{dendextend} package).  

A viewport is created which contains the dendrogram.  


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
