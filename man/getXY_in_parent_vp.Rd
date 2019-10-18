\name{getXY_in_parent_vp}
\alias{getXY_in_parent_vp}
\title{
Convert XY in a Parent Viewport
}
\description{
Convert XY in a Parent Viewport
}
\usage{
getXY_in_parent_vp(u, vp_name = "ROOT")
}
\arguments{

  \item{u}{A list of two units which correspond to x and y.}
  \item{vp_name}{The name of the parent viewport.}

}
\details{
It converts a coordinate measured in current viewport to the coordinate in a parent viewport.

In the conversion, all units are recalculated as absolute units, so if you change the size
of the interactive graphic window, you need to rerun the function.
}
\value{
A list of two units.
}
\examples{
grid.newpage()
pushViewport(viewport(x = 0.5, y = 0.5, width = 0.5, height = 0.5, just = c("left", "bottom")))
grid.rect()
grid.points(x = unit(2, "cm"), y = unit(2, "cm"), pch = 1)
u = list(x = unit(2, "cm"), y = unit(2, "cm"))
u2 = getXY_in_parent_vp(u)
popViewport()
grid.rect(gp = gpar(col = "red"))
grid.points(x = u2$x, u2$y, pch = 2)
}
