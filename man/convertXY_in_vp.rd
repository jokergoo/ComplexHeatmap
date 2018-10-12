\name{convertXY_in_vp}
\alias{convertXY_in_vp}
\title{
Convert XY in a parent viewport
}
\description{
Convert XY in a parent viewport
}
\usage{
convertXY_in_vp(u, vp_name = "ROOT")
}
\arguments{

  \item{u}{A list of two units which is x and y}
  \item{vp_name}{the name of the parent viewport}

}
\examples{
grid.newpage()
pushViewport(viewport(x = 0.5, y = 0.5, width = 0.5, height = 0.5, just = c("left", "bottom")))
u = list(x = unit(2, "cm"), y = unit(2, "cm"))
convertXY_in_vp(u)
}
