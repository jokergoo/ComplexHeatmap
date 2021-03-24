\name{bar3D}
\alias{bar3D}
\title{
Draw 3D bars
}
\description{
Draw 3D bars
}
\usage{
bar3D(x, y, w, h, l, theta = 60, default.units = "npc", fill = "white", col = "black")
}
\arguments{

  \item{x}{x coordinate of the center point in the bottom face.}
  \item{y}{y coordinate of the center point in the bottom face.}
  \item{w}{Width of the botton face.}
  \item{h}{Height of the botton face.}
  \item{l}{Length of the bars (in the z-direction).}
  \item{theta}{The angle for the projection.}
  \item{default.units}{Units.}
  \item{fill}{Filled colors for the bars.}
  \item{col}{Border colors.}

}
\examples{
grid.newpage()
bar3D(c(0.3, 0.7), 0.5, 0.2, 0.2, 0.2, fill = 2:3)
}
