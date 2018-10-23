\name{grid.boxplot}
\alias{grid.boxplot}
\title{
Draw a Single Boxplot
}
\description{
Draw a Single Boxplot
}
\usage{
grid.boxplot(value, pos, outline = TRUE, box_width = 0.6,
    pch = 1, size = unit(2, "mm"), gp = gpar(fill = "#CCCCCC"),
    direction = c("vertical", "horizontal"))
}
\arguments{

  \item{value}{A vector of numeric values.}
  \item{pos}{Position of the boxplot.}
  \item{outline}{Whether draw outlines?}
  \item{box_width}{width of the box.}
  \item{pch}{Point type.}
  \item{size}{Point size.}
  \item{gp}{Graphic parameters.}
  \item{direction}{Whether the box is vertical or horizontal.}

}
\details{
All the values are measured with \code{native} coordinate.
}
\examples{
lt = list(rnorm(100), rnorm(100))
grid.newpage()
pushViewport(viewport(xscale = c(0.5, 2.5), yscale = range(lt)))
grid.boxplot(lt[[1]], pos = 1, gp = gpar(fill = "red"))
grid.boxplot(lt[[2]], pos = 2, gp = gpar(fill = "green"))
popViewport()
}
