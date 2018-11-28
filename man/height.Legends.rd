\name{height.Legends}
\alias{height.Legends}
\title{
Height of the Legends
}
\description{
Height of the Legends
}
\usage{
\method{height}{Legends}(x, ...)
}
\arguments{

  \item{x}{The \code{\link[grid:grid.grob]{grob}} object returned by \code{\link{Legend}} or \code{\link{packLegend}}.}
  \item{...}{Other arguments.}

}
\value{
The returned unit x is always in \code{mm}.
}
\examples{
lgd = Legend(labels = 1:10, title = "foo", legend_gp = gpar(fill = "red"))
ComplexHeatmap:::height(lgd)
}
