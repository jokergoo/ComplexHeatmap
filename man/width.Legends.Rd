\name{width.Legends}
\alias{width.Legends}
\title{
Width of the Legends
}
\description{
Width of the Legends
}
\usage{
\method{width}{Legends}(x, ...)
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
ComplexHeatmap:::width(lgd)
}
