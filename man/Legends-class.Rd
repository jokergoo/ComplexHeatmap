\name{Legends-class}
\docType{class}
\alias{Legends-class}
\title{
The Class for Legends
}
\description{
The Class for Legends
}
\details{
This is a very simple class for legends that it only has one slot which is the real \code{\link[grid:grid.grob]{grob}} of the legends.

Construct a single legend by \code{\link{Legend}} and a group of legends by \code{\link{packLegend}}.
}
\examples{
lgd = Legend(at = 1:4)
lgd
lgd@grob
}
