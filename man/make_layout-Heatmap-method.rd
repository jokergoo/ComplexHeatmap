\name{make_layout-Heatmap-method}
\alias{make_layout,Heatmap-method}
\title{
Make the layout of a single heatmap
}
\description{
Make the layout of a single heatmap
}
\usage{
\S4method{make_layout}{Heatmap}(object)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object.}

}
\details{
The layout of the single heatmap will be established by setting the size of each heatmap components.
Also functions that make graphics for heatmap components will be recorded.

Whether apply row clustering or column clustering affects the layout, so clustering should be applied 
first before making the layout.

This function is only for internal use.
}
\value{
A \code{\link{Heatmap-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# no example for this internal method
NULL

}
