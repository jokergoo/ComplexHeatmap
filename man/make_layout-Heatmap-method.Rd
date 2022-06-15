\name{make_layout-Heatmap-method}
\alias{make_layout,Heatmap-method}
\title{
Make the Layout of a Single Heatmap
}
\description{
Make the Layout of a Single Heatmap
}
\usage{
\S4method{make_layout}{Heatmap}(object)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}

}
\details{
The layout of the single heatmap will be established by setting the size of each heatmap component.
Also how to make graphics for heatmap components will be recorded by saving as functions.

Whether to apply row clustering or column clustering affects the layout, so clustering should be applied 
first by \code{\link{prepare,Heatmap-method}} before making the layout.

This function is only for internal use.
}
\value{
A \code{\link{Heatmap-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
