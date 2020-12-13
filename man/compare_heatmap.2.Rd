\name{compare_heatmap.2}
\alias{compare_heatmap.2}
\title{
Compare heatmaps between gplots::heatmap.2() and ComplexHeatmap::heatmap()
}
\description{
Compare heatmaps between gplots::heatmap.2() and ComplexHeatmap::heatmap()
}
\usage{
compare_heatmap.2(...)
}
\arguments{

  \item{...}{The same set of arguments passed to \code{gplots::heatmap.2} and \code{ComplexHeatmap::heatmap.2}.}

}
\details{
The function plots two heatmaps, one by \code{gplots::heatmap.2} and one by \code{ComplexHeatmap::heatmap.2}.
Users can see the difference between the two implementations.
}
\examples{
mat = matrix(rnorm(100), 10)
compare_heatmap.2(mat)
}
