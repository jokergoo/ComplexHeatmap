\name{compare_heatmap}
\alias{compare_heatmap}
\title{
Compare heatmaps between stats::heatmap() and ComplexHeatmap::heatmap()
}
\description{
Compare heatmaps between stats::heatmap() and ComplexHeatmap::heatmap()
}
\usage{
compare_heatmap(...)
}
\arguments{

  \item{...}{The same set of arguments passed to \code{stats::heatmap} and \code{ComplexHeatmap::heatmap}.}

}
\details{
The function plots two heatmaps, one by \code{stats::heatmap} and one by \code{ComplexHeatmap::heatmap}.
Users can see the difference between the two implementations.
}
\examples{
mat = matrix(rnorm(100), 10)
compare_heatmap(mat)
}
