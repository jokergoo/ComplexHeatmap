\name{compare_pheatmap}
\alias{compare_pheatmap}
\title{
Compare heatmaps between pheatmap::pheatmap() and ComplexHeatmap::pheatmap()
}
\description{
Compare heatmaps between pheatmap::pheatmap() and ComplexHeatmap::pheatmap()
}
\usage{
compare_pheatmap(...)
}
\arguments{

  \item{...}{The same set of rguments passed to \code{\link[pheatmap]{pheatmap}} and \code{\link{pheatmap}}.}

}
\details{
The function plots two heatmaps, one by \code{\link[pheatmap]{pheatmap}} and one by \code{\link{pheatmap}}.
Users can see the difference between the two implementations.
}
\examples{
mat = matrix(rnorm(100), 10)
compare_pheatmap(mat)
}
