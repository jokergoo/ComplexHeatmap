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

  \item{...}{The same set of arguments passed to \code{pheatmap::pheatmap} and \code{ComplexHeatmap::pheatmap}.}

}
\details{
The function plots two heatmaps, one by \code{pheatmap::pheatmap} and one by \code{ComplexHeatmap::pheatmap}.
Users can see the difference between the two implementations.
}
\examples{
mat = matrix(rnorm(100), 10)
compare_pheatmap(mat)
}
