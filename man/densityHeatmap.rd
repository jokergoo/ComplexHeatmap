\name{densityHeatmap}
\alias{densityHeatmap}
\title{
Use colors to represent density distribution
}
\description{
Use colors to represent density distribution
}
\usage{
densityHeatmap(data, col = rev(brewer.pal(11, "Spectral")),
    anno = NULL, ylab = deparse(substitute(data)),
    title = paste0("Density heatmap of ", deparse(substitute(data))))
}
\arguments{

  \item{data}{a matrix or a list. If it is a matrix, density will be calculated by columns.}
  \item{col}{a list of colors that density values are scaled to.}
  \item{anno}{annotation for matrix columns or list. The value should be a vector or a data frame. It can also be a \code{\link{HeatmapAnnotation-class}} object.}
  \item{ylab}{label on y-axis in the plot}
  \item{title}{title of the plot}

}
\details{
To visualize distribution of columns in a matrix or in a list, sometimes we use boxplot or beanplot.
Here we use colors to map to the density values and visualize distribution of values
in each column (or each element in the list) through a heatmap. It is useful if you have huge number of columns in \code{data} to visualize.
}
\value{
No value is returned.
}
\examples{
matrix = matrix(rnorm(100), 10); colnames(matrix) = letters[1:10]
densityHeatmap(matrix)
densityHeatmap(matrix, anno = rep(c("A", "B"), each = 5))
densityHeatmap(matrix, col = c("white", "red"), anno = rep(c("A", "B"), each = 5))
ha = HeatmapAnnotation(points = anno_points(runif(10)))
densityHeatmap(matrix, anno = ha)
lt = list(rnorm(10), rnorm(10))
densityHeatmap(lt)
}
