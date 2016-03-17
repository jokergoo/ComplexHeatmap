\name{densityHeatmap}
\alias{densityHeatmap}
\title{
Use colors to represent density distribution
}
\description{
Use colors to represent density distribution
}
\usage{
densityHeatmap(data,
    col = rev(brewer.pal(11, "Spectral")),
    color_space = "LAB",
    anno = NULL,
    ylab = deparse(substitute(data)),
    title = paste0("Density heatmap of ", deparse(substitute(data))),
    range = c(-Inf, Inf),
    column_order = NULL,
    ...)
}
\arguments{

  \item{data}{a matrix or a list. If it is a matrix, density will be calculated by columns.}
  \item{col}{a list of colors that density values are mapped to.}
  \item{color_space}{the color space in which colors are interpolated. Pass to \code{\link[circlize]{colorRamp2}}.}
  \item{anno}{annotation for the matrix columns or the list. The value should be a vector or a data frame  and colors for annotations are randomly assigned. If you want to customize the annotation colors, use a \code{\link{HeatmapAnnotation-class}} object directly.}
  \item{ylab}{label on y-axis in the plot}
  \item{title}{title of the plot}
  \item{range}{ranges on the y-axis. By default the range is between 1th quantile and 99th quantile of the data.}
  \item{column_order}{order of columns}
  \item{...}{pass to \code{\link{draw,HeatmapList-method}}}

}
\details{
To visualize data distribution in a matrix or in a list, sometimes we use boxplot or beanplot.
Here we use colors to map the density values and visualize distribution of values
in each column (or each vector in the list) through a heatmap. It is useful if you have huge number 
of columns in \code{data} to visualize.
}
\value{
No value is returned.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
matrix = matrix(rnorm(100), 10); colnames(matrix) = letters[1:10]
densityHeatmap(matrix)
densityHeatmap(matrix, anno = rep(c("A", "B"), each = 5))
densityHeatmap(matrix, col = c("white", "red"), anno = rep(c("A", "B"), each = 5))

ha = HeatmapAnnotation(points = anno_points(runif(10)),
    anno = rep(c("A", "B"), each = 5), col = list(anno = c("A" = "red", "B" = "blue")))
densityHeatmap(matrix, anno = ha)

lt = list(rnorm(10), rnorm(10))
densityHeatmap(lt)
}
