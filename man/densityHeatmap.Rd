\name{densityHeatmap}
\alias{densityHeatmap}
\title{
Visualize Density Distribution by Heatmap
}
\description{
Visualize Density Distribution by Heatmap
}
\usage{
densityHeatmap(data,
    density_param = list(na.rm = TRUE),
    
    col = rev(brewer.pal(11, "Spectral")),
    color_space = "LAB",
    ylab = deparse(substitute(data)),
    column_title = paste0("Density heatmap of ", deparse(substitute(data))),
    title = column_title,
    ylim = NULL,
    range = ylim,
    
    title_gp = gpar(fontsize = 14),
    ylab_gp = gpar(fontsize = 12),
    tick_label_gp = gpar(fontsize = 10),
    quantile_gp = gpar(fontsize = 10),
    show_quantiles = TRUE,
    
    column_order = NULL,
    column_names_side = "bottom",
    show_column_names = TRUE,
    column_names_max_height = unit(6, "cm"),
    column_names_gp = gpar(fontsize = 12),
    column_names_rot = 90,
    
    cluster_columns = FALSE,
    clustering_distance_columns = "ks",
    clustering_method_columns = "complete",
    mc.cores = 1, cores = mc.cores,
    
    ...)
}
\arguments{

  \item{data}{A matrix or a list. If it is a matrix, density is calculated by columns.}
  \item{density_param}{Parameters send to \code{\link[stats]{density}}, \code{na.rm} is enforced to be \code{TRUE}.}
  \item{col}{A vector of colors that density values are mapped to.}
  \item{color_space}{The color space in which colors are interpolated. Pass to \code{\link[circlize]{colorRamp2}}.}
  \item{ylab}{Label on y-axis.}
  \item{column_title}{Title of the heatmap.}
  \item{title}{Same as \code{column_title}.}
  \item{ylim}{Ranges on the y-axis.}
  \item{range}{Same as \code{ylim}.}
  \item{title_gp}{Graphic parameters for title.}
  \item{ylab_gp}{Graphic parameters for y-labels.}
  \item{tick_label_gp}{Graphic parameters for y-ticks.}
  \item{quantile_gp}{Graphic parameters for the quantiles.}
  \item{show_quantiles}{Whether show quantile lines.}
  \item{column_order}{Order of columns.}
  \item{column_names_side}{Pass to \code{\link{Heatmap}}.}
  \item{show_column_names}{Pass to \code{\link{Heatmap}}.}
  \item{column_names_max_height}{Pass to \code{\link{Heatmap}}.}
  \item{column_names_gp}{Pass to \code{\link{Heatmap}}.}
  \item{column_names_rot}{Pass to \code{\link{Heatmap}}.}
  \item{cluster_columns}{Whether cluster columns?}
  \item{clustering_distance_columns}{There is a specific distance method \code{ks} which is the Kolmogorov-Smirnov statistic between two distributions. For other methods, the distance is calculated on the density matrix.}
  \item{clustering_method_columns}{Pass to \code{\link{Heatmap}}.}
  \item{mc.cores}{Multiple cores for calculating ks distance. This argument will be removed in future versions.}
  \item{cores}{Multiple cores for calculating ks distance.}
  \item{...}{Pass to \code{\link{Heatmap}}.}

}
\details{
To visualize data distribution in a matrix or in a list, we normally use
boxplot or violinplot. We can also use colors to map the density values and
visualize distribution of values through a heatmap. It is useful if you have
huge number of columns in \code{data} to visualize.

The density matrix is generated with 500 rows ranging between the maximun
and minimal values in all densities.
}
\value{
A \code{\link{Heatmap-class}} object. It can oly add other heatmaps/annotations vertically.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/other-high-level-plots.html#density-heatmap}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
matrix = matrix(rnorm(100), 10); colnames(matrix) = letters[1:10]
densityHeatmap(matrix)

lt = list(rnorm(10), rnorm(10))
densityHeatmap(lt)

ha = HeatmapAnnotation(points = anno_points(runif(10)),
    anno = rep(c("A", "B"), each = 5), col = list(anno = c("A" = "red", "B" = "blue")))
densityHeatmap(matrix, top_annotation = ha)
densityHeatmap(matrix, top_annotation = ha) \%v\% Heatmap(matrix, height = unit(6, "cm"))
}
