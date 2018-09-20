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
    top_annotation = NULL,
    bottom_annotation = NULL,
    ylab = deparse(substitute(data)),
    column_title = paste0("Density heatmap of ", deparse(substitute(data))),
    title = column_title,
    ylim = c(-Inf, Inf),
    range = ylim,
    
    title_gp = gpar(fontsize = 14),
    ylab_gp = gpar(fontsize = 12),
    tick_label_gp = gpar(fontsize = 10),
    quantile_gp = gpar(fontsize = 10),
    
    column_order = NULL,
    column_names_side = "bottom",
    show_column_names = TRUE,
    column_names_max_height = unit(6, "cm"),
    column_names_gp = gpar(fontsize = 12),
    column_names_rot = 90,
    
    cluster_columns = FALSE,
    ...)
}
\arguments{

  \item{data}{A matrix or a list. If it is a matrix, density will be calculated by columns.}
  \item{density_param}{Parameters send to \code{\link[stats]{density}}, \code{na.rm} is enforced to be \code{TRUE}.}
  \item{col}{A vector of colors that density values are mapped to.}
  \item{color_space}{The color space in which colors are interpolated. Pass to \code{\link[circlize]{colorRamp2}}.}
  \item{top_annotation}{A \code{\link{HeatmapAnnotation-class}} object which is put on top of the heatmap.}
  \item{bottom_annotation}{A \code{\link{HeatmapAnnotation-class}} object which is put at bottom of the heatmap.}
  \item{ylab}{Label on y-axis.}
  \item{column_title}{Title of the heatmap.}
  \item{title}{Same as \code{column_title}.}
  \item{ylim}{Ranges on the y-axis. By default the range is between 1th quantile and 99th quantile of the data.}
  \item{range}{Same as \code{ylim}.}
  \item{title_gp}{= gpar(fontsize = 14),}
  \item{ylab_gp}{= gpar(fontsize = 12),}
  \item{tick_label_gp}{= gpar(fontsize = 10),}
  \item{quantile_gp}{= gpar(fontsize = 10),# -column_order Order of columns.}
  \item{column_order}{column_order}
  \item{column_names_side}{Pass to \code{\link{Heatmap}}.}
  \item{show_column_names}{Pass to \code{\link{Heatmap}}.}
  \item{column_names_max_height}{Pass to \code{\link{Heatmap}}.}
  \item{column_names_gp}{Pass to \code{\link{Heatmap}}.}
  \item{column_names_rot}{Pass to \code{\link{Heatmap}}.}
  \item{cluster_columns}{Whether cluster columns (here clustered by density values)? Normally we don't cluster columns.}
  \item{...}{pass to \code{\link{Heatmap}}.}

}
\details{
To visualize data distribution in a matrix or in a list, sometimes we use boxplot or beanplot.
Here we use colors to map the density values and visualize distribution of values
in each column (or each vector in the list) through a heatmap. It is useful if you have huge number 
of columns in \code{data} to visualize.

The density matrix is generated with 500 rows ranging between the maximun and minimal values in all densities.
The density values in each row are linearly intepolated between the two density values at the two nearest bounds.
}
\value{
A \code{\link{HeatmapList-class}} object with only one heatmap, but it can only add other heatmaps/annotations vertically.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
matrix = matrix(rnorm(100), 10); colnames(matrix) = letters[1:10]
densityHeatmap(matrix)

ha = HeatmapAnnotation(points = anno_points(runif(10)),
    anno = rep(c("A", "B"), each = 5), col = list(anno = c("A" = "red", "B" = "blue")))
densityHeatmap(matrix, top_annotation = ha)

lt = list(rnorm(10), rnorm(10))
densityHeatmap(lt)
}
