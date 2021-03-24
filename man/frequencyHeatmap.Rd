\name{frequencyHeatmap}
\alias{frequencyHeatmap}
\title{
Visualize Frequency Distribution by Heatmap
}
\description{
Visualize Frequency Distribution by Heatmap
}
\usage{
frequencyHeatmap(data,
    breaks = "Sturges",
    stat = c("count", "density", "proportion"),
    
    col = brewer.pal(9, "Blues"),
    color_space = "LAB",
    ylab = deparse(substitute(data)),
    column_title = paste0("Frequency heatmap of ", deparse(substitute(data))),
    title = column_title,
    ylim = NULL,
    range = ylim,
    
    title_gp = gpar(fontsize = 14),
    ylab_gp = gpar(fontsize = 12),
    tick_label_gp = gpar(fontsize = 10),
    
    column_order = NULL,
    column_names_side = "bottom",
    show_column_names = TRUE,
    column_names_max_height = unit(6, "cm"),
    column_names_gp = gpar(fontsize = 12),
    column_names_rot = 90,
    cluster_columns = FALSE,
    
    use_3d = FALSE,
    ...)
}
\arguments{

  \item{data}{A matrix or a list. If it is a matrix, density is calculated by columns.}
  \item{breaks}{Pass to \code{\link[graphics]{hist}}. Please only set equal bin size.}
  \item{stat}{Statistic to use.}
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
  \item{column_order}{Order of columns.}
  \item{column_names_side}{Pass to \code{\link{Heatmap}}.}
  \item{show_column_names}{Pass to \code{\link{Heatmap}}.}
  \item{column_names_max_height}{Pass to \code{\link{Heatmap}}.}
  \item{column_names_gp}{Pass to \code{\link{Heatmap}}.}
  \item{column_names_rot}{Pass to \code{\link{Heatmap}}.}
  \item{cluster_columns}{Whether cluster columns?}
  \item{use_3d}{Whether to visualize the frequencies as a 3D heatmap with \code{\link{Heatmap3D}}?}
  \item{...}{Pass to \code{\link{Heatmap}} or \code{\link{Heatmap3D}} (if \code{use_3d = TRUE}).}

}
\value{
A \code{\link{Heatmap-class}} object. It can oly add other heatmaps/annotations vertically.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
matrix = matrix(rnorm(100), 10); colnames(matrix) = letters[1:10]
frequencyHeatmap(matrix)
frequencyHeatmap(matrix, use_3d = TRUE)
}
