\name{make_layout-HeatmapList-method}
\alias{make_layout,HeatmapList-method}
\title{
Make layout for the complete plot
}
\description{
Make layout for the complete plot
}
\usage{
\S4method{make_layout}{HeatmapList}(object, row_title = character(0),
    row_title_side = c("left", "right"),
    row_title_gp = gpar(fontsize = 14),
    column_title = character(0),
    column_title_side = c("top", "bottom"),
    column_title_gp = gpar(fontsize = 14),
    heatmap_legend_side = c("right", "left", "bottom", "top"),
    show_heatmap_legend = TRUE,
    heatmap_legend_list = list(),
    annotation_legend_side = c("right", "left", "bottom", "top"),
    show_annotation_legend = TRUE,
    annotation_legend_list = list(),
    gap = unit(3, "mm"),
    main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
    row_dend_side = c("original", "left", "right"),
    row_hclust_side = row_dend_side,
    row_sub_title_side = c("original", "left", "right"),
    cluster_rows = NULL,
    clustering_distance_rows = NULL,
    clustering_method_rows = NULL,
    row_dend_width = NULL,
    show_row_dend = NULL,
    row_dend_reorder = NULL,
    row_dend_gp = NULL,
    row_order = NULL,
    km = NULL,
    split = NULL)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object.}
  \item{row_title}{title on the row.}
  \item{row_title_side}{will the title be put on the left or right of the heatmap.}
  \item{row_title_gp}{graphic parameters for drawing text.}
  \item{column_title}{title on the column.}
  \item{column_title_side}{will the title be put on the top or bottom of the heatmap.}
  \item{column_title_gp}{graphic parameters for drawing text.}
  \item{heatmap_legend_side}{side of the heatmap legend.}
  \item{show_heatmap_legend}{whether show heatmap legend.}
  \item{heatmap_legend_list}{a list of self-defined legend, should be wrapped into \code{\link[grid]{grob}} objects.}
  \item{annotation_legend_side}{side of annotation legend.}
  \item{show_annotation_legend}{whether show annotation legend.}
  \item{annotation_legend_list}{a list of self-defined legend, should be wrapped into \code{\link[grid]{grob}} objects.}
  \item{gap}{gap between heatmaps, should be a \code{\link[grid]{unit}} object.}
  \item{main_heatmap}{name or index for the main heatmap}
  \item{row_dend_side}{if auto adjust, where to put the row dendrograms for the main heatmap}
  \item{row_hclust_side}{deprecated, use \code{row_dend_side} instead}
  \item{row_sub_title_side}{if auto adjust, where to put sub row titles for the main heatmap}
  \item{cluster_rows}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{cluster_rows} in main heatmap is ignored.}
  \item{clustering_distance_rows}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{clustering_distance_rows} in main heatmap is ignored.}
  \item{clustering_method_rows}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{clustering_method_rows} in main heatmap is ignored.}
  \item{row_dend_width}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{row_dend_width} in main heatmap is ignored.}
  \item{show_row_dend}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{show_row_dend} in main heatmap is ignored.}
  \item{row_dend_reorder}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{row_dend_reorder} in main heatmap is ignored.}
  \item{row_dend_gp}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{row_dend_gp} in main heatmap is ignored.}
  \item{row_order}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{row_order} in main heatmap is ignored.}
  \item{km}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{km} in main heatmap is ignored.}
  \item{split}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{split} in main heatmap is ignored.}

}
\details{
It sets the size of each component of the heatmap list and adjusts graphic parameters for each heatmap if necessary.

The layout for the heatmap list and layout for each heatmap are calculated when drawing the heatmap list.

This function is only for internal use.
}
\value{
A \code{\link{HeatmapList-class}} object in which settings for each heatmap are adjusted.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# no example for this internal method
NULL

}
