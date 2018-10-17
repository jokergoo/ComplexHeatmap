\name{make_layout-HeatmapList-method}
\alias{make_layout,HeatmapList-method}
\title{
Make layout for the complete plot
}
\description{
Make layout for the complete plot
}
\usage{
\S4method{make_layout}{HeatmapList}(object,
    
    row_title = character(0),
    row_title_side = c("left", "right"),
    row_title_gp = gpar(fontsize = 14),
    column_title = character(0),
    column_title_side = c("top", "bottom"),
    column_title_gp = gpar(fontsize = 14),
    
    heatmap_legend_side = c("right", "left", "bottom", "top"),
    merge_legends = FALSE,
    show_heatmap_legend = TRUE,
    heatmap_legend_list = list(),
    annotation_legend_side = c("right", "left", "bottom", "top"),
    show_annotation_legend = TRUE,
    annotation_legend_list = list(),
    
    ht_gap = unit(2, "mm"),
    
    main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
    padding = GLOBAL_PADDING,
    
    auto_adjust = TRUE,
    row_dend_side = c("original", "left", "right"),
    row_sub_title_side = c("original", "left", "right"),
    column_dend_side = c("original", "top", "bottom"),
    column_sub_title_side = c("original", "top", "bottom"),
    
    row_gap = NULL,
    cluster_rows = NULL,
    clustering_distance_rows = NULL,
    clustering_method_rows = NULL,
    row_dend_width = NULL,
    show_row_dend = NULL,
    row_dend_reorder = NULL,
    row_dend_gp = NULL,
    row_order = NULL,
    row_km = NULL,
    row_split = NULL,
    height = NULL,
    heatmap_height = NULL,
    
    column_gap = NULL,
    cluster_columns = NULL,
    clustering_distance_columns = NULL,
    clustering_method_columns = NULL,
    column_dend_width = NULL,
    show_column_dend = NULL,
    column_dend_reorder = NULL,
    column_dend_gp = NULL,
    column_order = NULL,
    column_km = NULL,
    column_split = NULL,
    width = NULL,
    heatmap_width = NULL)
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
  \item{merge_legends}{whether put heatmap legends and annotation legends in a same column}
  \item{show_heatmap_legend}{whether show heatmap legend.}
  \item{heatmap_legend_list}{a list of self-defined legend, should be wrapped into \code{\link[grid]{grob}} objects.}
  \item{annotation_legend_side}{side of annotation legend.}
  \item{show_annotation_legend}{whether show annotation legend.}
  \item{annotation_legend_list}{a list of self-defined legend, should be wrapped into \code{\link[grid]{grob}} objects.}
  \item{ht_gap}{gap between heatmaps, should be a \code{\link[grid]{unit}} object.}
  \item{main_heatmap}{name or index for the main heatmap}
  \item{padding}{padding of the plot}
  \item{auto_adjust}{whether autoadjust}
  \item{row_dend_side}{if auto adjust, where to put the row dendrograms for the main heatmap}
  \item{row_sub_title_side}{row sub title}
  \item{column_dend_side}{column dend}
  \item{column_sub_title_side}{column sub title}
  \item{row_gap}{gap between row clusters if rows are split}
  \item{cluster_rows}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{cluster_rows} in main heatmap is ignored.}
  \item{clustering_distance_rows}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{clustering_distance_rows} in main heatmap is ignored.}
  \item{clustering_method_rows}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{clustering_method_rows} in main heatmap is ignored.}
  \item{row_dend_width}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{row_dend_width} in main heatmap is ignored.}
  \item{show_row_dend}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{show_row_dend} in main heatmap is ignored.}
  \item{row_dend_reorder}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{row_dend_reorder} in main heatmap is ignored.}
  \item{row_dend_gp}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{row_dend_gp} in main heatmap is ignored.}
  \item{row_order}{same setting as in \code{\link{Heatmap}}, if it is specified, \code{row_order} in main heatmap is ignored.}
  \item{row_km}{row km}
  \item{row_split}{row split}
  \item{height}{height of the heatmap body}
  \item{heatmap_height}{height of the complete heatmap}
  \item{column_gap}{column gap}
  \item{cluster_columns}{cluster columns}
  \item{clustering_distance_columns}{clustering distance columns}
  \item{clustering_method_columns}{clustering method columns}
  \item{column_dend_width}{column dend width}
  \item{show_column_dend}{show column dendrogram}
  \item{column_dend_reorder}{column dend reorder}
  \item{column_dend_gp}{column dendrogram gp}
  \item{column_order}{column order}
  \item{column_km}{column km}
  \item{column_split}{column split}
  \item{width}{width of the heatmap body}
  \item{heatmap_width}{width of the complete heatmap}

}
\details{
It sets the size of each component of the heatmap list and adjusts graphic
parameters for each heatmap if necessary.

The layout for the heatmap list and layout for each heatmap are calculated
when drawing the heatmap list.

This function is only for internal use.
}
\value{
A \code{\link{HeatmapList-class}} object in which settings for each heatmap are adjusted.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL
}
