\name{draw-HeatmapList-method}
\alias{draw,HeatmapList-method}
\title{
Draw a list of heatmaps
}
\description{
Draw a list of heatmaps
}
\usage{
\S4method{draw}{HeatmapList}(object,
    newpage = TRUE,
    background = "white",
    
    row_title = character(0),
    row_title_side = c("left", "right"),
    row_title_gp = gpar(fontsize = 13),
    column_title = character(0),
    column_title_side = c("top", "bottom"),
    column_title_gp = gpar(fontsize = 13),
    
    heatmap_legend_side = c("right", "left", "bottom", "top"),
    merge_legends = ht_opt$merge_legends,
    show_heatmap_legend = TRUE,
    heatmap_legend_list = list(),
    annotation_legend_side = c("right", "left", "bottom", "top"),
    show_annotation_legend = TRUE,
    annotation_legend_list = list(),
    align_heatmap_legend = NULL,
    align_annotation_legend = NULL,
    legend_grouping = c("adjusted", "original"),
    
    gap = unit(2, "mm"),
    ht_gap = gap,
    
    main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
    padding = GLOBAL_PADDING,
    adjust_annotation_extension = NULL,
    
    auto_adjust = TRUE,
    row_dend_side = c("original", "left", "right"),
    row_sub_title_side = c("original", "left", "right"),
    column_dend_side = c("original", "top", "bottom"),
    column_sub_title_side = c("original", "top", "bottom"),
    
    row_gap = NULL,
    cluster_rows = NULL,
    cluster_row_slices = NULL,
    clustering_distance_rows = NULL,
    clustering_method_rows = NULL,
    row_dend_width = NULL,
    show_row_dend = NULL,
    row_dend_reorder = NULL,
    row_dend_gp = NULL,
    row_order = NULL,
    km = NULL,
    split = NULL,
    row_km = km,
    row_km_repeats = NULL,
    row_split = split,
    height = NULL,
    heatmap_height = NULL,
    
    column_gap = NULL,
    cluster_columns = NULL,
    cluster_column_slices = NULL,
    clustering_distance_columns = NULL,
    clustering_method_columns = NULL,
    column_dend_width = NULL,
    show_column_dend = NULL,
    column_dend_reorder = NULL,
    column_dend_gp = NULL,
    column_order = NULL,
    column_km = NULL,
    column_km_repeats = NULL,
    column_split = NULL,
    width = NULL,
    heatmap_width = NULL,
    
    use_raster = NULL,
    raster_device = NULL,
    raster_quality = NULL,
    raster_device_param = NULL,
    raster_resize = NULL,
    
    post_fun = NULL,
    save_last = ht_opt$save_last,
    
    ### global setting
    heatmap_row_names_gp = NULL,
    heatmap_column_names_gp = NULL,
    heatmap_row_title_gp = NULL,
    heatmap_column_title_gp = NULL,
    legend_title_gp = NULL,
    legend_title_position = NULL,
    legend_labels_gp = NULL,
    legend_grid_height = NULL,
    legend_grid_width = NULL,
    legend_border = NULL,
    legend_gap = NULL,
    heatmap_border = NULL,
    annotation_border = NULL,
    fastcluster = NULL,
    simple_anno_size = NULL,
    show_parent_dend_line = NULL)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object.}
  \item{newpage}{whether create a new page for the graphics. If you want to arrange multiple plots in one page, I suggest to use \code{\link[grid:grid.grab]{grid.grabExpr}}.}
  \item{background}{Background color of the whole plot.}
  \item{row_title}{title on the row.}
  \item{row_title_side}{will the title be put on the left or right of the heatmap.}
  \item{row_title_gp}{graphic parameters for drawing text.}
  \item{column_title}{title on the column.}
  \item{column_title_side}{will the title be put on the top or bottom of the heatmap.}
  \item{column_title_gp}{graphic parameters for drawing text.}
  \item{heatmap_legend_side}{side to put heatmap legend}
  \item{merge_legends}{merge heatmap legends and annotation legends to put into one column.}
  \item{show_heatmap_legend}{whether show all heatmap legends}
  \item{heatmap_legend_list}{use-defined legends which are put after the heatmap legends}
  \item{annotation_legend_side}{side of the annotation legends}
  \item{show_annotation_legend}{whether show annotation legends}
  \item{annotation_legend_list}{user-defined legends which are put after the annotation legends}
  \item{align_heatmap_legend}{How to align the legends to heatmap. Possible values are "heatmap_center", "heatmap_top" and "global_center". If the value is \code{NULL}, it automatically picks the proper value from the three options.}
  \item{align_annotation_legend}{How to align the legends to heatmap. Possible values are "heatmap_center", "heatmap_top" and "global_center".}
  \item{legend_grouping}{How the legends are grouped. Values should be "adjusted" or "original". If it is set as "original", all annotation legends are grouped together.}
  \item{gap}{gap between heatmaps/annotations}
  \item{ht_gap}{same as \code{gap}.}
  \item{main_heatmap}{index of main heatmap. The value can be a numeric index or the heatmap name}
  \item{padding}{padding of the whole plot. The value is a unit vector of length 4, which corresponds to bottom, left, top and right.}
  \item{adjust_annotation_extension}{whether take annotation name into account when calculating positions of graphic elements.}
  \item{auto_adjust}{whether apply automatic adjustment? The auto-adjustment includes turning off dendrograms, titles and row/columns for non-main heatmaps.}
  \item{row_dend_side}{side of the dendrogram from the main heatmap}
  \item{row_sub_title_side}{side of the row title from the main heatmap}
  \item{column_dend_side}{side of the dendrogram from the main heatmap}
  \item{column_sub_title_side}{side of the column title from the main heatmap}
  \item{row_gap}{this modifies \code{row_gap} of the main heatmap}
  \item{cluster_rows}{this modifies \code{cluster_rows} of the main heatmap}
  \item{cluster_row_slices}{this modifies \code{cluster_row_slices} of the main heatmap}
  \item{clustering_distance_rows}{this modifies \code{clustering_distance_rows} of the main heatmap}
  \item{clustering_method_rows}{this modifies \code{clustering_method_rows} of the main heatmap}
  \item{row_dend_width}{this modifies \code{row_dend_width} of the main heatmap}
  \item{show_row_dend}{this modifies \code{show_row_dend} of the main heatmap}
  \item{row_dend_reorder}{this modifies \code{row_dend_reorder} of the main heatmap}
  \item{row_dend_gp}{this modifies \code{row_dend_gp} of the main heatmap}
  \item{row_order}{this modifies \code{row_order} of the main heatmap}
  \item{km}{= this modifies \code{km} of the main heatmap}
  \item{split}{this modifies \code{split} of the main heatmap}
  \item{row_km}{this modifies \code{row_km} of the main heatmap}
  \item{row_km_repeats}{this modifies \code{row_km_repeats} of the main heatmap}
  \item{row_split}{this modifies \code{row_split} of the main heatmap}
  \item{height}{this modifies \code{height} of the main heatmap}
  \item{heatmap_height}{this modifies \code{heatmap_height} of the main heatmap}
  \item{column_gap}{this modifies \code{column_gap} of the main heatmap}
  \item{cluster_columns}{this modifies \code{cluster_columns} of the main heatmap}
  \item{cluster_column_slices}{this modifies \code{cluster_column_slices} of the main heatmap}
  \item{clustering_distance_columns}{this modifies \code{clustering_distance_columns} of the main heatmap}
  \item{clustering_method_columns}{this modifies \code{clustering_method_columns} of the main heatmap}
  \item{column_dend_width}{this modifies \code{column_dend_width} of the main heatmap}
  \item{show_column_dend}{this modifies \code{show_column_dend} of the main heatmap}
  \item{column_dend_reorder}{this modifies \code{column_dend_reorder} of the main heatmap}
  \item{column_dend_gp}{this modifies \code{column_dend_gp} of the main heatmap}
  \item{column_order}{this modifies \code{column_order} of the main heatmap}
  \item{column_km}{this modifies \code{column_km} of the main heatmap}
  \item{column_km_repeats}{this modifies \code{column_km_repeats} of the main heatmap}
  \item{column_split}{this modifies \code{column_split} of the main heatmap}
  \item{width}{this modifies \code{width} of the main heatmap}
  \item{heatmap_width}{this modifies \code{heatmap_width} of the main heatmap}
  \item{use_raster}{this modifies \code{use_raster} of every heatmap.}
  \item{raster_device}{this modifies \code{raster_device} of every heatmap.}
  \item{raster_quality}{this modifies \code{raster_quality} of every heatmap.}
  \item{raster_device_param}{this modifies \code{raster_device_param} of every heatmap.}
  \item{raster_resize}{this modifies \code{raster_resize} of every heatmap.}
  \item{post_fun}{A self-defined function will be executed after all the heatmaps are drawn.}
  \item{save_last}{Whether to save the last plot?}
  \item{heatmap_row_names_gp}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{heatmap_column_names_gp}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{heatmap_row_title_gp}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{heatmap_column_title_gp}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{legend_title_gp}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{legend_title_position}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{legend_labels_gp}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{legend_grid_height}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{legend_grid_width}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{legend_border}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{legend_gap}{Gap between legends. The value should be a vector of two units. One for gaps between vertical legends and one for the horizontal legends. If only one single unit is specified, the same gap set for the vertical and horizontal legends. }
  \item{heatmap_border}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{annotation_border}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{fastcluster}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{simple_anno_size}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{show_parent_dend_line}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}

}
\details{
The function first calls \code{\link{make_layout,HeatmapList-method}} to calculate
the layout of the heatmap list and the layout of every single heatmap,
then makes the plot by re-calling the graphic functions which are already recorded
in the layout.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/a-list-of-heatmaps.html}
}
\value{
This function returns a \code{\link{HeatmapList-class}} object for which the layout has been created.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
