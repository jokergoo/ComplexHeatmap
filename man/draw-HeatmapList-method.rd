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
    
    row_title = character(0),
    row_title_side = c("left", "right"),
    row_title_gp = gpar(fontsize = 14),
    column_title = character(0),
    column_title_side = c("top", "bottom"),
    column_title_gp = gpar(fontsize = 14),
    
    heatmap_legend_side = c("right", "left", "bottom", "top"),
    heatmap_legend_offset = unit(0, "mm"),
    merge_legends = FALSE,
    show_heatmap_legend = TRUE,
    heatmap_legend_list = list(),
    annotation_legend_side = c("right", "left", "bottom", "top"),
    annotation_legend_offset = unit(0, "mm"),
    show_annotation_legend = TRUE,
    annotation_legend_list = list(),
    
    gap = unit(2, "mm"),
    ht_gap = gap,
    
    main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
    padding = GLOBAL_PADDING,
    adjust_annotation_extension = TRUE,
    
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
    km = NULL,
    split = NULL,
    row_km = km,
    row_split = split,
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
    heatmap_width = NULL,
    
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
    heatmap_border = NULL,
    annotation_border = NULL,
    fastcluster = NULL,
    anno_simple_size = NULL)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object.}
  \item{newpage}{whether create a new page for the graphics. If you want to arrange multiple plots in one page, I suggest to use \code{\link[grid]{grabExpr}}.}
  \item{row_title}{title on the row.}
  \item{row_title_side}{will the title be put on the left or right of the heatmap.}
  \item{row_title_gp}{graphic parameters for drawing text.}
  \item{column_title}{title on the column.}
  \item{column_title_side}{will the title be put on the top or bottom of the heatmap.}
  \item{column_title_gp}{graphic parameters for drawing text.}
  \item{heatmap_legend_side}{side to put heatmap legend}
  \item{heatmap_legend_offset}{currently disabled}
  \item{merge_legends}{merge heatmap legends and annotation legends to put into one column.}
  \item{show_heatmap_legend}{whether show all heatmap legends}
  \item{heatmap_legend_list}{use-defined legends which are put after the heatmap legends}
  \item{annotation_legend_side}{side of the annotation legends}
  \item{annotation_legend_offset}{currently disabled}
  \item{show_annotation_legend}{whether show annotation legends}
  \item{annotation_legend_list}{user-defined legends which are put after the annotation legends}
  \item{gap}{gap between heatmaps/annotations}
  \item{ht_gap}{same as \code{gap}.}
  \item{main_heatmap}{index of main heatmap. The value can be a numeric index or the heatmap name}
  \item{padding}{padding of the whole plot. The value is a unit vector of length 4, which corresponds to bottom, left, top and right.}
  \item{adjust_annotation_name}{whether take annotation name into account when calculating positions of graphic elements.}
  \item{row_dend_side}{side of the dendrogram from the main heatmap}
  \item{row_sub_title_side}{side of the row title from the main heatmap}
  \item{column_dend_side}{side of the dendrogram from the main heatmap}
  \item{column_sub_title_side}{side of the column title from the main heatmap}
  \item{row_gap}{this modifies \code{row_gap} of the main heatmap}
  \item{cluster_rows}{this modifies \code{cluster_rows} of the main heatmap}
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
  \item{row_split}{this modifies \code{row_split} of the main heatmap}
  \item{heatmap_body_height}{this modifies \code{heatmap_body_height} of the main heatmap}
  \item{column_gap}{this modifies \code{column_gap} of the main heatmap}
  \item{cluster_columns}{this modifies \code{cluster_columns} of the main heatmap}
  \item{clustering_distance_columns}{this modifies \code{clustering_distance_columns} of the main heatmap}
  \item{clustering_method_columns}{this modifies \code{clustering_method_columns} of the main heatmap}
  \item{column_dend_width}{this modifies \code{column_dend_width} of the main heatmap}
  \item{show_column_dend}{this modifies \code{show_column_dend} of the main heatmap}
  \item{column_dend_reorder}{this modifies \code{column_dend_reorder} of the main heatmap}
  \item{column_dend_gp}{this modifies \code{column_dend_gp} of the main heatmap}
  \item{column_order}{this modifies \code{column_order} of the main heatmap}
  \item{column_km}{this modifies \code{column_km} of the main heatmap}
  \item{column_split}{this modifies \code{column_split} of the main heatmap}
  \item{heatmap_body_width}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
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
  \item{heatmap_border}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{annotation_border}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{fastcluster}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}
  \item{anno_simple_size}{this set the value in \code{\link{ht_opt}} and reset back after the plot is done}

}
\details{
The function first calls \code{\link{make_layout,HeatmapList-method}} to calculate
the layout of the heatmap list and the layout of every single heatmap,
then makes the plot by re-calling the graphic functions which are already recorded
in the layout.
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
