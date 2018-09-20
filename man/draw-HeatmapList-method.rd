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
    padding = NULL,
    
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
    heatmap_body_height = NULL,
    
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
    heatmap_body_width = NULL,
    
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
    legend_grid_border = NULL,
    fastcluster = NULL,
    show_vp_border = NULL,
    anno_simple_row_size = NULL)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList-class}} object}
  \item{newpage}{whether create a new page for the graphics.}
  \item{row_title}{title on the row.}
  \item{row_title_side}{will the title be put on the left or right of the heatmap.}
  \item{row_title_gp}{graphic parameters for drawing text.}
  \item{column_title}{title on the column.}
  \item{column_title_side}{will the title be put on the top or bottom of the heatmap.}
  \item{column_title_gp}{graphic parameters for drawing text.}
  \item{heatmap_legend_side}{= c("right", "left", "bottom", "top"), }
  \item{heatmap_legend_offset}{= unit(0, "mm"),}
  \item{merge_legends}{= FALSE,}
  \item{show_heatmap_legend}{= TRUE, }
  \item{heatmap_legend_list}{= list(),}
  \item{annotation_legend_side}{= c("right", "left", "bottom", "top"), }
  \item{annotation_legend_offset}{= unit(0, "mm"),}
  \item{show_annotation_legend}{= TRUE, }
  \item{annotation_legend_list}{= list(),}
  \item{gap}{= unit(2, "mm"), }
  \item{ht_gap}{= gap, }
  \item{main_heatmap}{= which(sapply(object@ht_list, inherits, "Heatmap"))[1],}
  \item{padding}{= NULL,}
  \item{row_dend_side}{= c("original", "left", "right"),}
  \item{row_sub_title_side}{= c("original", "left", "right"),}
  \item{column_dend_side}{= c("original", "top", "bottom"),}
  \item{column_sub_title_side}{= c("original", "top", "bottom"), }
  \item{row_gap}{= NULL,}
  \item{cluster_rows}{= NULL,}
  \item{clustering_distance_rows}{= NULL,}
  \item{clustering_method_rows}{= NULL,}
  \item{row_dend_width}{= NULL, }
  \item{show_row_dend}{= NULL, }
  \item{row_dend_reorder}{= NULL,}
  \item{row_dend_gp}{= NULL,}
  \item{row_order}{= NULL,}
  \item{km}{= NULL,}
  \item{split}{= NULL,}
  \item{row_km}{= km,}
  \item{row_split}{= split,}
  \item{heatmap_body_height}{= NULL,}
  \item{column_gap}{= NULL,}
  \item{cluster_columns}{= NULL,}
  \item{clustering_distance_columns}{= NULL,}
  \item{clustering_method_columns}{= NULL,}
  \item{column_dend_width}{= NULL, }
  \item{show_column_dend}{= NULL, }
  \item{column_dend_reorder}{= NULL,}
  \item{column_dend_gp}{= NULL,}
  \item{column_order}{= NULL,}
  \item{column_km}{= NULL,}
  \item{column_split}{= NULL,}
  \item{heatmap_body_width}{= NULL,}
  \item{heatmap_row_names_gp}{= NULL,}
  \item{heatmap_column_names_gp}{= NULL,}
  \item{heatmap_row_title_gp}{= NULL,}
  \item{heatmap_column_title_gp}{= NULL,}
  \item{legend_title_gp}{= NULL,}
  \item{legend_title_position}{= NULL,}
  \item{legend_labels_gp}{= NULL,}
  \item{legend_grid_height}{= NULL,}
  \item{legend_grid_width}{= NULL,}
  \item{legend_grid_border}{= NULL,}
  \item{fastcluster}{= NULL,}
  \item{show_vp_border}{= NULL,}
  \item{anno_simple_row_size}{= NULL}

}
\details{
The function first calls \code{\link{make_layout,HeatmapList-method}} to calculate
the layout of the heatmap list and the layout of every single heatmap,
then makes the plot by re-calling the graphic functions which are already recorded
in the layout.
}
\value{
This function returns a list of row dendrograms and column dendrogram.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL
}
