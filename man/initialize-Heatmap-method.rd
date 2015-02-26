\name{initialize-Heatmap-method}
\alias{initialize,Heatmap-method}
\title{
Constructor method of Heatmap class  


}
\description{
Constructor method of Heatmap class  


}
\usage{
\S4method{initialize}{Heatmap}(.Object, matrix, col, name, rect_gp = gpar(col = NA), cell_fun = function(i, j, x, y, width, height) NULL,
    row_title = character(0), row_title_side = c("left", "right"),
    row_title_gp = gpar(fontsize = 14), column_title = character(0),
    column_title_side = c("top", "bottom"), column_title_gp = gpar(fontsize = 14),
    cluster_rows = TRUE, clustering_distance_rows = "euclidean",
    clustering_method_rows = "complete", row_hclust_side = c("left", "right"),
    row_hclust_width = unit(10, "mm"), show_row_hclust = TRUE,
    row_hclust_gp = gpar(), cluster_columns = TRUE,
    clustering_distance_columns = "euclidean", clustering_method_columns = "complete",
    column_hclust_side = c("top", "bottom"), column_hclust_height = unit(10, "mm"),
    show_column_hclust = TRUE, column_hclust_gp = gpar(),
    row_names_side = c("right", "left"), show_row_names = TRUE, row_names_max_width = unit(2, "cm"),
    row_names_gp = gpar(fontsize = 12), column_names_side = c("bottom", "top"),
    show_column_names = TRUE, column_names_max_height = unit(2, "cm"), column_names_gp = gpar(fontsize = 12),
    top_annotation = NULL, top_annotation_height = unit(1, "cm"),
    bottom_annotation = NULL, bottom_annotation_height = unit(1, "cm"),
    km = 1, gap = unit(1, "mm"), split = NULL, combined_name_fun = function(x) paste(x, collapse = "/"),
    width = NULL)
}
\arguments{

  \item{.Object}{private object.}
  \item{matrix}{matrix. Either numeric or character. If it is a simple vector, it will be converted to a one-column matrix.}
  \item{col}{a vector of colors if the matrix is character or a color mapping  function if the matrix is numeric. Pass to \code{\link{initialize,ColorMapping-method}}.}
  \item{name}{name of the heatmap.}
  \item{rect_gp}{graphic parameters for drawing rectangles (for heatmap body).}
  \item{cell_fun}{function to add graphics on each cell}
  \item{row_title}{title on row.}
  \item{row_title_side}{will the title be put on the left or right of the heatmap.}
  \item{row_title_gp}{graphic parameters for drawing text.}
  \item{column_title}{title on column.}
  \item{column_title_side}{will the title be put on the top or bottom of the heatmap.}
  \item{column_title_gp}{graphic parameters for drawing text.}
  \item{cluster_rows}{whether make cluster on rows.}
  \item{clustering_distance_rows}{it can be a pre-defined character which is in  ("euclidean", "maximum", "manhattan", "canberra", "binary",  "minkowski", "pearson", "spearman", "kendall"). It can also be a function. If the function has one argument, the input argument should be a matrix and  the returned value should be a \code{\link[stats]{dist}} object. If the function has two arguments, the input arguments are two vectors and the function calcualtes distance between these two vectors.}
  \item{clustering_method_rows}{method to make cluster, pass to \code{\link[stats]{hclust}}.}
  \item{row_hclust_side}{should the row cluster be put on the left or right of the heatmap.}
  \item{row_hclust_width}{width of the row cluster, should be a \code{\link[grid]{unit}} object.}
  \item{show_row_hclust}{whether show row clusters.}
  \item{row_hclust_gp}{graphics parameters for drawing lines.}
  \item{cluster_columns}{whether make cluster on columns.}
  \item{clustering_distance_columns}{same setting as \code{clustering_distance_rows}.}
  \item{clustering_method_columns}{method to make cluster, pass to \code{\link[stats]{hclust}}.}
  \item{column_hclust_side}{should the column cluster be put on the top or bottom of the heatmap.}
  \item{column_hclust_height}{height of the column cluster, should be a \code{\link[grid]{unit}} object.}
  \item{show_column_hclust}{whether show column clusters.}
  \item{column_hclust_gp}{graphic parameters for drawling lines.}
  \item{row_names_side}{should the row names be put on the left or right of the heatmap.}
  \item{show_row_names}{whether show row names.}
  \item{row_names_max_width}{maximum width of row names viewport.}
  \item{row_names_gp}{graphic parameters for drawing text.}
  \item{column_names_side}{should the column names be put on the top or bottom of the heatmap.}
  \item{column_names_max_height}{maximum height of column names viewport.}
  \item{show_column_names}{whether show column names.}
  \item{column_names_gp}{graphic parameters for drawing text.}
  \item{top_annotation}{a \code{\link{HeatmapAnnotation}} object.}
  \item{top_annotation_height}{height.}
  \item{bottom_annotation}{a \code{\link{HeatmapAnnotation}} object.}
  \item{bottom_annotation_height}{height.}
  \item{km}{whether do k-means clustering on rows. }
  \item{gap}{gap between row-slice, should be \code{\link[grid]{unit}} object}
  \item{split}{a vector or a data frame by which the rows are splitted }
  \item{combined_name_fun}{if heatmap is splitted by rows, how to make a combined row title?}
  \item{width}{the width of the single heatmap.}

}
\details{
The initialization function only applies parameter checking. Clustering on rows can be applied by \code{\link{make_row_cluster,Heatmap-method}}; clustering on columns can be applied by \code{\link{make_column_cluster,Heatmap-method}} and layout can be constructed by \code{\link{make_layout,Heatmap-method}}. Basically, these three methods will be called when calling \code{\link{draw,Heatmap-method}} or \code{\link{draw,HeatmapList-method}}.  

If \code{km} or/and \code{split} are set, the clustering inside each row slice uses \code{clustering_method_rows} and \code{clustering_method_rows} as input parameters.  

Following methods can be applied on the \code{\link{Heatmap}} object:  

\itemize{
  \item \code{\link{show,Heatmap-method}}: draw a single heatmap with default parameters
  \item \code{\link{draw,Heatmap-method}}: draw a single heatmap.
  \item \code{\link{add_heatmap,Heatmap-method}} add heatmaps to a list of heatmaps.
}


}
\value{
A \code{\link{Heatmap}} object  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
