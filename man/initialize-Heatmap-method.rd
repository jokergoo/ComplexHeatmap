\name{initialize-Heatmap-method}
\alias{initialize,Heatmap-method}
\title{
Constructor method of Heatmap class  


}
\description{
Constructor method of Heatmap class  


}
\usage{
\S4method{initialize}{Heatmap}(.Object, matrix, col, name, rect_gp = gpar(col = NA),
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
    row_names_side = c("right", "left"), show_row_names = TRUE,
    row_names_gp = gpar(fontsize = 12), column_names_side = c("bottom", "top"),
    show_column_names = TRUE, column_names_gp = gpar(fontsize = 12),
    annotation = NULL, annotation_color = NULL, annotation_side = c("top", "bottom"),
    annotation_height = if(is.null(annotation)) unit(0, "null") else ncol(annotation)*unit(4, "mm"),
    annotation_gp = gpar(col = NA), km = 1, gap = unit(1, "mm"), split = NULL, width = NULL)
}
\arguments{

  \item{.Object}{private object.}
  \item{matrix}{matrix. Either numeric or character. If it is a simple vector, it will be converted to a one-column matrix.}
  \item{col}{a vector of colors if the matrix is character or a color mapping  function if the matrix is numeric. Pass to \code{\link{initialize,ColorMapping-method}}.}
  \item{name}{name of the heatmap.}
  \item{rect_gp}{graphic parameters for drawing rectangles (for heatmap body).}
  \item{row_title}{title on row.}
  \item{row_title_side}{will the title be put on the left or right of the heatmap.}
  \item{row_title_gp}{graphic parameters for drawing text.}
  \item{column_title}{title on column.}
  \item{column_title_side}{will the title be put on the top or bottom of the heatmap.}
  \item{column_title_gp}{graphic parameters for drawing text.}
  \item{cluster_rows}{whether make cluster on rows.}
  \item{clustering_distance_rows}{it can be a pre-defined character which is in  ("euclidean", "maximum", "manhattan", "canberra", "binary",  "minkowski", "pearson", "spearman", "kendall"). It can also be a function. If the function has one argument, the input argument should be a matrix and  the returned value should be a \code{\link[stats]{dist}} object. If the function has two arguments, the input arguments are two vectors and the function calcualtes distance between these two vectors.}
  \item{clustering_method_rows}{method to make cluster, pass to \code{\link[stats]{hclust}}.}
  \item{row_hclust_side}{should the row cluster be put on the left or right of the heatmap.}
  \item{row_hclust_width}{width of the row cluster, should be a \code{\link[grid]{unit}} object.}
  \item{show_row_hclust}{whether show row clusters.}
  \item{row_hclust_gp}{graphics parameters for drawing lines.}
  \item{cluster_columns}{whether make cluster on columns.}
  \item{clustering_distance_columns}{same setting as \code{clustering_distance_rows}.}
  \item{clustering_method_columns}{method to make cluster, pass to \code{\link[stats]{hclust}}.}
  \item{column_hclust_side}{should the column cluster be put on the top or bottom of the heatmap.}
  \item{column_hclust_height}{height of the column cluster, should be a \code{\link[grid]{unit}} object.}
  \item{show_column_hclust}{whether show column clusters.}
  \item{column_hclust_gp}{graphic parameters for drawling lines.}
  \item{row_names_side}{should the row names be put on the left or right of the heatmap.}
  \item{show_row_names}{whether show row names.}
  \item{row_names_gp}{graphic parameters for drawing text.}
  \item{column_names_side}{should the column names be put on the top or bottom of the heatmap.}
  \item{show_column_names}{whether show column names.}
  \item{column_names_gp}{graphic parameters for drawing text.}
  \item{annotation}{column annotation. The value should be a data frame for which row_names correspond to column names of the matrix and columns correspond to different annotations.}
  \item{annotation_color}{colors for the annotations. The value is a list in which each element corresponds to each annotation and the value for each element is a vector or a color mapping function which depends on the annotation is discrete or continuous.}
  \item{annotation_side}{should the annotaitons be put on the top or bottom of the heatmap.}
  \item{annotation_height}{height of the annotations, should be a \code{\link[grid]{unit}} object.}
  \item{annotation_gp}{graphic parameters for drawing rectangles.}

}
\details{
Following methods can be applied on the \code{\link{Heatmap}} object:  

\itemize{
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
