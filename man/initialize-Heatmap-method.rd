\name{initialize-Heatmap-method}
\alias{initialize,Heatmap-method}
\title{
constructor of \code{\link{Heatmap}} class  


}
\description{
constructor of \code{\link{Heatmap}} class  


}
\usage{
\S4method{initialize}{Heatmap}(.Object, matrix, col, name, rect_gp = gpar(col = NA),
    row_title = character(0), row_title_side = c("left", "right"), row_title_gp = gpar(fontsize = 14),
    column_title = character(0), column_title_side = c("top", "bottom"), column_title_gp = gpar(fontsize = 14),
    cluster_rows = TRUE, clustering_distance_rows = "euclidean", clustering_method_rows = "complete",
    row_hclust_side = c("left", "right"), row_hclust_width = unit(10, "mm"), show_row_hclust = TRUE, row_hclust_gp = gpar(),
    cluster_columns = TRUE, clustering_distance_columns = "euclidean", clustering_method_columns = "complete",
    column_hclust_side = c("top", "bottom"), column_hclust_height = unit(10, "mm"), show_column_hclust = TRUE, column_hclust_gp = gpar(),
    rownames_side = c("right", "left"), show_rownames = TRUE, rownames_gp = gpar(fontsize = 12),
    colnames_side = c("bottom", "top"), show_colnames = TRUE, colnames_gp = gpar(fontsize = 12),
    annotation = NULL, annotation_color = NULL, annotation_side = c("top", "bottom"),
    annotation_height = if(is.null(annotation)) unit(0, "null") else ncol(annotation)*unit(4, "mm"), annotation_gp = gpar(col = NA))
}
\arguments{

  \item{.Object}{object}
  \item{matrix}{matrix}
  \item{col}{color}
  \item{name}{name}
  \item{rect_gp}{graphic parameters for drawing rectangles}
  \item{row_title}{title on rows}
  \item{row_title_side}{side}
  \item{row_title_gp}{gp}
  \item{column_title}{foo}
  \item{column_title_side}{foo}
  \item{column_title_gp}{foo}
  \item{cluster_rows}{foo}
  \item{clustering_distance_rows}{foo}
  \item{clustering_method_rows}{foo}
  \item{row_hclust_side}{foo}
  \item{row_hclust_width}{foo}
  \item{show_row_hclust}{foo}
  \item{row_hclust_gp}{foo}
  \item{cluster_columns}{foo}
  \item{clustering_distance_columns}{foo}
  \item{clustering_method_columns}{foo}
  \item{column_hclust_side}{foo}
  \item{column_hclust_height}{foo}
  \item{show_column_hclust}{foo}
  \item{column_hclust_gp}{foo}
  \item{rownames_side}{foo}
  \item{show_rownames}{foo}
  \item{rownames_gp}{foo}
  \item{colnames_side}{foo}
  \item{show_colnames}{foo}
  \item{colnames_gp}{foo}
  \item{annotation}{foo}
  \item{annotation_color}{foo}
  \item{annotation_side}{foo}
  \item{annotation_height}{foo}
  \item{annotation_gp}{foo}

}
\value{
a \code{\link{Heatmap}} object  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
