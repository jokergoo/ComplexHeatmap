\name{oncoPrint}
\alias{oncoPrint}
\title{
Make oncoPrint
}
\description{
Make oncoPrint
}
\usage{
oncoPrint(mat, get_type = function(x) x,
    alter_fun = alter_fun_list, alter_fun_list = NULL, col,
    row_order = oncoprint_row_order(),
    column_order = oncoprint_column_order(),
<<<<<<< HEAD
    show_pct = TRUE, pct_gp = row_names_gp, pct_digits = 0,
=======
    show_column_names = FALSE,
    show_pct = TRUE, pct_gp = gpar(), pct_digits = 0,
>>>>>>> bioc/master
    axis_gp = gpar(fontsize = 8),
    show_row_barplot = TRUE,
    row_barplot_width = unit(2, "cm"),
    remove_empty_columns = FALSE,
    heatmap_legend_param = list(title = "Alterations"),
    top_annotation = HeatmapAnnotation(column_bar = anno_oncoprint_barplot(),
    annotation_height = unit(2, "cm")),
<<<<<<< HEAD
    top_annotation_height = top_annotation@size,
    bottom_annotation = new("HeatmapAnnotation"),
    bottom_annotation_height = bottom_annotation@size,
    barplot_ignore = NULL,
    row_title = character(0),
    row_title_side = c("left", "right"),
    row_title_gp = gpar(fontsize = 14),
    row_title_rot = switch(row_title_side[1], "left" = 90, "right" = 270),
    column_title = character(0),
    column_title_side = c("top", "bottom"),
    column_title_gp = gpar(fontsize = 14),
    column_title_rot = 0,
    show_row_names = TRUE,
    row_names_gp = gpar(fontsize = 12),
    show_column_names = FALSE,
    column_names_gp = gpar(fontsize = 12),
    split = NULL,
    gap = unit(1, "mm"),
    combined_name_fun = function(x) paste(x, collapse = "/"),
    width = NULL,
=======
    barplot_ignore = NULL,
>>>>>>> bioc/master
    ...)
}
\arguments{

  \item{mat}{a character matrix which encodes mulitple alterations or a list of matrix for which every matrix contains binary value representing the alteration is present or absent. When it is a list, the names represent alteration types. You can use \code{\link{unify_mat_list}} to make all matrix having same row names and column names.}
  \item{get_type}{If different alterations are encoded in the matrix, this self-defined function determines how to extract them. Only work when \code{mat} is a matrix.}
  \item{alter_fun}{a single function or a list of functions which define how to add graphics for different alterations. If it is a list, the names of the list should cover all alteration types.}
  \item{alter_fun_list}{deprecated, use \code{alter_run} instead.}
  \item{col}{a vector of color for which names correspond to alteration types.}
  \item{row_order}{order of genes. By default it is sorted by frequency of alterations decreasingly. Set it to \code{NULL} if you don't want to set the order}
  \item{column_order}{order of samples. By default the order is calculated by the 'memo sort' method which can visualize the mutual exclusivity across genes. Set it to \code{NULL} if you don't want to set the order}
<<<<<<< HEAD
=======
  \item{show_column_names}{whether show column names}
>>>>>>> bioc/master
  \item{show_pct}{whether show percent values on the left of the oncoprint}
  \item{pct_gp}{graphic paramters for percent row annotation}
  \item{pct_digits}{digits for percent values}
  \item{axis_gp}{graphic paramters for axes}
  \item{show_row_barplot}{whether show barplot annotation on rows}
  \item{row_barplot_width}{width of barplot annotation on rows. It should be a \code{\link[grid]{unit}} object}
  \item{remove_empty_columns}{if there is no alteration in that sample, whether remove it on the heatmap}
  \item{heatmap_legend_param}{pass to \code{\link{Heatmap}}}
  \item{top_annotation}{by default the top annotation contains barplots representing frequency of mutations in every sample.}
<<<<<<< HEAD
  \item{top_annotation_height}{total height of the column annotations on the top.}
  \item{bottom_annotation}{a \code{\link{HeatmapAnnotation}} object.}
  \item{bottom_annotation_height}{total height of the column annotations on the bottom.}
  \item{barplot_ignore}{specific alterations that you don't want to put on the barplots. If you want to really suppress the top barplot set \code{top_annotation} to \code{NULL}.}
  \item{row_title}{title on row.}
  \item{row_title_side}{will the title be put on the left or right of the heatmap?}
  \item{row_title_gp}{graphic parameters for drawing text.}
  \item{row_title_rot}{rotation of row titles. Only 0, 90, 270 are allowed to set.}
  \item{column_title}{title on column.}
  \item{column_title_side}{will the title be put on the top or bottom of the heatmap?}
  \item{column_title_gp}{graphic parameters for drawing text.}
  \item{column_title_rot}{rotation of column titles. Only 0, 90, 270 are allowed to set.}
  \item{show_row_names}{whether show row names.}
  \item{row_names_gp}{graphic parameters for drawing text.}
  \item{show_column_names}{whether show column names.}
  \item{column_names_gp}{graphic parameters for drawing text.}
  \item{split}{a vector or a data frame by which the rows are split. But if \code{cluster_rows} is a clustering object, \code{split} can be a single number indicating rows are to be split according to the split on the tree.}
  \item{gap}{gap between row-slices if the heatmap is split by rows, should be \code{\link[grid]{unit}} object. If it is a vector, the order corresponds to top to bottom in the heatmap}
  \item{combined_name_fun}{if the heatmap is split by rows, how to make a combined row title for each slice? The input parameter for this function is a vector which contains level names under each column in \code{split}.}
  \item{width}{the width of the single heatmap, should be a fixed \code{\link[grid]{unit}} object. It is used for the layout when the heatmap is appended to a list of heatmaps.}
=======
  \item{barplot_ignore}{specific alterations that you don't want to put on the barplots. If you want to really suppress the top barplot set \code{top_annotation} to \code{NULL}.}
>>>>>>> bioc/master
  \item{...}{pass to \code{\link{Heatmap}}, so can set \code{bottom_annotation} here.}

}
\details{
The function returns a normal heatmap list and you can add more heatmaps/row annotations to it.

The 'memo sort' method is from \url{https://gist.github.com/armish/564a65ab874a770e2c26} . Thanks to
B. Arman Aksoy for contributing the code.

The function would be a little bit slow if you plot it in an interactive device because all alterations
are added through a foo loop.

For more explanation, please go to the vignette.
}
\value{
A \code{\link{HeatmapList-class}} object which means you can add other heatmaps or row annotations to it.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
