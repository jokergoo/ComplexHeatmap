\name{oncoPrint}
\alias{oncoPrint}
\title{
Make oncoPrint
}
\description{
Make oncoPrint
}
\usage{
oncoPrint(mat,
    get_type = function(x) x,
    alter_fun,
    alter_fun_is_vectorized = NULL,
    col,
    
    top_annotation = HeatmapAnnotation(column_barplot = anno_oncoprint_barplot(),
    show_annotation_name = FALSE),
    right_annotation = rowAnnotation(row_barplot = anno_oncoprint_barplot(
    axis_param = list(side = "top", labels_rot = 0)),
    show_annotation_name = FALSE),
    
    show_pct = TRUE,
    pct_gp = gpar(fontsize = 10),
    pct_digits = 0,
    pct_side = "left",
    show_row_names = TRUE,
    row_names_side = "right",
    row_names_gp = pct_gp,
    
    remove_empty_columns = FALSE,
    remove_empty_rows = FALSE,
    show_column_names = FALSE,
    heatmap_legend_param = list(title = "Alterations"),
    ...)
}
\arguments{

  \item{mat}{a character matrix which encodes mulitple alterations or a list of matrix for which every matrix contains binary value representing the alteration is present or absent. When it is a list, the names represent alteration types. You can use \code{\link{unify_mat_list}} to make all matrix having same row names and column names.}
  \item{get_type}{If different alterations are encoded in the matrix, this self-defined function determines how to extract them. Only work when \code{mat} is a matrix.}
  \item{alter_fun}{a single function or a list of functions which define how to add graphics for different alterations. If it is a list, the names of the list should cover all alteration types.}
  \item{alter_fun_is_vectorized}{-alter_fun_is_vectorized}
  \item{col}{a vector of color for which names correspond to alteration types.}
  \item{top_annotation}{-top_annotation}
  \item{right_annotation}{-right_annotation}
  \item{show_pct}{whether show percent values on the left of the oncoprint}
  \item{pct_gp}{graphic paramters for percent row annotation}
  \item{pct_digits}{digits for percent values}
  \item{pct_side}{side of pct}
  \item{show_row_names}{-show_row_names}
  \item{row_names_side}{-row_names_side}
  \item{row_names_gp}{-row_names_gp}
  \item{remove_empty_columns}{if there is no alteration in that sample, whether remove it on the heatmap}
  \item{remove_empty_rows}{if there is no alteration in that sample, whether remove it on the heatmap}
  \item{show_column_names}{-show_column_names}
  \item{heatmap_legend_param}{pass to \code{\link{Heatmap}}}
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
