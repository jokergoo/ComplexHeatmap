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
    
    row_order = NULL,
    column_order = NULL,
    
    remove_empty_columns = FALSE,
    remove_empty_rows = FALSE,
    show_column_names = FALSE,
    heatmap_legend_param = list(title = "Alterations"),
    ...)
}
\arguments{

  \item{mat}{a character matrix which encodes mulitple alterations or a list of matrix for which every matrix contains binary value representing the alteration is present or absent. When it is a list, the names of the list represent alteration types. You can use \code{\link{unify_mat_list}} to make all matrix having same row names and column names.}
  \item{get_type}{If different alterations are encoded in the matrix, this self-defined function determines how to extract them. Only work when \code{mat} is a matrix.}
  \item{alter_fun}{a single function or a list of functions which define how to add graphics for different alterations. If it is a list, the names of the list should cover all alteration types.}
  \item{alter_fun_is_vectorized}{Whether \code{alter_fun} is implemented vectorized. Internally the function will guess.}
  \item{col}{a vector of color for which names correspond to alteration types.}
  \item{top_annotation}{Annotation put on top of the oncoPrint. By default it is barplot which shows the number of genes having the alteration in each sample.}
  \item{right_annotation}{Annotation put on the right of hte oncoPrint. By default it is barplto which shows the number of samples having the alteration in each gene.}
  \item{show_pct}{whether show percent values on the left of the oncoprint}
  \item{pct_gp}{graphic paramters for percent row annotation}
  \item{pct_digits}{digits for percent values}
  \item{pct_side}{side of pct}
  \item{show_row_names}{Whether show row names?}
  \item{row_names_side}{side of the row names}
  \item{row_names_gp}{Graphic parameters of row names.}
  \item{row_order}{row order}
  \item{column_order}{column order}
  \item{remove_empty_columns}{if there is no alteration in that sample, whether remove it on the heatmap}
  \item{remove_empty_rows}{if there is no alteration in that sample, whether remove it on the heatmap}
  \item{show_column_names}{Whether show column names?}
  \item{heatmap_legend_param}{pass to \code{\link{Heatmap}}}
  \item{...}{pass to \code{\link{Heatmap}}, so can set \code{bottom_annotation} here.}

}
\details{
The 'memo sort' method is from \url{https://gist.github.com/armish/564a65ab874a770e2c26} . Thanks to
B. Arman Aksoy for contributing the code.

For more explanation, please go to the vignette.
}
\value{
A \code{\link{Heatmap-class}} object which means you can add other heatmaps or annotations to it.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL
}
