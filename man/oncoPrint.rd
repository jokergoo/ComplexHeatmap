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
    show_column_names = FALSE,
    show_pct = TRUE, pct_gp = gpar(), pct_digits = 0,
    axis_gp = gpar(fontsize = 8),
    show_row_barplot = TRUE,
    row_barplot_width = unit(2, "cm"),
    remove_empty_columns = FALSE,
    heatmap_legend_param = list(title = "Alterations"),
    top_annotation = HeatmapAnnotation(column_bar = anno_column_bar,
    annotation_height = unit(2, "cm")),
    barplot_ignore = NULL,
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
  \item{show_column_names}{whether show column names}
  \item{show_pct}{whether show percent values on the left of the oncoprint}
  \item{pct_gp}{graphic paramters for percent row annotation}
  \item{pct_digits}{digits for percent values}
  \item{axis_gp}{graphic paramters for axes}
  \item{show_row_barplot}{whether show barplot annotation on rows}
  \item{row_barplot_width}{width of barplot annotation on rows. It should be a \code{\link[grid]{unit}} object}
  \item{remove_empty_columns}{if there is no alteration in that sample, whether remove it on the heatmap}
  \item{heatmap_legend_param}{pass to \code{\link{Heatmap}}}
  \item{top_annotation}{by default the top annotation contains barplots representing frequency of mutations in every sample.}
  \item{barplot_ignore}{alterations that you don't want to put on the barplots.}
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
