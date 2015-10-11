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
    alter_fun_list, col,
    row_order = oncoprint_row_order(),
    column_order = oncoprint_column_order(),
    show_column_names = FALSE,
    pct_gp = gpar(),
    axis_gp = gpar(fontsize = 8),
    show_row_barplot = TRUE,
    row_barplot_width = unit(2, "cm"),
    show_column_barplot = TRUE,
    column_barplot_height = unit(2, "cm"),
    remove_empty_columns = FALSE,
    heatmap_legend_param = list(title = "Alterations"),
    ...)
}
\arguments{

  \item{mat}{a character matrix which encodes mulitple alterations or a list of matrix for which every matrix contains binaryvalue representing the alteration is present or absent. When it is a list, the names represent alteration types.You can use \code{\link{unify_mat_list}} to make all matrix having same row names and column names.}
  \item{get_type}{If different alterations are encoded in the matrix, this self-defined functiondetermines how to extract them. Only work when \code{mat} is a matrix.}
  \item{alter_fun_list}{a list of functions which define how to add graphics for different alterations.The names of the list should cover all alteration types.}
  \item{col}{a vector of color for which names correspond to alteration types.}
  \item{row_order}{order of genes. By default it is sorted by frequency of alterations decreasingly.Set it to \code{NULL} if you don't want to set the order}
  \item{column_order}{order of samples. By default the order is calculated by the 'memo sort' method which can visualizethe mutual exclusivity across genes.}
  \item{show_column_names}{whether show column names}
  \item{pct_gp}{graphic paramters for percent row annotation}
  \item{axis_gp}{graphic paramters for axes}
  \item{show_row_barplot}{whether show barplot annotation on rows}
  \item{row_barplot_width}{width of barplot annotation on rows. It should be a \code{\link[grid]{unit}} object}
  \item{show_column_barplot}{whether show barplot annotation on columns}
  \item{column_barplot_height}{height of barplot annotatioin on columns. it should be a \code{\link[grid]{unit}} object.}
  \item{remove_empty_columns}{if there is no alteration in that sample, whether remove it on the heatmap}
  \item{heatmap_legend_param}{pass to \code{\link{Heatmap}}}
  \item{...}{pass to \code{\link{Heatmap}}}

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
