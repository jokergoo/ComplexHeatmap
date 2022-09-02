\name{oncoPrint}
\alias{oncoPrint}
\title{
Make oncoPrint
}
\description{
Make oncoPrint
}
\usage{
oncoPrint(mat, name,
    get_type = default_get_type,
    alter_fun,
    alter_fun_is_vectorized = NULL,
    col = NULL,
    
    top_annotation = HeatmapAnnotation(cbar = anno_oncoprint_barplot()),
    right_annotation = rowAnnotation(rbar = anno_oncoprint_barplot()),
    left_annotation = NULL,
    bottom_annotation = NULL,
    
    show_pct = TRUE,
    pct_gp = gpar(fontsize = 10),
    pct_digits = 0,
    pct_side = "left",
    pct_include = NULL,
    
    row_labels = NULL,
    show_row_names = TRUE,
    row_names_side = "right",
    row_names_gp = pct_gp,
    row_split = NULL,
    
    column_labels = NULL,
    column_names_gp = gpar(fontsize = 10),
    column_split = NULL,
    
    row_order = NULL,
    column_order = NULL,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    
    remove_empty_columns = FALSE,
    remove_empty_rows = FALSE,
    show_column_names = FALSE,
    heatmap_legend_param = NULL,
    ...)
}
\arguments{

  \item{mat}{The value should be a character matrix which encodes mulitple alterations  or a list of matrices for which every matrix contains binary value representing whether the alteration is present or absent.  When the value is a list, the names of the list represent alteration types. You can use \code{\link{unify_mat_list}} to make all matrix having same row names and column names.}
  \item{name}{Name of the oncoPrint. Not necessary to specify.}
  \item{get_type}{If different alterations are encoded in the matrix as complex strings, this self-defined function determines how to extract them. It only works when \code{mat} is a matrix. The default value is \code{\link{default_get_type}}.}
  \item{alter_fun}{A single function or a list of functions which defines how to add graphics for different alterations. You can use \code{\link{alter_graphic}} to automatically generate for rectangles and points.}
  \item{alter_fun_is_vectorized}{Whether \code{alter_fun} is implemented vectorized. Internally the function will guess.}
  \item{col}{A vector of color for which names correspond to alteration types.}
  \item{top_annotation}{Annotation put on top of the oncoPrint. By default it is barplot which shows the number of genes with a certain alteration in each sample.}
  \item{right_annotation}{Annotation put on the right of the oncoPrint. By default it is barplot which shows the number of samples with a certain alteration in each gene.}
  \item{left_annotation}{Annotation put on the left of the oncoPrint.}
  \item{bottom_annotation}{Annotation put at the bottom of the oncoPrint.}
  \item{show_pct}{whether show percent values on the left of the oncoprint?}
  \item{pct_gp}{Graphic paramters for percent values}
  \item{pct_digits}{Digits for the percent values.}
  \item{pct_side}{Side of the percent values to the oncoPrint. This argument is currently disabled.}
  \item{pct_include}{Alteration types that are included for the calculation of percent values.}
  \item{row_labels}{Labels as the row names of the oncoPrint.}
  \item{show_row_names}{Whether show row names?}
  \item{row_names_side}{Side of the row names to the oncoPrint. This argument is currently disabled.}
  \item{row_names_gp}{Graphic parameters for the row names.}
  \item{row_split}{Pass to \code{\link{Heatmap}}.}
  \item{column_labels}{Pass to \code{\link{Heatmap}}.}
  \item{column_names_gp}{Pass to \code{\link{Heatmap}}.}
  \item{column_split}{Pass to \code{\link{Heatmap}}.}
  \item{row_order}{Order of rows. By default rows are sorted by the number of occurence of the alterations.}
  \item{cluster_rows}{If it is set, it must be a dendrogram/hclust object.}
  \item{cluster_columns}{If it is set, it must be a dendrogram/hclust object.}
  \item{column_order}{Order of columns. By default the columns are sorted to show the mutual exclusivity of alterations.}
  \item{remove_empty_columns}{If there is no alteration in some samples, whether remove them on the oncoPrint?}
  \item{remove_empty_rows}{If there is no alteration in some samples, whether remove them on the oncoPrint?}
  \item{show_column_names}{Whether show column names?}
  \item{heatmap_legend_param}{pass to \code{\link{Heatmap}}.}
  \item{...}{Pass to \code{\link{Heatmap}}.}

}
\details{
The 'memo sort' method is from \url{https://gist.github.com/armish/564a65ab874a770e2c26} . Thanks to
B. Arman Aksoy for contributing the code.

\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/oncoprint.html} gives details for configuring
a oncoPrint.
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
