\name{SingleAnnotation}
\alias{SingleAnnotation}
\title{
Constructor Method for SingleAnnotation Class
}
\description{
Constructor Method for SingleAnnotation Class
}
\usage{
SingleAnnotation(name, value, col, fun,
    label = NULL,
    na_col = "grey",
    which = c("column", "row"),
    show_legend = TRUE,
    gp = gpar(col = NA),
    border = FALSE,
    legend_param = list(),
    show_name = TRUE,
    name_gp = gpar(fontsize = 12),
    name_offset = NULL,
    name_side = ifelse(which == "column", "right", "bottom"),
    name_rot = NULL,
    simple_anno_size = ht_opt$simple_anno_size,
    width = NULL, height = NULL)
}
\arguments{

  \item{name}{Name for the annotation. If it is not specified, an internal name is assigned.}
  \item{value}{A vector or a matrix of discrete or continuous values.}
  \item{col}{Colors corresponding to \code{value}. If the mapping is discrete, the value of \code{col} should be a named vector; If the mapping is continuous, the value of \code{col} should be  a color mapping function.}
  \item{fun}{A user-defined function to add annotation graphics. The argument of this function should be at least  a vector of index that corresponds to rows or columns. Normally the function should be  constructed by \code{\link{AnnotationFunction}} if you want the annotation supports splitting.  See **Details** for more explanation.}
  \item{label}{Label for the annotation. By default is the annotation name.}
  \item{na_col}{Color for \code{NA} values in the simple annotations.}
  \item{which}{Whether the annotation is a row annotation or a column annotation?}
  \item{show_legend}{If it is a simple annotation, whether show legend in the final heatmap?}
  \item{gp}{Since simple annotation is represented as rows of grids. This argument controls graphic parameters for the simple annotation. The \code{fill} parameter is ignored here.}
  \item{border}{border, only work for simple annotation}
  \item{legend_param}{Parameters for the legend. See \code{\link{color_mapping_legend,ColorMapping-method}} for all possible options.}
  \item{show_name}{Whether show annotation name?}
  \item{name_gp}{Graphic parameters for annotation name.}
  \item{name_offset}{Offset to the annotation, a \code{\link[grid]{unit}} object.}
  \item{name_side}{'right' and 'left' for column annotations and 'top' and 'bottom' for row annotations}
  \item{name_rot}{Rotation of the annotation name.}
  \item{simple_anno_size}{size of the simple annotation.}
  \item{width}{The width of the plotting region (the viewport) that the annotation is drawn. If it is a row annotation, the width must be an absolute unit.}
  \item{height}{The height of the plotting region (the viewport) that the annotation is drawn. If it is a column annotation, the width must be an absolute unit.}

}
\details{
A single annotation is a basic unit of complex heatmap annotations where the heamtap annotations
are always a list of single annotations. An annotation can be simply heatmap-like (here we call
it simple annotation) or more complex like points, lines, boxes (for which we call it complex annotation).

In the \code{\link{SingleAnnotation}} constructor, \code{value}, \code{col}, \code{na_col} are used to construct a \code{\link{anno_simple}}
annotation funciton which is generated internally by \code{\link{AnnotationFunction}}. The legend of the simple annotation
can be automatcally generated,

For construcing a complex annotation, users need to use \code{fun} which is a user-defind function. Normally it 
is constucted by \code{\link{AnnotationFunction}}. One big advantage for using \code{\link{AnnotationFunction}} is the annotation function
or the graphics drawn by the annotation function can be split according to row splitting or column splitting of
the heatmap. Users can also provide a "pure" function which is a normal R function for the \code{fun} argument. 
The function only needs one argument which is a vector of index for rows or columns depending whether it is 
a row annotation or column annotation. The other two optional arguments are the current slice index and total
number of slices. See **Examples** section for an example. If it is a normal R function, it will be constructed
into the \code{\link{AnnotationFunction-class}} object internally.

The \code{\link{SingleAnnotation-class}} is a simple wrapper on top of \code{\link{AnnotationFunction-class}} only with annotation 
name added.

The class also stored the "extended area" relative to the area for the annotation graphics. The extended areas
are those created by annotation names and axes.
}
\seealso{
There are following built-in annotation functions that can be directly used to generate complex annotations: 
\code{\link{anno_simple}}, \code{\link{anno_points}}, \code{\link{anno_lines}}, \code{\link{anno_barplot}}, \code{\link{anno_histogram}}, \code{\link{anno_boxplot}}, \code{\link{anno_density}}, \code{\link{anno_text}},
\code{\link{anno_joyplot}}, \code{\link{anno_horizon}}, \code{\link{anno_image}}, \code{\link{anno_block}}, \code{\link{anno_summary}} and \code{\link{anno_mark}}.
}
\value{
A \code{\link{SingleAnnotation-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
ha = SingleAnnotation(value = 1:10)
draw(ha, test = "single column annotation")

m = cbind(1:10, 10:1)
colnames(m) = c("a", "b")
ha = SingleAnnotation(value = m)
draw(ha, test = "matrix as column annotation")

anno = anno_barplot(matrix(nc = 2, c(1:10, 10:1)))
ha = SingleAnnotation(fun = anno)
draw(ha, test = "anno_barplot as input")

fun = local({
    # because there variables outside the function for use, we put it a local environment
    value = 1:10
    function(index, k = 1, n = 1) {
        pushViewport(viewport(xscale = c(0.5, length(index) + 0.5), yscale = range(value)))
        grid.points(seq_along(index), value[index])
        grid.rect()
        if(k == 1) grid.yaxis()
        popViewport()
    }
})
ha = SingleAnnotation(fun = fun, height = unit(4, "cm"))
draw(ha, index = 1:10, test = "self-defined function")
}
