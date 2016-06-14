\name{SingleAnnotation}
\alias{SingleAnnotation}
\title{
Constructor method for SingleAnnotation class
}
\description{
Constructor method for SingleAnnotation class
}
\usage{
SingleAnnotation(name, value, col, fun,
    na_col = "grey",
    which = c("column", "row"),
    show_legend = TRUE,
    gp = gpar(col = NA),
    legend_param = list(),
    show_name = FALSE,
    name_gp = gpar(fontsize = 12),
    name_offset = unit(2, "mm"),
    name_side = ifelse(which == "column", "right", "bottom"),
    name_rot = ifelse(which == "column", 0, 90))
}
\arguments{

  \item{name}{name for this annotation. If it is not specified, an internal name is assigned.}
  \item{value}{A vector of discrete or continuous annotation.}
  \item{col}{colors corresponding to \code{value}. If the mapping is discrete mapping, the value of \code{col} should be a vector; If the mapping is continuous mapping, the value of \code{col} should be  a color mapping function. }
  \item{fun}{a self-defined function to add annotation graphics. The argument of this function should only  be a vector of index that corresponds to rows or columns.}
  \item{na_col}{color for \code{NA} values in simple annotations.}
  \item{which}{is the annotation a row annotation or a column annotation?}
  \item{show_legend}{if it is a simple annotation, whether show legend when making the complete heatmap.}
  \item{gp}{Since simple annotation is represented as a row of grids. This argument controls graphic parameters for the simple annotation.}
  \item{legend_param}{parameters for the legend. See \code{\link{color_mapping_legend,ColorMapping-method}} for options.}
  \item{show_name}{whether show annotation name}
  \item{name_gp}{graphic parameters for annotation name}
  \item{name_offset}{offset to the annotation, a \code{\link[grid]{unit}} object}
  \item{name_side}{'right' and 'left' for column annotations and 'top' and 'bottom' for row annotations}
  \item{name_rot}{rotation of the annotation name, can only take values in \code{c(00, 90, 180, 270)}.}

}
\details{
The most simple annotation is one row or one column grids in which different colors
represent different classes of the data. Here the function use \code{\link{ColorMapping-class}}
to process such simple annotation. \code{value} and \code{col} arguments controls values and colors
of the simple annotation and a \code{\link{ColorMapping-class}} object will be constructed based on \code{value} and \code{col}.

\code{fun} is used to construct a more complex annotation. Users can add any type of annotation graphics
by implementing a function. The only input argument of \code{fun} is a index
of rows or columns which is already adjusted by the clustering. In the package, there are already
several annotation graphic function generators: \code{\link{anno_points}}, \code{\link{anno_histogram}} and \code{\link{anno_boxplot}}.

In the case that row annotations are splitted by rows, \code{index} corresponding to row orders in each row-slice
and \code{fun} will be applied on each of the row slices.

One thing that users should be careful is the difference of coordinates when the annotation is a row
annotation or a column annotation.
}
\seealso{
There are following built-in annotation functions that can be used to generate complex annotations: 
\code{\link{anno_points}}, \code{\link{anno_barplot}}, \code{\link{anno_histogram}}, \code{\link{anno_boxplot}}, \code{\link{anno_density}}, \code{\link{anno_text}} and \code{\link{anno_link}}.
}
\value{
A \code{\link{SingleAnnotation-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# discrete character
SingleAnnotation(name = "test", value = c("a", "a", "a", "b", "b", "b"))
SingleAnnotation(name = "test", value = c("a", "a", "a", "b", "b", "b"), 
    which = "row")

# with defined colors
SingleAnnotation(value = c("a", "a", "a", "b", "b", "b"), 
    col = c("a" = "red", "b" = "blue"))

# continuous numbers
require(circlize)
SingleAnnotation(value = 1:10)
SingleAnnotation(value = 1:10, col = colorRamp2(c(1, 10), c("blue", "red")))

# self-defined graphic function
SingleAnnotation(fun = anno_points(1:10))

}
