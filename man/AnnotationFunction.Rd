\name{AnnotationFunction}
\alias{AnnotationFunction}
\title{
Constructor of AnnotationFunction Class
}
\description{
Constructor of AnnotationFunction Class
}
\usage{
AnnotationFunction(fun, fun_name = "", which = c("column", "row"), cell_fun = NULL,
    var_import = list(), n = NA, data_scale = c(0, 1), subset_rule = list(),
    subsettable = length(subset_rule) > 0, show_name = TRUE, width = NULL, height = NULL)
}
\arguments{

  \item{fun}{A function which defines how to draw the annotation. See **Details** section.}
  \item{fun_name}{The name of the function. It is only used for printing the object.}
  \item{which}{Whether it is drawn as a column annotation or a row annotation?}
  \item{cell_fun}{A simplified version of \code{fun}. \code{cell_fun} only accepts one single index and it draws repeatedly in each annotation cell.}
  \item{var_import}{The names of the variables or the variable themselves that the annotation function depends on. See **Details** section.}
  \item{n}{Number of observations in the annotation. It is not mandatory, but it is better to provide this information so that the higher order \code{\link{HeatmapAnnotation}} knows it and it can perform check on the consistency of annotations and heatmaps.}
  \item{data_scale}{The data scale on the data axis (y-axis for column annotation and x-axis for row annotation). It is only used when \code{\link{decorate_annotation}} is used with "native" unit coordinates.}
  \item{subset_rule}{The rule of subsetting variables in \code{var_import}. It should be set when users want the final object to be subsettable. See **Details** section.}
  \item{subsettable}{Whether the object is subsettable?}
  \item{show_name}{It is used to turn off the drawing of annotation names in \code{\link{HeatmapAnnotation}}. Annotations always have names associated and normally they will be drawn beside the annotation graphics to tell what the annotation is about. e.g. the annotation names put beside the points annotation graphics. However, for some of the annotations, the names are not necessarily to be drawn, such as text annotations drawn by \code{\link{anno_text}} or an empty annotation drawn by \code{\link{anno_empty}}. In this case, when \code{show_names} is set to \code{FALSE}, there will be no annotation names drawn for the annotation.}
  \item{width}{The width of the plotting region (the viewport) that the annotation is drawn. If it is a row annotation, the width must be an absolute unit. Since the \code{AnnotationFunction} object is always contained by the \code{\link{SingleAnnotation-class}}object, you can only set the width of row annotations or height of column annotations, while e.g. the height of the row annotation is always \code{unit(1, "npc")} which means it always fully filled in the parent \code{SingleAnnotation} and only in \code{\link{SingleAnnotation}} or even \code{\link{HeatmapAnnotation}} can adjust the height of the row annotations.}
  \item{height}{The height of the plotting region (the viewport) that the annotation is drawn. If it is a column annotation, the width must be an absolute unit.}

}
\details{
In the package, we have implemted quite a lot annotation functions by \code{\link{AnnotationFunction}} constructor:
\code{\link{anno_empty}}, \code{\link{anno_image}}, \code{\link{anno_points}}, \code{\link{anno_lines}}, \code{\link{anno_barplot}}, \code{\link{anno_boxplot}}, \code{\link{anno_histogram}},
\code{\link{anno_density}}, \code{\link{anno_joyplot}}, \code{\link{anno_horizon}}, \code{\link{anno_text}} and \code{\link{anno_mark}}. These built-in annotation functions
support as both row annotations and column annotations and they are are all subsettable.

The build-in annotation functions are already enough for most of the analysis, nevertheless, if users
want to know more about how to construct the AnnotationFunction class manually, they can refer to
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#implement-new-annotation-functions.}
}
\value{
A \code{\link{AnnotationFunction-class}} object which can be used in \code{\link{HeatmapAnnotation}}.
}
\examples{
x = 1:10
anno1 = AnnotationFunction(
    fun = function(index, k, n) {
        n = length(index)
        pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(0, 10)))
        grid.rect()
        grid.points(1:n, x[index], default.units = "native")
        if(k == 1) grid.yaxis()
        popViewport()
    },
    var_import = list(x = x),
    n = 10,
    subsettable = TRUE,
    height = unit(2, "cm")
)
m = rbind(1:10, 11:20)
Heatmap(m, top_annotation = HeatmapAnnotation(foo = anno1))
Heatmap(m, top_annotation = HeatmapAnnotation(foo = anno1), column_km = 2)
}
