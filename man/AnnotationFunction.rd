\name{AnnotationFunction}
\alias{AnnotationFunction}
\title{
Constructor of AnnotationFunction Class
}
\description{
Constructor of AnnotationFunction Class
}
\usage{
AnnotationFunction(fun, fun_name = "", which = c("column", "row"),
    var_import = list(), n = 0, data_scale = c(0, 1), subset_rule = list(),
    subsetable = FALSE, width = NULL, height = NULL)
}
\arguments{

  \item{fun}{A function which defines how to draw this annotation. See **Details** section.}
  \item{fun_name}{The name of the function. It is only used for printing the object.}
  \item{which}{Whether it is drawn as a column annotation or a row annotation?}
  \item{var_import}{The names of the variables or the variable themselves that the annotation function depends on. See **Details** section.}
  \item{n}{Number of observations in the annotation. It is not mandatory, but it is better to provide this information so that the higher order \code{\link{HeatmapAnnotation}} knows it and it can perform check on the consistency of annotations and heatmaps.}
  \item{data_scale}{The data scale on the data axis (y-axis for column annotation and x-axis for row annotation). It is only used when \code{\link{decorate_annotation}} is used with "native" unit coordinates.}
  \item{subset_rule}{The rule of subsetting variables in \code{var_import}. It should be set when users want the final object to be subsetable. See **Details** section.}
  \item{subsetable}{Whether the object is subsetable?}
  \item{width}{The width of the plotting region (the viewport) that the annotation is drawn. If it is a row annotation, the width must be an absolute unit.}
  \item{height}{The height of the plotting region (the viewport) that the annotation is drawn. If it is a column annotation, the width must be an absolute unit.}

}
\details{
A normal R function defines how to draw the annotation graphics. As expected, the main part of the AnnotationFunction
class is this function. The function defines how to draw at specific positions which correspond to rows or columns
in the heatmap. The function should have three arguments: \code{index}, \code{k} and \code{n} (the names of the arguments can
be arbitory) where \code{k} and \code{n} are optional. \code{index} corresponds to the indices of rows or columns of the heatmap.
The value of \code{index} is not necessarily to be the whole row indices or column indices. It can also be a subset of
the indices if the annotation is split into slices according to the split of the heatmap. The value in \code{index} is
always sorted according to the reordering of heatmap rows or columns (e.g. by clustering). So, \code{index} actually contains
a list of row or column indices for the current slice after row or column reordering. This type of design makes sure
the annotation graphics are drawn at the correct positions and can be correctly corresponded to the heatmap rows or columns.

As mentioned, annotation can be split into slices. \code{k} corresponds to the current slice and \code{n} corresponds to
the total number of slices. The information of \code{k} and \code{n} sometimes can be useful, for example, we want to add axis
in the annotation, and if it is a column annotation and axis is drawn on the very right of the annotation area, the axis
is only drawn when \code{k == n}.

Since the function only allows \code{index}, \code{k} and \code{n}, the function sometimes uses several external variables which can
not be defined inside the function, e.g. the data points for the annotation. These variables should be imported
into the AnnotationFunction class so that the function can correctly find these variables.

One important feature for AnnotationFunction class is it can be subsetable. To allow subsetting of the object,
users need to define the rule for the imported variables if there is any. The rules are simple function which
accpets the variable and indices, and returns the subset of the variable. The subset rule functions implemented
in this package are \code{\link{subset_gp}}, \code{\link{subset_matrix_by_row}} and \code{\link{subset_vector}}. These three functions are enough for
most of the cases.

In following, we defined three AnnotationFunction objects:

1. It needs external variable and support subsetting

x = 1:10
anno1 = AnnotationFunction(
	fun = function(index) {
		n = length(index)
		pushViewport(viewport())
		grid.points(1:n, x[index])
		popViewport()
	},
	var_imported = list(x = x),
	n = 10,
	subset_rule = list(x = subset_vector),
	subsetable = TRUE
)

2. The data variable is defined inside the function and no need to import other variables.

anno2 = AnnotationFunction(
	fun = function(index) {
		x = 1:10
		n = length(index)
		pushViewport(viewport())
		grid.points(1:n, x[index])
		popViewport()
	},
	n = 10,
	subsetable = TRUE
)

3. The most compact way to only specify the function to the constructor.

anno3 = AnnotationFunction(
	fun = function(index) {
		x = 1:10
		n = length(index)
		pushViewport(viewport())
		grid.points(1:n, x[index])
		popViewport()
	}
)

Finally, you need to push a viewport for graphics and finally pop the viewport.

In the package, quite a lot annotation function are constructed by \code{\link{AnnotationFunction}} constructor:
\code{\link{anno_empty}}, \code{\link{anno_image}}, \code{\link{anno_points}}, \code{\link{anno_lines}}, \code{\link{anno_barplot}}, \code{\link{anno_boxplot}}, \code{\link{anno_histogram}},
\code{\link{anno_density}}, \code{\link{anno_joyplot}}, \code{\link{anno_horizon}}, \code{\link{anno_text}} and \code{\link{anno_mark}}. Thess built-in annotation functions
are all subsettable.
}
\examples{
# There is no example
NULL

}
