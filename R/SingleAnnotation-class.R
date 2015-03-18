

# == title 
# Class for a single annotation
#
# == details
# A complex heatmap always has more than one annotations on rows and columns. Here
# the `SingleAnnotation-class` defines the basic unit of annotations.
# The most simple annotation is one row or one column grids in which different colors
# represent different classes of the data. The annotation can also be more complex
# graphics, such as a boxplot that shows data distribution in corresponding row or column.
#
# The `SingleAnnotation-class` is used for storing data for a single annotation and provides
# methods for drawing annotation graphics.
#
# == methods
# The `SingleAnnotation-class` provides following methods:
#
# - `SingleAnnotation`: constructor method
# - `draw,SingleAnnotation-method`: draw the single annotation.
#
# == seealso
# The `SingleAnnotation-class` is always used internally. The public `HeatmapAnnotation-class`
# contains a list of `SingleAnnotation-class` objects and is used to add annotation graphics on heatmaps.
# 
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
SingleAnnotation = setClass("SingleAnnotation",
	slots = list(
		name = "character",
		color_mapping = "ANY",
		fun = "function",
		show_legend = "logical",
		which = "character"
	),
	prototype = list(
		color_mapping = NULL,
		fun = function(index) NULL,
		show = TRUE
	)
)

# == title
# Constructor method for SingleAnnotation class
#
# == param
# -name name for this annotation.
# -value A vector of annotation.
# -col colors corresponding to ``value``. If the mapping is discrete mapping, the value of ``col``
#      should be a vector; If the mapping is continuous mapping, the value of ``col`` should be 
#      a color mapping function. 
# -fun a self-defined function to add annotation graphics. The argument of this function should only 
#      be a vector of index that corresponds to rows or columns.
# -which is the annotation a row annotation or a column annotation?
# -show_legend if it is a simple annotation, whether show legend when making the complete heatmap.
# -gp graphic parameters for simple annotations.
#
# == details
# The most simple annotation is one row or one column grids in which different colors
# represent different classes of the data. Here the function use `ColorMapping-class`
# to process such simple annotation. ``value`` and ``col`` arguments controls values and colors
# of the simple annotation and a `ColorMapping-class` object will be constructed based on ``value`` and ``col``.
#
# ``fun`` is used to construct a more complex annotation. Users can add any type of annotation graphics
# by implementing a function. The only input argument of ``fun`` is a index
# of rows or columns which is already adjusted by the clustering. In the package, there are already
# several annotation graphic function generators: `anno_points`, `anno_histogram` and `anno_boxplot`.
#
# One thing that users should be careful is the difference of coordinates when the annotation is a row
# annotation or a column annotation. 
#
# == value
# A `SingleAnnotation-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
SingleAnnotation = function(name, value, col, fun, which = c("column", "row"), 
	show_legend = TRUE, gp = gpar(col = NA)) {

	.Object = new("SingleAnnotation")

	which = match.arg(which)[1]
	.Object@which = which

	if(missing(name)) {
        name = paste0("anno", get_annotation_index() + 1)
        increase_annotation_index()
    }
    .Object@name = name

    gp = check_gp(gp)
    if(!is.null(gp$fill)) {
    	stop("You should not set `fill`.")
    }

    if(missing(fun)) {
    	if(missing(col)) {
    		col = default_col(value)
    	}

    	if(is.atomic(col)) {
            color_mapping = ColorMapping(name = name, colors = col)
        } else if(is.function(col)) {
            color_mapping = ColorMapping(name = name, col_fun = col)
        }

        .Object@color_mapping = color_mapping
        value = value

        if(which == "column") {
	        .Object@fun = function(index) {
	        	n = length(index)
				x = (seq_len(n) - 0.5) / n
				fill = map_to_colors(color_mapping, value[index])
				grid.rect(x, y = 0.5, width = 1/n, height = 1, gp = do.call("gpar", c(list(fill = fill), gp)))
			}
		} else {
			.Object@fun = function(index) {
				n = length(index)
				y = (seq_len(n) - 0.5) / n
				fill = map_to_colors(color_mapping, value[index])
				grid.rect(x = 0.5, y, height = 1/n, width = 1, gp = do.call("gpar", c(list(fill = fill), gp)))
			}
		}

		.Object@show_legend = show_legend
    } else {
    	.Object@fun = fun
    	.Object@show_legend = FALSE
    }

    return(.Object)
}

# == title
# Draw the single annotation
#
# == param
# -object a `SingleAnnotation-class` object.
# -index a vector of orders
#
# == details
# A viewport is created.
#
# The graphics would be different depending the annotation is a row annotation or a column annotation.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw",
	signature = "SingleAnnotation",
	definition = function(object, index) {

	pushViewport(viewport(name = paste("annotation", object@name, sep = "_")))
	object@fun(index)
	upViewport()

})

# == title
# Print the SingleAnnotation object
#
# == param
# -object a `SingleAnnotation-class` object.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "show",
	signature = "SingleAnnotation",
	definition = function(object) {
	if(is.null(object@color_mapping)) {
		cat("An annotation with self-defined function\n")
		cat("name:", object@name, "\n")
		cat("position:", object@which, "\n")
	} else {
		cat("An annotation with", object@color_mapping@type, "color mapping\n")
		cat("name:", object@name, "\n")
		cat("position:", object@which, "\n")
		cat("show legend:", object@show_legend, "\n")
	}
})
