

# == title 
# Class for a single annotation
#
# == details
# A complex heatmap always has more than one annotations on rows and columns.
# The most simple annotation is one row or one column grids in which different colors
# represent different classes of the data. It can also be more complex
# graphics, such as a boxplot shows data distribution of corresponding rows or columns.
#
# The `SingleAnnotation` class is used to store data for a single annotation and provides
# methods to draw annotation.
#
# == methods
# The `SingleAnnotation` class provides following methods:
#
# - `initialize,SingleAnnotation-method`: constructor method
# - `draw,SingleAnnotation-method`: draw the single annotation.
#
# == seealso
# A `HeatmapAnnotation` object contains a list of `SingleAnnotation` objects.
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
# -.Object private object.
# -name name for this annotation
# -value A vector of annotation
# -col colors corresponding to ``value``. If the mapping is discrete mapping, the value of ``col``
#      should be a vector; If the mapping is continuous mapping, the value of ``col`` should be 
#      a color mapping function. 
# -fun a self-defined function. The argument of this function should be a vector of index.
# -which is the annotation placed on rows or columns
# -show_legend if it is a simple annotation, whether show legend when making the complete heatmap.
#
# == details
# The most simple annotation is one row or one column grids in which different colors
# represent different classes of the data. Here the function use `ColorMapping` class
# to process such information. ``value`` and ``col`` arguments are used to construct a 
# `ColorMapping` object.
#
# ``fun`` is used to construct a more complex annotation. The only input argument of ``fun`` is a index
# of rows or columns so that graphics can be corresponded to the rows or columns.
#
#    value = 1:10
#    anno = SingleAnnotation(fun = function(index) {
#        n = length(index)
#        x = (seq_len(n) - 0.5) / n
#        y  = (value - min(value))/(max(value) - min(value))
#        grid.points(x, y, default.units = "npc")
#    })
#
# == value
# A `SingleAnnotation` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "initialize",
	signature = "SingleAnnotation",
	definition = function(.Object, name, value, col, fun, which = c("row", "column"), 
	show_legend = TRUE) {

	which = match.arg(which)[1]
	.Object@which = which

	if(missing(name)) {
        name = paste0("anno", get_annotation_index() + 1)
        increase_annotation_index()
    }
    .Object@name = name

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

        if(which == "row") {
	        .Object@fun = function(index) {
				n = length(index)
				x = (seq_len(n) - 0.5) / n
				fill = map(color_mapping, value[index])
				grid.rect(x, y = 0.5, width = 1/n, height = 1, gp = gpar(fill = fill, col = NA))
			}
		} else {
			.Object@fun = function(index) {
				n = length(index)
				y = (seq_len(n) - 0.5) / n
				fill = map(color_mapping, value[index])
				grid.rect(x = 0.5, y, height = 1/n, width = 1, gp = gpar(fill = fill, col = NA))
			}
		}

		.Object@show_legend = show_legend
    } else {
    	.Object@fun = fun
    	.Object@show_legend = FALSE
    }

    return(.Object)
})

# == title
# Draw the single annotation
#
# == param
# -object a `SingleAnnotation` object.
# -index the index
#
# == details
# A viewport is created.
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
# -object a `SingleAnnotation` object.
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
		cat("An annotaiton with", object@color_mapping@type, "color mapping\n")
		cat("name:", object@name, "\n")
		cat("position:", object@which, "\n")
		cat("show legend:", object@show_legend, "\n")
	}
})
