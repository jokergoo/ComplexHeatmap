


# == title
# Class for heatmap annotations
#
# == details
# A complex heatmap contains a list of annotations which represent as different graphics
# placed on rows and columns. The `HeatmapAnnotation-class` is a category of single annotations which are
# by a list of `SingleAnnotation-class` objects with same number of rows or columns.
#
# == methods
# The `HeatmapAnnotation-class` provides following methods:
#
# - `HeatmapAnnotation`: constructor method
# - `draw,HeatmapAnnotation-method`: draw the annotations
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
HeatmapAnnotation = setClass("HeatmapAnnotation",
	slots = list(
		name = "character",
		anno_list = "list",  # a list of `SingleAnnotation` objects
		anno_size = "ANY",
		which = "character",
		size = "ANY"  # only for  consistent of Heatmap
	),
	prototype = list(
		anno_list = list(),
		size = unit(0, "null"),
		which = "row"
	),
    contains = "AdditiveUnit"
)

# == title
# Constructor method for HeatmapAnnotation class
#
# == param
# -df a data frame. Each column will be treated as a simple annotation. The data frame must have column names.
# -name name of the heatmap annotation
# -col a list of colors which contains color mapping to columns in ``df``. See `SingleAnnotation` for how to set colors.
# -show_legend whether show legend for each column in ``df``.
# -... functions which define complex annotations. Values should be named arguments.
# -which are the annotations row annotations or column annotations?
# -annotation_height height of each annotation if annotations are column annotations.
# -annotation_width width of each annotation if annotations are row annotations.
# -height not using currently.
# -width width of the whole heatmap annotations, only used for column annotation when appending to the list of heatmaps.
# -gp graphic parameters for simple annotations.
#
# == details
# The simple annotations are defined by ``df`` and ``col`` arguments. Complex annotations are
# defined by the function list. 
#
# == value
# A `HeatmapAnnotation-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
HeatmapAnnotation = function(df, name, col, show_legend, ..., 
	which = c("column", "row"), annotation_height = 1, annotation_width = 1, 
	height = unit(1, "cm"), width = unit(1, "cm"), gp = gpar(col = NA)) {

	.Object = new("HeatmapAnnotation")

	anno_list = list()
	which = match.arg(which)[1]

	if(missing(name)) {
		name = paste0("heatmap_annotation_", get_row_annotation_index())
		increase_row_annotation_index()
	}

	.Object@name = name

	if(!missing(df)) {
		if(is.null(colnames(df))) {
	        stop("`df` should have column names.")
	    }

	    anno_name = colnames(df)
	    n_anno = ncol(df)

	    if(missing(show_legend)) {
	    	show_legend = rep(TRUE, n_anno)
	    }
	    if(length(show_legend) == 1) {
	    	show_legend = rep(show_legend, n_anno)
	    }

	    if(missing(col)) {
	        for(i in seq_len(n_anno)) {
	        	anno_list = c(anno_list, list(SingleAnnotation(name = anno_name[i], value = df[, i], which = which, show_legend = show_legend[i], gp = gp)))
	        }
	    } else {
	        for(i in seq_len(n_anno)) {
	        	if(is.null(col[[ anno_name[i] ]])) { # if the color is not provided
	        		anno_list = c(anno_list, list(SingleAnnotation(name = anno_name[i], value = df[, i], which = which, show_legend = show_legend[i], gp = gp)))
	        	} else {
	        		anno_list = c(anno_list, list(SingleAnnotation(name = anno_name[i], value = df[, i], col = col[[ anno_name[i] ]], which = which, show_legend = show_legend[i], gp = gp)))
	        	}
	        }
	    }
	}

	# self-defined anntatoin graph are passed by a list of named functions
	fun_list = list(...)
	if(length(fun_list)) {
		if(! all(sapply(fun_list, is.function))) {
			stop("`...` should only contains functions.")
		}

		fun_name = names(fun_list)
		if(is.null(fun_name)) {
			stop("functions should be specified as named arguments.")
		}
		if(any(fun_name %in% c("df", "col", "show_legend", "which", "height", "width", "annotation_height", "annotation_width", "gp"))) {
			stop("function names should be same as other argument names.")
		}
			
		for(i in seq_along(fun_name)) {
			anno_list = c(anno_list, list(SingleAnnotation(name = fun_name[i], fun = fun_list[[i]], which = which)))
		}
	}

	n_anno = length(anno_list)

	anno_size = switch(which,
		column = annotation_height,
		row = annotation_width)

	if(length(anno_size) == 1) {
		if(!is.unit(anno_size)) {
			anno_size = rep(anno_size, n_anno)
		}
	}

	if(!is.unit(anno_size)) {
		anno_size = unit(anno_size/sum(anno_size), "npc")
	}


    .Object@anno_list = anno_list
    .Object@anno_size = anno_size
    .Object@which = which

    size = switch(which,
		column = height,
		row = width)

    .Object@size = size

    return(.Object)
}

# == title
# Get a list of color mapping objects
#
# == param
# -object a `HeatmapAnnotation-class` object.
#
# == details
# Color mapping for visible simple annotations are only returned.
#
# This function is only for internal use.
#
# == values
# A list of `ColorMapping-class` objects or an empty list.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "get_color_mapping_list",
	signature = "HeatmapAnnotation",
	definition = function(object) {

	color_mapping_list = list()
	for(i in seq_along(object@anno_list)) {
		if(object@anno_list[[i]]@show_legend) {
			color_mapping_list = c(color_mapping_list, list(object@anno_list[[i]]@color_mapping))
		}
	}
	return(color_mapping_list)
})

# == title
# Draw the heatmap annotations
#
# == param
# -object a `HeatmapAnnotation-class` object.
# -index a vector of order.
# -... pass to `grid::viewport` which contains all annotations.
#
# == details
# A viewport is created. Mostly, this method is used inside `draw,HeatmapList-method`.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw",
	signature = "HeatmapAnnotation",
	definition = function(object, index, ...) {

	which = object@which
	n_anno = length(object@anno_list)
	anno_size = object@anno_size

	pushViewport(viewport(...))
	for(i in seq_len(n_anno)) {
		if(which == "column") {
			pushViewport(viewport(y = sum(anno_size[seq_len(i)]), height = anno_size[i], just = c("center", "top")))
		} else {
			pushViewport(viewport(x = sum(anno_size[seq_len(i)]), width = anno_size[i], just = c("right", "center")))
		}
		draw(object@anno_list[[i]], index)
		upViewport()
	}
	upViewport()
})

# == title
# Print the Heatmap Annotation object
#
# == param
# -object a `HeatmapAnnotation-class` object.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "show",
	signature = "HeatmapAnnotation",
	definition = function(object) {

	n = length(object@anno_list)

	if(n == 1) {
		cat("A HeatmapAnnotation object with 1 annotation.\n")
	} else {
		cat("A HeatmapAnnotation object with", length(object@anno_list), "annotations.\n")
	}
	cat("\n")
	for(i in seq_along(object@anno_list)) {
		show(object@anno_list[[i]])
		cat("\n")
	}
})


# == title
# Add row annotations or heatmaps as a heatmap list
#
# == param
# -object a `HeatmapAnnotation-class` object.
# -x a `Heatmap-class` object, a `HeatmapAnnotation-class` object or a `HeatmapList-class` object.
#
# == details
# There is a shortcut function ``+.AdditiveUnit``.
#
# == value
# A `HeatmapList-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "add_heatmap",
    signature = "HeatmapAnnotation",
    definition = function(object, x) {

    ht_list = new("HeatmapList")
    ht_list = add_heatmap(ht_list, object)
    ht_list = add_heatmap(ht_list, x)
    return(ht_list)

})
