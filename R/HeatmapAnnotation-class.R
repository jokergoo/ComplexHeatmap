
# == title
# Class for heatmap annotations
#
# == details
# A complex heatmap contains a list of annotations which are represented as different graphics
# placed on rows and columns. The `HeatmapAnnotation-class` contains a list of single annotations which are
# represented as a list of `SingleAnnotation-class` objects with same number of rows or columns.
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
		size = "ANY",  # only for  consistent of Heatmap
		gap = "ANY"
	),
	prototype = list(
		anno_list = list(),
		size = unit(0, "mm"),
		which = "row",
		gap = unit(0, "mm")
	),
    contains = "AdditiveUnit"
)

# == title
# Constructor method for HeatmapAnnotation class
#
# == param
# -df a data frame. Each column will be treated as a simple annotation. The data frame must have column names.
# -name name of the heatmap annotation, optional.
# -col a list of colors which contains color mapping to columns in ``df``. See `SingleAnnotation` for how to set colors.
# -na_col color for ``NA`` values in simple annotations.
# -annotation_legend_param a list which contains parameters for annotation legends
# -show_legend whether show legend for each column in ``df``.
# -... functions which define complex annotations or vectors of simple annotation. Values should be named arguments.
# -which are the annotations row annotations or column annotations?
# -annotation_height height of each annotation if annotations are column annotations.
# -annotation_width width of each annotation if annotations are row annotations.
# -height not using currently.
# -width width of the whole heatmap annotations, only used for row annotation when appending to the list of heatmaps.
# -gp graphic parameters for simple annotations.
# -gap gap between each annotation
# -show_annotation_name whether show annotation names. For column annotation, annotation names are drawn either on the left
#   or the right, and for row annotations, names are draw either on top to at bottom. The value can be a vector.
# -annotation_name_gp graphic parameters for anntation names. Graphic paramters can be vectors.
# -annotation_name_offset offset to the annotations, `grid::unit` object. The value can be a vector.
# -annotation_name_side side of the annotation names.
# -annotation_name_rot rotation of the annotation names, can only take values in ``c(00, 90, 180, 270)``. The value can be a vector.
#
# == details
# The simple annotations are defined by ``df`` and ``col`` arguments. Complex annotations are
# defined by the function list. So you need to at least to define ``df`` or a annotation function.
#
# == value
# A `HeatmapAnnotation-class` object.
#
# == seealso
# There are two shortcut functions: `rowAnnotation` and `columnAnnotation`.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
HeatmapAnnotation = function(df, name, col, na_col = "grey",
	annotation_legend_param = list(), 
	show_legend = TRUE, 
	..., 
	which = c("column", "row"), 
	annotation_height = 1, 
	annotation_width = 1, 
	height = calc_anno_size(), 
	width = calc_anno_size(), 
	gp = gpar(col = NA),
	gap = unit(0, "mm"),
	show_annotation_name = FALSE,
	annotation_name_gp = gpar(),
	annotation_name_offset = unit(2, "mm"),
	annotation_name_side = ifelse(which == "column", "right", "bottom"),
	annotation_name_rot = ifelse(which == "column", 0, 90)) {

	.Object = new("HeatmapAnnotation")

	anno_list = list()
	which = match.arg(which)[1]

	if(missing(name)) {
		name = paste0("heatmap_annotation_", get_row_annotation_index())
		increase_row_annotation_index()
	}

	.Object@name = name
	n_anno = 0

    arg_list = as.list(match.call())[-1]
    called_args = names(arg_list)
    anno_args = setdiff(called_args, c("name", "col", "na_col", "annotation_legend_param", "show_legend", "which", 
    	                             "annotation_height", "annotation_width", "height", "width", "gp", "gap",
    	                             "show_annotation_name", "annotation_name_gp", "annotation_name_offset", "annotation_name_side", "annotation_name_rot"))
    if(any(anno_args == "")) stop("annotations should have names.")
    if(any(duplicated(anno_args))) stop("names of annotations should be unique.")
    anno_arg_list = list(...)
    if(length(anno_arg_list)) {
	    n_simple_anno = {if("df" %in% anno_args) ncol(df) else 0} + sum(sapply(anno_arg_list, is.atomic))
	    simple_anno_name = c({if("df" %in% anno_args) colnames(df) else NULL}, anno_args[sapply(anno_arg_list, is.atomic)])
	} else {
		n_simple_anno = {if("df" %in% anno_args) ncol(df) else 0}
	    simple_anno_name = {if("df" %in% anno_args) colnames(df) else NULL}
	}

    if(any(duplicated(simple_anno_name))) stop("names of simple annotations should be unique.")

    # normalize `show_legend`
    if(length(show_legend) == 1) {
		show_legend = rep(show_legend, n_simple_anno)
	}

	# normalize `heatmap_legend_param`
	if(length(annotation_legend_param) == 0) {
		annotation_legend_param = rep.list(NULL, n_simple_anno)
	} else if(inherits(annotation_legend_param, "list")) {
		if(all(sapply(annotation_legend_param, inherits, "list"))) {  # if it is a list of lists
			nl = length(annotation_legend_param)
			if(nl > n_simple_anno) {
				stop("Amount of legend params is larger than the number of simple annotations.")
			}
			if(is.null(names(annotation_legend_param))) {
				names(annotation_legend_param) = simple_anno_name[seq_len(nl)]
			} else if(length(setdiff(names(annotation_legend_param), simple_anno_name))) {
				stop("Some names in 'annotation_legend_param' are not in names of simple annotations.")
			} else {
				annotation_legend_param = annotation_legend_param[ intersect(simple_anno_name, names(annotation_legend_param)) ]
			}
			lp = rep.list(NULL, n_simple_anno)

			names(lp) = simple_anno_name
			for(i in seq_along(lp)) {
				if(names(lp)[i] %in% names(annotation_legend_param)) {
					lp[[i]] = annotation_legend_param[[names(lp)[i]]]
				}
			}
			annotation_legend_param = lp
		} else {
			annotation_legend_param = rep.list(annotation_legend_param, n_simple_anno)
		}
	}

	n_total_anno = 0
	for(ag in anno_args) {
		if(ag == "df") {
			n_total_anno = n_total_anno + ncol(df)
		} else {
			n_total_anno = n_total_anno + 1
		}
	}
	if(length(show_annotation_name) == 1) {
    	show_annotation_name = rep(show_annotation_name, n_total_anno)
    }
    if(length(annotation_name_offset) == 1) {
    	annotation_name_offset = rep(annotation_name_offset, n_total_anno)
    }
    if(length(annotation_name_side) == 1) {
    	annotation_name_side = rep(annotation_name_side, n_total_anno)
    }
    if(length(annotation_name_rot) == 1) {
    	annotation_name_rot = rep(annotation_name_rot, n_total_anno)
    }
    annotation_name_gp = recycle_gp(annotation_name_gp, n_total_anno)

    if(!missing(col)) {
    	if(is.null(names(col))) {
    		stop("`col` should be a named list.")
    	}
    	if(any(is.na(names(col)))) {
    		stop("`col` should be a named list.")
    	}
    	if(any(sapply(col, function(x) if(is.function(x)) FALSE else is.null(names(x))))) {
    		stop("elements in `col` should be named vectors.")
    	}
    	if(any(sapply(col, function(x) if(is.function(x)) FALSE else any(is.na(names(x)))))) {
    		stop("elements in `col` should be named vectors.")
    	}
    }
	i_simple = 0
	i_anno = 0
	simple_length = NULL
	col_name_defined = NULL
    for(ag in anno_args) {
		if(ag == "df") {
			if(is.null(colnames(df))) {
		        stop("`df` should have column names.")
		    }
		    if(is.null(simple_length)) {
		    	simple_length = nrow(df)
		    } else if(nrow(df) != simple_length) {
		    	stop("length of simple annotations differ.")
		    }

		    anno_name = colnames(df)
		    n_anno = ncol(df)

		    if(missing(col)) {
		        for(i in seq_len(n_anno)) {
		        	i_anno = i_anno + 1
		        	anno_list = c(anno_list, list(SingleAnnotation(name = anno_name[i], value = df[, i], na_col = na_col, which = which, 
		        		show_legend = show_legend[i_simple + i], gp = gp, legend_param = annotation_legend_param[[i_simple + i]],
		        		show_name = show_annotation_name[i_anno], name_gp = subset_gp(annotation_name_gp, i_anno), 
		        		name_offset = annotation_name_offset[i_anno], name_side = annotation_name_side[i_anno], name_rot = annotation_name_rot[i_anno])))
		        }
		    } else {
		        for(i in seq_len(n_anno)) {
		        	i_anno = i_anno + 1
		        	if(is.null(col[[ anno_name[i] ]])) { # if the color is not provided
		        		anno_list = c(anno_list, list(SingleAnnotation(name = anno_name[i], value = df[, i], na_col = na_col, which = which, 
		        			show_legend = show_legend[i_simple + i], gp = gp, legend_param = annotation_legend_param[[i_simple + i]],
		        			show_name = show_annotation_name[i_anno], name_gp = subset_gp(annotation_name_gp, i_anno), 
		        			name_offset = annotation_name_offset[i_anno], name_side = annotation_name_side[i_anno], name_rot = annotation_name_rot[i_anno])))
		        	} else {
		        		anno_list = c(anno_list, list(SingleAnnotation(name = anno_name[i], value = df[, i], na_col = na_col, col = col[[ anno_name[i] ]], 
		        			which = which, show_legend = show_legend[i_simple + i], gp = gp, legend_param = annotation_legend_param[[i_simple + i]],
		        			show_name = show_annotation_name[i_anno], name_gp = subset_gp(annotation_name_gp, i_anno), 
		        			name_offset = annotation_name_offset[i_anno], name_side = annotation_name_side[i_anno], name_rot = annotation_name_rot[i_anno])))
		        		col_name_defined = c(col_name_defined, anno_name[i])
		        	}
		        }
		    }
		    i_simple = i_simple + n_anno
		} else {
			i_anno = i_anno + 1
			if(inherits(anno_arg_list[[ag]], "function")) {
				anno_list = c(anno_list, list(SingleAnnotation(name = ag, fun = anno_arg_list[[ag]], which = which,
					show_name = show_annotation_name[i_anno], name_gp = subset_gp(annotation_name_gp, i_anno), 
		        	name_offset = annotation_name_offset[i_anno], name_side = annotation_name_side[i_anno], name_rot = annotation_name_rot[i_anno])))
			} else if(is.atomic(anno_arg_list[[ag]])) {

			    if(is.null(simple_length)) {
			    	simple_length = length(anno_arg_list[[ag]])
			    } else if(length(anno_arg_list[[ag]]) != simple_length) {
			    	stop("length of simple annotations differ.")
			    }
				if(missing(col)) {
			        anno_list = c(anno_list, list(SingleAnnotation(name = ag, value = anno_arg_list[[ag]], na_col = na_col, which = which, 
			        	show_legend = show_legend[i_simple + 1], gp = gp, legend_param = annotation_legend_param[[i_simple + 1]],
			        	show_name = show_annotation_name[i_anno], name_gp = subset_gp(annotation_name_gp, i_anno), 
		        		name_offset = annotation_name_offset[i_anno], name_side = annotation_name_side[i_anno], name_rot = annotation_name_rot[i_anno])))
			    } else {
			        if(is.null(col[[ ag ]])) { # if the color is not provided
			        	anno_list = c(anno_list, list(SingleAnnotation(name = ag, value = anno_arg_list[[ag]], na_col = na_col, which = which, 
			        		show_legend = show_legend[i_simple + 1], gp = gp, legend_param = annotation_legend_param[[i_simple + 1]],
			        		show_name = show_annotation_name[i_anno], name_gp = subset_gp(annotation_name_gp, i_anno), 
		        		name_offset = annotation_name_offset[i_anno], name_side = annotation_name_side[i_anno], name_rot = annotation_name_rot[i_anno])))
			        } else {
			        	anno_list = c(anno_list, list(SingleAnnotation(name = ag, value = anno_arg_list[[ag]], na_col = na_col, col = col[[ ag ]], 
			        		which = which, show_legend = show_legend[i_simple + 1], gp = gp, legend_param = annotation_legend_param[[i_simple + 1]],
			        		show_name = show_annotation_name[i_anno], name_gp = subset_gp(annotation_name_gp, i_anno), 
		        			name_offset = annotation_name_offset[i_anno], name_side = annotation_name_side[i_anno], name_rot = annotation_name_rot[i_anno])))
			        	col_name_defined = c(col_name_defined, ag)
			        }
			    }
			    i_simple = i_simple + 1
			} else {
				stop("additional arguments should be annotation vectors or annotation functions.")
			} 
		}
	}
	
	if(!missing(col)) {
		unused_col_name = setdiff(names(col), col_name_defined)
		if(length(unused_col_name)) {
			warning(paste0("Following are defined in `col` while have no corresponding annotations:\n", paste(unused_col_name, collapse = ", ")))
		}
	}

	n_total_anno = length(anno_list)

	if(is.null(gap)) gap = unit(0, "mm")

	# the nth gap does not really matter
    if(length(gap) == 1) {
    	.Object@gap = rep(gap, n_total_anno)
    } else if(length(gap) == n_total_anno - 1) {
    	.Object@gap = unit.c(gap, unit(0, "mm"))
    } else if(length(gap) < n_total_anno - 1) {
    	stop("Length of `gap` is wrong.")
    } else {
    	gap[n_total_anno] = unit(0, "mm")
    	.Object@gap = gap
    }

	anno_size = switch(which,
		column = annotation_height,
		row = annotation_width)

	if(length(anno_size) == 1) {
		if(!is.unit(anno_size)) {
			anno_size = rep(anno_size, n_total_anno)
		}
	}

	if(!is.unit(anno_size)) {
		anno_size = anno_size/sum(anno_size)*(unit(1, "npc") - sum(.Object@gap))
	}

	names(anno_list) = sapply(anno_list, function(x) x@name)
    .Object@anno_list = anno_list
    .Object@anno_size = anno_size
    .Object@which = which

    calc_anno_size = function() sum(.Object@anno_size)

    size = switch(which,
		column = height,
		row = width)

    called_args = names(match.call()[-1])

    if(!is_abs_unit(size)) {
    	if(which == "row" && !("width" %in% called_args))
    		size = unit(5*length(anno_list), "mm") + sum(gap)
    	else if(which == "column" && !("height" %in% called_args))
    		size = unit(5*length(anno_list), "mm") + sum(gap)	
    }
    .Object@size = size

    return(.Object)
}

# == title
# Construct row annotations
#
# == param
# -... pass to `HeatmapAnnotation`
#
# == details
# The function is identical to 
#
#     HeatmapAnnotation(..., which = "row")
#
# == value
# A `HeatmapAnnotation-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
rowAnnotation = function(...) {
	HeatmapAnnotation(..., which = "row")
}

# == title
# Construct column annotations
#
# == param
# -... pass to `HeatmapAnnotation`
#
# == details
# The function is identical to
#
#     HeatmapAnnotation(..., which = "column")
#
# == value
# A `HeatmapAnnotation-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
columnAnnotation = function(...) {
	HeatmapAnnotation(..., which = "column")
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
			color_mapping_list = c.list(color_mapping_list, object@anno_list[[i]]@color_mapping)
		}
	}
	return(color_mapping_list)
})

# == title
# Get a list of color mapping parameters
#
# == param
# -object a `HeatmapAnnotation-class` object.
#
# == details
# Color mapping parameters for visible simple annotations are only returned.
#
# This function is only for internal use.
#
# == values
# A list.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "get_color_mapping_param_list",
	signature = "HeatmapAnnotation",
	definition = function(object) {

	color_mapping_param_list = list()
	for(i in seq_along(object@anno_list)) {
		if(object@anno_list[[i]]@show_legend) {
			color_mapping_param_list = c.list(color_mapping_param_list, object@anno_list[[i]]@color_mapping_param)
		}
	}
	return(color_mapping_param_list)
})

# == title
# Draw the heatmap annotations
#
# == param
# -object a `HeatmapAnnotation-class` object.
# -index a vector of order.
# -k if row annotation is splitted, the value identifies which row slice.
# -n total number of row slices.
# -align_to if the allocated space is more than than the column annotation itself, should
#     the viewport be aligned to the top or bottom?
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
	definition = function(object, index, k = NULL, n = NULL, align_to = "bottom", ...) {

	which = object@which
	n_anno = length(object@anno_list)
	anno_size = object@anno_size
	gap = object@gap

	pushViewport(viewport(...))
	if(which == "column") {
		if(align_to == "bottom") { # put on top of the heatmap
			# start from the last annoation which is put on bottom
			for(i in seq_len(n_anno)) {
				pushViewport(viewport(y = sum(anno_size[seq(i, n_anno)]) + sum(gap[seq(i, n_anno)]) - gap[n_anno], 
					height = anno_size[i], just = c("center", "top")))
				oe = try(draw(object@anno_list[[i]], index, k, n))
				if("try-error" %in% class(oe)) {
					cat("Error when drawing annotation '", object@anno_list[[i]]@name, "'\n", sep = "")
					stop(oe)
				}
				upViewport()
			}
		} else { # put on bottom of the heatmap
			# start for the first annotation which is put on the top
			for(i in seq_len(n_anno)) {
				pushViewport(viewport(y = unit(1, "npc") - (sum(anno_size[seq_len(i)]) + sum(gap[seq_len(i)]) - gap[i]), 
					height = anno_size[i], just = c("center", "bottom")))
				oe = try(draw(object@anno_list[[i]], index, k, n))
				if("try-error" %in% class(oe)) {
					cat("Error when drawing annotation '", object@anno_list[[i]]@name, "'\n", sep = "")
					stop(oe)
				}
				upViewport()
			}
		}
	} else if(which == "row") {
		for(i in seq_len(n_anno)) {
			pushViewport(viewport(x = sum(anno_size[seq_len(i)]) + sum(gap[seq_len(i)]) - gap[i], width = anno_size[i], just = c("right", "center")))
			oe = try(draw(object@anno_list[[i]], index, k, n))
			if("try-error" %in% class(oe)) {
				cat("Error when drawing annotation '", object@anno_list[[i]]@name, "'\n", sep = "")
				stop(oe)
			}
			upViewport()
		}
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
