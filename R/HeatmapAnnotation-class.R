
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
		gap = "ANY",
		subsetable = "logical",
		extended = "ANY"
	),
	prototype = list(
		anno_list = list(),
		size = unit(0, "mm"),
		which = "column",
		gap = unit(0, "mm"),
		subsetable = FALSE,
		extended = unit(c(0, 0, 0, 0), "mm")
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
# -height height of the column annotations, basically it is identical to ``bottom_annotation_height`` or ``top_annotation_height``
#        in `Heatmap` function.
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
HeatmapAnnotation = function(..., 
	df, name, col, na_col = "grey",
	annotation_legend_param = list(), 
	show_legend = TRUE, 
	which = c("column", "row"), 
	annotation_height = NULL, 
	annotation_width = NULL, 
	height = NULL, 
	width = NULL, 
	gp = gpar(col = NA),
	gap = unit(0, "mm"),
	show_annotation_name = TRUE,
	annotation_name_gp = gpar(),
	annotation_name_offset = unit(2, "mm"),
	annotation_name_side = ifelse(which == "column", "right", "bottom"),
	annotation_name_rot = ifelse(which == "column", 0, 90)) {

	which = match.arg(which)[1]
	.ENV$current_annotation_which = which
	on.exit(.ENV$current_annotation_which <- NULL)

	fun_args = names(as.list(environment()))
	
	.Object = new("HeatmapAnnotation")

	anno_list = list()
	
	if(missing(name)) {
		name = paste0("heatmap_annotation_", get_row_annotation_index())
		increase_row_annotation_index()
	}

	.Object@name = name
	n_anno = 0

    arg_list = as.list(sys.call())[-1]
    called_args = names(arg_list)
    anno_args = setdiff(called_args, fun_args)
    if(any(anno_args == "")) stop("annotations should have names.")

    if("df" %in% called_args) {
    	if(is.matrix(df)) {
    		warning("`df` should be a data frame while not a matrix. Convert it to data frame.")
    		df = as.data.frame(df)
    	} else if(!is.data.frame(df)) {
    		stop("`df` should be a data frame.")
    	}
    }

    anno_arg_list = list(...)
	if("df" %in% called_args && length(anno_arg_list)) {
		if(any(duplicated(c(names(df), names(anno_arg_list))))) {
			stop("Annotation names are duplicated. Check the column names of `df`.")
		}
	}

	anno_value_list = list()
	for(nm in called_args) {
		if(nm %in% names(anno_arg_list)) {
			anno_value_list[[nm]] = anno_arg_list[[nm]]
		} else if(nm == "df") {
			for(nm2 in colnames(df))
			anno_value_list[[nm2]] = df[, nm2]
		}
	}

    l_simple_anno = sapply(anno_value_list, is.atomic)
    n_simple_anno = sum(l_simple_anno)
    simple_anno_name = names(anno_value_list[l_simple_anno])
	
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

	is_name_offset_called = !missing(annotation_name_offset)
    is_name_rot_called = !missing(annotation_name_rot)

	n_total_anno = length(anno_value_list)
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

    ### check the length of annotations
    len = sapply(anno_value_list, function(x) {
    	if(is.matrix(x)) {
    		nrow(x)
    	} else if(inherits(x, "AnnotationFunction")) {
    		x@n
    	} else if(is.atomic(x)) {
    		length(x)
    	} else {
    		NA
    	}
    })

    if(length(unique(len)) > 1) {
    	stop("Length of annotations differs.")
    }

	i_simple = 0
	i_anno = 0
	simple_length = NULL
	col_name_defined = NULL
    for(ag in names(anno_value_list)) {

		i_anno = i_anno + 1
		arg_list = list(name = ag, which = which,
				show_name = show_annotation_name[i_anno], 
				name_gp = subset_gp(annotation_name_gp, i_anno), 
	        	name_offset = annotation_name_offset[i_anno], 
	        	name_side = annotation_name_side[i_anno], 
	        	name_rot = annotation_name_rot[i_anno])
		if(!is_name_offset_called) {
			arg_list$name_rot = NULL
		}
		if(!is_name_rot_called) {
			arg_list$name_offset = NULL
		}
		if(inherits(anno_value_list[[ag]], c("function", "AnnotationFunction"))) {
			arg_list$fun = fun = anno_value_list[[ag]]
			anno_list[[ag]] = do.call(SingleAnnotation, arg_list)
		} else if(is.atomic(anno_value_list[[ag]])) {
			arg_list$show_legend = show_legend[i_simple + 1]
			arg_list$gp = gp
			arg_list$legend_param = annotation_legend_param[[i_simple + 1]]
			arg_list$value = anno_value_list[[ag]]
			arg_list$na_col = na_col
			if(missing(col)) {
				anno_list[[ag]] = do.call(SingleAnnotation, arg_list)
		    } else {
		        if(is.null(col[[ ag ]])) { # if the color is not provided
		        	anno_list[[ag]] = do.call(SingleAnnotation, arg_list)
		        } else {
		        	anno_list$col = col[[ ag ]]
		        	anno_list[[ag]] = do.call(SingleAnnotation, arg_list)
		        	col_name_defined = c(col_name_defined, ag)
		        }
		    }
		    i_simple = i_simple + 1
		} else {
			stop("Annotations should be vector/data frame/matrix/functions.")
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
    	gap = rep(gap, n_total_anno)
    } else if(length(gap) == n_total_anno - 1) {
    	gap = unit.c(gap, unit(0, "mm"))
    } else if(length(gap) < n_total_anno - 1) {
    	stop("Length of `gap` is wrong.")
    } 

    .Object@gap = gap

    ### calualte the width/heigit of annotations
    if(which == "column") {
    	anno_size = lapply(anno_list, function(x) x@height)

    	if(any(!sapply(anno_size, is_abs_unit))) {
    		stop("Heights of annotations should be absolute units.")
    	}
    	anno_size = do.call("unit.c", anno_size)
    	if(is.null(height)) {
    		size = sum(anno_size) + sum(gap) - gap[n_total_anno]
    	} else {
    		if(!is_abs_unit(height)) {
    			stop("`height` should be an absolute unit.")
    		}
    		size = height
    		anno_size_in_numeric = convertHeight(anno_size, "mm", valueOnly = TRUE)
    		gap_in_numeric = convertHeight(gap, "mm", valueOnly = TRUE)
    		anno_size = anno_size_in_numeric/(sum(anno_size_in_numeric) + sum(gap) - gap[n_total_anno]) * height
    	}
    } else if(which == "row") {
    	anno_size = lapply(anno_list, function(x) x@width)
    	if(any(!sapply(anno_size, is_abs_unit))) {
    		stop("Widths of annotations should be absolute units.")
    	}
    	anno_size = do.call("unit.c", anno_size)
    	if(is.null(width)) {
    		size = sum(anno_size) + sum(gap) - gap[n_total_anno]
    	} else {
    		if(!is_abs_unit(width)) {
    			stop("`width` should be an absolute unit.")
    		}
    		size = width
    		anno_size_in_numeric = convertWidth(anno_size, "mm", valueOnly = TRUE)
    		gap_in_numeric = convertWidth(gap, "mm", valueOnly = TRUE)
    		anno_size = anno_size_in_numeric/(sum(anno_size_in_numeric) + sum(gap) - gap[n_total_anno]) * width
    	}
    }

	names(anno_list) = sapply(anno_list, function(x) x@name)
    .Object@anno_list = anno_list
    .Object@anno_size = anno_size
    .Object@which = which
    .Object@size = size

    .Object@subsetable = all(sapply(anno_list, function(x) x@subsetable))
    extended = unit(c(0, 0, 0, 0), "mm")
    for(i in 1:4) {
    	extended[[i]] = max(sapply(anno_list, function(anno) {
    		anno@extended[[i]]
    	}))
    }
    .Object@extended = extended

    return(.Object)
}

setMethod(f = "resize",
	signature = "HeatmapAnnotation",
	definition = function(object, annotation_height = NULL, annotation_width = NULL,
		height = NULL, width = NULL, line_size = NULL) {

	which = object@which
	if(which == "column") {
		if(is.null(height)) {
			is_size_set = FALSE
		} else {
			if(!inherits(height, "unit")) {
				stop("`height` should be a `unit` object")
			}
			if(!is_abs_unit(height)) {
				stop("`height` should be an absolute unit.")
			}
			is_size_set = TRUE
		}
		if(is.null(annotation_height)) {
			is_annotation_size_set = FALSE
		} else {
			is_annotation_size_set = TRUE
			annotation_size_adjusted = annotation_height
		}
		size_adjusted = height
		size_name = "height"
	} else if(which == "row") {
		if(is.null(width)) {
			is_size_set = FALSE
		} else {
			if(!inherits(width, "unit")) {
				stop("`width` should be a `unit` object")
			}
			if(!is_abs_unit(width)) {
				stop("`width` should be an absolute unit.")
			}
			is_size_set = TRUE
		}
		if(is.null(annotation_width)) {
			is_annotation_size_set = FALSE
		} else {
			is_annotation_size_set = TRUE
			annotation_size_adjusted = annotation_width
		}
		size_adjusted = width
		size_name = "width"
	} 

	if(which == "column") {
		convertUnitFun = convertHeight
	} else if(which == "row") {
		convertUnitFun = convertWidth
	}

	anno_size = object@anno_size
	size = object@size
	gap = object@gap
	gap = gap[-length(gap)]
	n = length(object@anno_list)

	# the basic rule is
	# 1. if annotation_height is set, it needs to be a vector and height is disabled. If all
	#    annotation_height are absolute units, height is ignored
	# 2. if annotation height contains non-absolute units, height also need to be set and the
	#    non-absolute unit should be set in a simple form such as 1:10 or unit(1, "null")
	# 3. line_size is only used when annotation_height is NULL
	# 4. if only height is set, non-simple annotation is adjusted while keep simple anntation unchanged
	# 5. if only height is set and all annotations are simple annotations, all anntations are adjusted.
	#      and line_size is disabled.

	if(is_annotation_size_set) {
		if(length(annotation_size_adjusted) == 1) {
			annotation_size_adjusted = rep(1, n)
		}
		if(length(annotation_size_adjusted) != n) {
			stop(paste0("Length of annotation_", size_name, " should be same as number of annotations.", sep = ""))
		}

		if(!inherits(annotation_size_adjusted, "unit")) {
			annotation_size_adjusted = unit(annotation_size_adjusted, "null") 
		}

		l_rel_unit = !sapply(1:n, function(i) is_abs_unit(annotation_size_adjusted[i]))
		if(any(l_rel_unit)) { # height/width must be set as an absolute unit
			# height/width must be set
			if(is_size_set) {
				if(is_abs_unit(size_adjusted)) {
					rel_num = sapply(which(l_rel_unit), function(i) {
						if(identical(class(annotation_size_adjusted[i]), "unit")) {
							if(attr(annotation_size_adjusted[i], "unit") != "null") {
								stop("relative unit should be defined as `unit(..., 'null')")
							}
						} else {
							stop("relative unit should be defined as `unit(..., 'null')")
						}
						annotation_size_adjusted[i][[1]]
					})
					rel_num = rel_num/sum(rel_num)
					if(any(!l_rel_unit)) {
						ts = size_adjusted - sum(gap) - sum(annotation_size_adjusted[!l_rel_unit])
					} else {
						ts = size_adjusted - sum(gap)
					}
					if(convertUnitFun(ts, "mm", valueOnly = TRUE) <= 0) {
						stop(paste0(size_name, "is too small."))
					}
					ind = which(l_rel_unit)
					for(i in seq_along(ind)) {
						annotation_size_adjusted[ ind[i] ] = ts*rel_num[i]
					}
				} else {
					stop(paste0("Since annotation_", size_name, " contains relative units, ", size_name, " must be set as an absolute unit."))
				}
			} else {
				stop(paste0("Since annotation_", size_name, " contains relative units, ", size_name, " must be set."))
			}
		}
	}

	# from here `annotation_size_adjusted` contains absolute units if it is called.

	gap = convertUnitFun(gap, "mm", valueOnly = TRUE)

	if(is_size_set) {
		size_adjusted = convertUnitFun(size_adjusted, "mm", valueOnly = TRUE)
	}
	if(is_annotation_size_set) {
		annotation_size_adjusted = convertUnitFun(annotation_size_adjusted, "mm", valueOnly = TRUE)
	}

	if(is_annotation_size_set) {
		# since annotation_size_adjusted has been recalculated, here we simply
		# update the corresponding slots
		object@size = unit(sum(annotation_size_adjusted) + sum(gap), "mm")
		object@anno_size = unit(annotation_size_adjusted, "mm")
	} else {
		size = convertUnitFun(size, "mm", valueOnly = TRUE)
		anno_size = convertUnitFun(anno_size, "mm", valueOnly = TRUE)
	
		l_simple_anno = sapply(seq_len(n), function(i) {
			!is.null(object@anno_list[[i]]@color_mapping)
		})

		if(all(l_simple_anno)) {
			anno_size2 = anno_size/sum(anno_size) * (size_adjusted - sum(gap))
			size_adjusted = unit(size_adjusted, "mm")
			anno_size2 = unit(anno_size2, "mm")
		} else {

			anno_size2 = anno_size
			size_adjusted = convertUnitFun(size_adjusted, "mm", valueOnly = TRUE)
			if(is.null(line_size)) {
				line_size = 5
			} else {
				line_size = convertUnitFun(line_size, "mm", valueOnly = TRUE)
			}

			if(size_adjusted <= sum(gap)) {
				stop(paste0(size_name, "you set is smaller than sum of gaps."))
			}

			## fix the size of simple annotation and zoom function annotations
			ts = size_adjusted - sum(gap) - sum(anno_size[l_simple_anno]*line_size/5)
			if(ts < 0) {
				stop(paste0(size_name, "you set is too small."))
			}
			anno_size2[!l_simple_anno] = anno_size[!l_simple_anno]/sum(anno_size[!l_simple_anno]) * ts
			anno_size2[l_simple_anno] = anno_size[l_simple_anno]*line_size/5

			size_adjusted = unit(size_adjusted, "mm")
			anno_size2 = unit(anno_size2, "mm")
		}
		object@size = size_adjusted
		object@anno_size = anno_size2
	}

	for(i in seq_along(object@anno_list)) {
		if(inherits(object@anno_list[[i]]@fun, "AnnotationFunction")) {
			slot(object@anno_list[[i]]@fun, size_name) = object@anno_size[i]
		}
		slot(object@anno_list[[i]], size_name) = object@anno_size[i]
	}

	return(object)
})

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
setMethod(f = "get_legend_param_list",
	signature = "HeatmapAnnotation",
	definition = function(object) {

	color_mapping_param_list = list()
	for(i in seq_along(object@anno_list)) {
		if(object@anno_list[[i]]@show_legend) {
			color_mapping_param_list = c.list(color_mapping_param_list, object@anno_list[[i]]@legend_param)
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
	definition = function(object, index, k = 1, n = 1, align_to = "bottom", ..., 
		test = FALSE) {

	which = object@which
	n_anno = length(object@anno_list)
	anno_size = object@anno_size
	gap = object@gap

	if(is.character(test)) {
        test2 = TRUE
    } else {
        test2 = test
    }

    if(test2) {
    	grid.newpage()
    	if(which == "column") pushViewport(viewport(width = unit(1, "npc") - unit(4, "cm"), height = object@size))
    	if(which == "row") pushViewport(viewport(height = unit(1, "npc") - unit(4, "cm"), width = object@size))
    } else {
		pushViewport(viewport(...))
	}

	if(missing(index)) {
        n_anno = length(object@anno_list)
		len = sapply(seq_len(n_anno), function(i) {
			if(inherits(object@anno_list[[i]]@fun, "AnnotationFunction")) {
				object@anno_list[[i]]@fun@n
			} else {
				NA
			}
		})
		len = len[!is.na(len)]
		if(length(len)) index = seq_len(len[1])
    }

	if(which == "column") {
		if(align_to == "bottom") { # put on top of the heatmap
			# start from the last annoation which is put on bottom
			for(i in seq_len(n_anno)) {
				pushViewport(viewport(y = sum(anno_size[seq(i, n_anno)]) + sum(gap[seq(i, n_anno)]) - gap[n_anno], 
					height = anno_size[i], just = c("center", "top")))
				oe = try(draw(object@anno_list[[i]], index, k, n))
				if(inherits(oe, "try-error")) {
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
				if(inherits(oe, "try-error")) {
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
			if(inherits(oe, "try-error")) {
				cat("Error when drawing annotation '", object@anno_list[[i]]@name, "'\n", sep = "")
				stop(oe)
			}
			upViewport()
		}
	}
	if(test2) {
        grid.text(test, y = unit(1, "npc") + unit(2, "mm"), just = "bottom")
        grid.rect(unit(0, "npc") - object@extended[2], unit(0, "npc") - object@extended[1], 
            width = unit(1, "npc") + object@extended[2] + object@extended[4],
            height = unit(1, "npc") + object@extended[1] + object@extended[3],
            just = c("left", "bottom"), gp = gpar(fill = "transparent", col = "red", lty = 2))
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
		cat("A HeatmapAnnotation object with 1 annotation\n")
	} else {
		cat("A HeatmapAnnotation object with", length(object@anno_list), "annotations\n")
	}
	cat("  name:", object@name, "\n")
	cat("  position:", object@which, "\n")
	n_anno = length(object@anno_list)
	len = sapply(seq_len(n_anno), function(i) {
		if(inherits(object@anno_list[[i]]@fun, "AnnotationFunction")) {
			object@anno_list[[i]]@fun@n
		} else {
			NA
		}
	})
	len = len[!is.na(len)]
	cat("  items:", ifelse(length(len), len[1], "unknown"), "\n")
	cat("  ", ifelse(object@which == "column", "height", "width"), ": ", as.character(object@size), "\n", sep = "")
    cat("  this object is", ifelse(object@subsetable, "\b", "not"), "subsetable\n")
    dirt = c("bottom", "left", "top", "right")
    for(i in 1:4) {
        if(!identical(unit(0, "mm"), object@extended[i])) {
            cat(" ", as.character(object@extended[i]), "extension on the", dirt[i], "\n")
        }
    }
	cat("\n")
	
	lt = list()
	lt$name = names(object@anno_list)
	lt$annotation_type = sapply(seq_len(n_anno), function(i) {
		if(!is.null(object@anno_list[[i]]@color_mapping)) {
			if(object@anno_list[[i]]@is_anno_matrix) {
				paste0(object@anno_list[[i]]@color_mapping@type, " matrix")
			} else {
				paste0(object@anno_list[[i]]@color_mapping@type, " vector")
			}
		} else if(inherits(object@anno_list[[i]]@fun, "AnnotationFunction")) {
			"AnnotationFunction"
		} else if(inherits(object@anno_list[[i]]@fun, "function")) {
			"function"
		} else {
			""
		}
	})
	lt$color_mapping = sapply(seq_len(n_anno), function(i) {
		ifelse(object@anno_list[[i]]@color_is_random, "random",
			ifelse(is.null(object@anno_list[[i]]@color_mapping), "", "user-defined"))
	})
	size_name = ifelse(object@which == "column", "height", "width")

	if(!dev.interactive()) {
		dev.null()
		on.exit(dev.off())
	}
	lt[[size_name]] = sapply(seq_len(n_anno), function(i) {
		if(size_name == "height") {
			u = object@anno_list[[i]]@height
			if(is_abs_unit(u)) {
				as.character(convertHeight(u, "mm"))
			} else {
				as.character(u)
			}
		} else if(size_name == "width") {
			u = object@anno_list[[i]]@width
			if(is_abs_unit(u)) {
				as.character(convertWidth(u, "mm"))
			} else {
				as.character(u)
			}
		}
	})
	df = as.data.frame(lt)
	print(df, row.names = FALSE)
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

c.HeatmapAnnotation = function(..., gap = unit(0, "mm")) {
	anno_list = list(...)
	n = length(anno_list)
	if(length(unique(sapply(anno_list, function(x) x@which))) != 1) {
		stop("All annotations should be all row annotation or all column annotation.")
	}
	if(length(gap) == 1) gap = rep(gap, n)
	x = anno_list[[1]]
	if(n > 1) {
		x@gap[length(x@gap)] = gap[1]
	}
	for(i in seq_along(anno_list)[-1]) {
		y = anno_list[[i]]
		y@gap[length(y@gap)] = gap[i]
		x@anno_list = c(x@anno_list, y@anno_list)
		x@anno_size = unit.c(x@anno_size, y@anno_size)
		x@gap = unit.c(x@gap, y@gap)
	}
	x@gap[length(x@gap)] = unit(0, "mm")
	x@size = sum(x@anno_size) + sum(x@gap) - x@gap[length(x@gap)]

	nm = names(x)

	ld = duplicated(nm)
	if(any(ld)) {
		dup = unique(nm[ld])
		warning(paste0("Following annotation names are duplicated:\n  ", paste(dup, collapse = ", ")))
		nm2 = nm
		nm2[unlist(split(seq_along(nm), nm))] = unlist(lapply(split(nm, nm), seq_along))
		l = nm %in% dup
		nm[l] = paste0(nm[l], "_", nm2[l])
		names(x) = nm
	}

	extended = unit(c(0, 0, 0, 0), "mm")
    for(i in 1:4) {
    	extended[[i]] = max(sapply(x@anno_list, function(anno) {
    		anno@extended[[i]]
    	}))
    }
    x@extended = extended

	return(x)
}

names.HeatmapAnnotation = function(x) {
	names(x@anno_list)
}

`names<-.HeatmapAnnotation` = function(x, value) {
	if(length(value) != length(x@anno_list)) {
		stop("Length of `value` should be same as number of annotations.")
	}
	if(any(duplicated(value))) {
		stop("Annotation names should be unique.")
	}
	names(x@anno_list) = value
	for(i in seq_along(value)) {
		x@anno_list[[i]]@name =  value[i]
	}
	return(x)
}


# ha[1:2, c("foo", "bar")]
"[.HeatmapAnnotation" = function(x, i, j) {
	if(!missing(j)) {
		if(is.character(j)) {
			j = which(names(x@anno_list) %in% j)
		}
	}
	
    if(nargs() == 1) { # ha[]
        return(x)
    } else if(nargs() == 3 && missing(i)) {  # ha[, "foo"]
        x2 = x
        x2@anno_list = x@anno_list[j]
        for(nm in names(x2@anno_list)) {
        	x2@anno_list[[nm]] = copy_all(x2@anno_list[[nm]])
        }
        x2@anno_size = x@anno_size[j]
        x2@gap = x@gap[j]
        x2@gap[length(x2@gap)] = unit(0, "mm")

        x2@size = sum(x2@anno_size) + sum(x2@gap) - x2@gap[length(x2@gap)]

    } else if(nargs() == 3 && missing(j)) {  # ha[1:4, ]
        x2 = x
        for(nm in names(x2@anno_list)) {
        	x2@anno_list[[nm]] = x2@anno_list[[nm]][i]
        }

    } else if(nargs() == 2) { # ha[1:4]
    	x2 = x
        for(nm in names(x2@anno_list)) {
        	x2@anno_list[[nm]] = x2@anno_list[[nm]][i]
        }

    } else if (!missing(i) && !missing(j)) { # ha[1:4, "foo"]
    	x2 = x
        x2@anno_list = x@anno_list[j]
        for(nm in names(x2@anno_list)) {
        	x2@anno_list[[nm]] = x2@anno_list[[nm]][i]
        }
        x2@anno_size = x@anno_size[j]
        x2@gap = x@gap[j]
        x2@gap[length(x2@gap)] = unit(0, "mm")

        x2@size = sum(x2@anno_size) + sum(x2@gap) - x2@gap[length(x2@gap)]

    }
    
    extended = unit(c(0, 0, 0, 0), "mm")
    for(i in 1:4) {
    	extended[[i]] = max(sapply(x2@anno_list, function(anno) {
    		anno@extended[[i]]
    	}))
    }
    x2@extended = extended

    return(x2)
}
