
# == title
# Class for Heatmap Annotations
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
		width = "ANY",  
		height = "ANY",  
		gap = "ANY",
		subsetable = "logical",
		extended = "ANY"
	),
	prototype = list(
		anno_list = list(),
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
# -... Name-value pairs where the names correspond to annotation names and values can be a vector, a matrix and an
#      annotation function. Each pair is sent to `SingleAnnotation` to contruct a single annotation.
# -df A data frame. Each column will be treated as a simple annotation. The data frame must have column names.
# -name Name of the heatmap annotation, optional.
# -col A list of colors which contain color mapping to columns in ``df`` and simple annotations define din ``...``. 
#      See `SingleAnnotation` for how to set colors.
# -na_col Color for ``NA`` values in simple annotations.
# -annotation_legend_param A list which contains parameters for annotation legends. See `color_mapping_legend,ColorMapping-method` for all possible options.
# -show_legend Whether show annotation legend. The value can be one single value or a vector which corresponds to the simple annotations.
# -which Are the annotations row annotations or column annotations?
# -gp Graphic parameters for simple annotations (with ``fill`` parameter ignored).
# -border border of single annotations.
# -gap Gap between each two annotation. It can be a single value or a vector of `grid::unit` objects.
# -show_annotation_name Whether show annotation names? For column annotation, annotation names are drawn either on the left
#   or the right, and for row annotations, names are draw either on top to at bottom. The value can be a vector.
# -annotation_name_gp Graphic parameters for anntation names. Graphic paramters can be vectors.
# -annotation_name_offset Offset to the annotations, `grid::unit` object. The value can be a vector.
# -annotation_name_side Side of the annotation names.
# -annotation_name_rot Rotation of the annotation names, can only take values in ``c(00, 90, 180, 270)``. The value can be a vector.
# -annotation_height Height of each annotation if annotations are column annotations.
# -annotation_width Width of each annotation if annotations are row annotations.
# -height Height of the complete column annotations.
# -width Width of the complete heatmap annotations.
# -anno_simple_size size of the simple annotation.
# -simple_anno_size_adjust whether also adjust the size of simple annotations when adjust the whole heatmap annotation.
#
# == details
# There are three ways to specify heatmap annotations:
#
# 1. If the annotation is simply a vector or a matrix, it can be specified as ``HeatmapAnnotation(foo = 1:10)``.
# 2. If the annotations are already stored as a data frame, it can be specified as ``HeatmapAnnotation(df = df)``.
# 3. For complex annotation, users can use the pre-defined annotation functions such as `anno_points`: ``HeatmapAnnotation(foo = anno_points(1:10))``.
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
	gp = gpar(col = NA),
	border = FALSE,
	gap = unit(0, "mm"),
	
	show_annotation_name = TRUE,
	annotation_name_gp = gpar(),
	annotation_name_offset = unit(1, "mm"),
	annotation_name_side = ifelse(which == "column", "right", "bottom"),
	annotation_name_rot = ifelse(which == "column", 0, 90),
	
	annotation_height = NULL, 
	annotation_width = NULL, 
	height = NULL,
	width = NULL,
	anno_simple_size = ht_opt$anno_simple_size,
	simple_anno_size_adjust = FALSE
	) {

	.ENV$current_annotation_which = NULL
	which = match.arg(which)[1]
	.ENV$current_annotation_which = which
	on.exit(.ENV$current_annotation_which <- NULL)

	fun_args = names(as.list(environment()))

	verbose = ht_opt$verbose
	
	.Object = new("HeatmapAnnotation")

	anno_list = list()
	
	if(missing(name)) {
		name = paste0("heatmap_annotation_", get_row_annotation_index())
		increase_row_annotation_index()
	}

	.Object@name = name
	n_anno = 0

	#### check system calls ####

	# HeatmapAnnotation is either called by `HeatmapAnnotation()` or by `rowAnnotation()`/`columnAnnotation()`
	sc = sys.calls()
	nsc = length(sc)
	if(nsc == 1) {
		scl = as.list(sc[[1]])
		arg_list = scl[-1]
	} else {
		scl = as.list(sc[[nsc-1]])
		if(any(as.character(scl[[1]]) %in% c("HeatmapAnnotation", "rowAnnotation", "columnAnnotation"))) {
			arg_list = scl[-1]
		} else {
			scl = as.list(sc[[nsc]])
			arg_list = scl[-1]
		}
	}

    called_args = names(arg_list)
    anno_args = setdiff(called_args, fun_args)
    if(any(anno_args == "")) stop("annotations should have names.")
    if(is.null(called_args)) {
    	stop_wrap("It seems you are putting only one argument to the function. If it is a simple vector annotation or a function annotation (e.g. anno_*()), specify it as HeatmapAnnotation(name = value). If it is a data frame annotation, specify it as HeatmapAnnotation(df = value)")
    }

    ##### pull all annotation to `anno_value_list`####
    if("df" %in% called_args) {
    	if(is.matrix(df)) {
    		warning_wrap("`df` should be a data frame while not a matrix. Convert it to data frame.")
    		df = as.data.frame(df)
    	} else if(!is.data.frame(df)) {
    		oe = try(df <- as.data.frame(df), silent = TRUE)
    		if(inherits(oe, "try-errir")) {
    			stop_wrap("`df` should be a data frame.")
    		}
    	}
    }

    anno_arg_list = list(...)
	if("df" %in% called_args && length(anno_arg_list)) {
		if(any(duplicated(c(names(df), names(anno_arg_list))))) {
			stop_wrap("Annotation names are duplicated. Check the column names of `df`.")
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

    if(verbose) qqcat("in total there are @{length(anno_value_list)} annotations (@{n_simple_anno} simple annotations)\n")
	
    # normalize `show_legend`
    if(length(show_legend) == 1) {
		show_legend = rep(show_legend, n_simple_anno)
	}

	###### normalize `heatmap_legend_param` #######
	if(length(annotation_legend_param) == 0) {
		annotation_legend_param = rep.list(NULL, n_simple_anno)
	} else if(inherits(annotation_legend_param, "list")) {
		if(all(sapply(annotation_legend_param, inherits, "list"))) {  # if it is a list of lists
			nl = length(annotation_legend_param)
			if(nl > n_simple_anno) {
				stop_wrap("Amount of legend params is larger than the number of simple annotations.")
			}
			if(is.null(names(annotation_legend_param))) {
				names(annotation_legend_param) = simple_anno_name[seq_len(nl)]
			} else if(length(setdiff(names(annotation_legend_param), simple_anno_name))) {
				stop_wrap("Some names in 'annotation_legend_param' are not in names of simple annotations.")
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

    an = names(anno_value_list)
    show_annotation_name = recycle_param(show_annotation_name, an, TRUE)
    annotation_name_offset = recycle_param(annotation_name_offset, an, TRUE)
    annotation_name_side = recycle_param(annotation_name_side, an, TRUE)
    annotation_name_rot = recycle_param(annotation_name_rot, an, TRUE)
    if(missing(border)) {
    	if(!is.null(ht_opt$annotation_border)) border = ht_opt$annotation_border
    }
    border = recycle_param(border, an, FALSE)
    annotation_name_gp = recycle_gp(annotation_name_gp, n_total_anno)

    if(!missing(col)) {
    	if(is.null(names(col))) {
    		stop_wrap("`col` should be a named list.")
    	}
    	if(any(is.na(names(col)))) {
    		stop_wrap("`col` should be a named list.")
    	}
    	if(any(sapply(col, function(x) if(is.function(x)) FALSE else is.null(names(x))))) {
    		stop_wrap("elements in `col` should be named vectors.")
    	}
    	if(any(sapply(col, function(x) if(is.function(x)) FALSE else any(is.na(names(x)))))) {
    		stop_wrap("elements in `col` should be named vectors.")
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
    len = len[!is.na(len)]
    len = len[len > 0]
    if(length(len)) {
	    if(length(unique(len)) > 1) {
	    	stop_wrap("Length of annotations differs.")
	    }
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
	        	name_rot = annotation_name_rot[i_anno],
	        	border = border[i_anno])
		if(!is_name_offset_called) {
			arg_list$name_rot = NULL
		}
		if(!is_name_rot_called) {
			arg_list$name_offset = NULL
		}
		if(inherits(anno_value_list[[ag]], c("function", "AnnotationFunction"))) {
			arg_list$fun = anno_value_list[[ag]]
			if(inherits(anno_value_list[[ag]], "function")) {
				if(which == "row") {
					arg_list$width = unit(1, "cm")
				} else {
					arg_list$height = unit(1, "cm")
				}
			}
			anno_list[[ag]] = do.call(SingleAnnotation, arg_list)
		} else if(is.atomic(anno_value_list[[ag]])) {
			arg_list$show_legend = show_legend[i_simple + 1]
			arg_list$gp = gp
			arg_list$legend_param = annotation_legend_param[[i_simple + 1]]
			arg_list$value = anno_value_list[[ag]]
			arg_list$na_col = na_col
			arg_list$anno_simple_size = anno_simple_size
			if(missing(col)) {
				anno_list[[ag]] = do.call(SingleAnnotation, arg_list)
		    } else {
		        if(is.null(col[[ ag ]])) { # if the color is not provided
		        	anno_list[[ag]] = do.call(SingleAnnotation, arg_list)
		        } else {
		        	arg_list$col = col[[ ag ]]
		        	anno_list[[ag]] = do.call(SingleAnnotation, arg_list)
		        	col_name_defined = c(col_name_defined, ag)
		        }
		    }
		    i_simple = i_simple + 1
		} else {
			stop_wrap(paste0(ag, ": annotations should be vector/data frame (only `df`)/matrix/functions."))
		} 
		
	}
	
	if(!missing(col)) {
		unused_col_name = setdiff(names(col), col_name_defined)
		if(length(unused_col_name)) {
			# warning(paste0("Following are defined in `col` while have no corresponding annotations:\n", paste(unused_col_name, collapse = ", ")))
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
    	stop_wrap("Length of `gap` is wrong.")
    } 

    .Object@gap = gap

    ### calualte the width/heigit of annotations
    global_height = NULL
    global_width = NULL
    if(which == "column") {
		anno_size = do.call("unit.c", lapply(anno_list, height))
		height = sum(anno_size) + sum(gap) - gap[n_total_anno]
    	
    	# for width, only look at `width`
    	if(is.null(width)) {
    		width = unit(1, "npc")
    	}
    	for(i in 1:n_total_anno) {
    		width(anno_list[[i]]) = width
    	}
    	
    } else if(which == "row") {

		anno_size = do.call("unit.c", lapply(anno_list, width))
		width = sum(anno_size) + sum(gap) - gap[n_total_anno]
    	
    	if(is.null(height)) {
    		height = unit(1, "npc")
    	}
    	for(i in 1:n_total_anno) {
    		height(anno_list[[i]]) = height
    	}
    }

    if(is_abs_unit(width)) {
    	width = convertWidth(width, "mm")
    }
    if(is_abs_unit(height)) {
    	height = convertWidth(height, "mm")
    }
    anno_size = convertWidth(anno_size, "mm")

	names(anno_list) = sapply(anno_list, function(x) x@name)
    .Object@anno_list = anno_list
    .Object@anno_size = anno_size
    .Object@which = which
    .Object@width = width
    .Object@height = height

    .Object@subsetable = all(sapply(anno_list, function(x) x@subsetable))
    extended = unit(c(0, 0, 0, 0), "mm")
    for(i in 1:4) {
    	extended[[i]] = max(sapply(anno_list, function(anno) {
    		anno@extended[[i]]
    	}))
    }
    .Object@extended = extended

    ## adjust height/width if `width`/`annotation_width` is set
    if(which == "column") {
	    .Object = resize(.Object, height = height, annotation_height = annotation_height,
	    	anno_simple_size = anno_simple_size, simple_anno_size_adjust = simple_anno_size_adjust)
	} else {
		.Object = resize(.Object, width = width, annotation_width = annotation_width, 
			anno_simple_size = anno_simple_size, simple_anno_size_adjust = simple_anno_size_adjust)
	}

    return(.Object)
}

# == title
# Construct Row Annotations
#
# == param
# -... Pass to `HeatmapAnnotation`
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
# Construct Column Annotations
#
# == param
# -... Pass to `HeatmapAnnotation`
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
# Get a List of ColorMapping objects
#
# == param
# -object A `HeatmapAnnotation-class` object.
#
# == details
# Color mappings for visible simple annotations are only returned.
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
# Get a List of Annotation Legend Parameters
#
# == param
# -object A `HeatmapAnnotation-class` object.
#
# == details
# The annotation legend parameters for visible simple annotations are only returned.
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
# Draw the Heatmap Annotations
#
# == param
# -object A `HeatmapAnnotation-class` object.
# -index A vector of indices.
# -k The current slice index for the annotation if it is split.
# -n Total number of slices.
# -... Pass to `grid::viewport` which contains all the annotations.
# -test Is it in test mode? The value can be logical or a text which is plotted as the title of plot.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw",
	signature = "HeatmapAnnotation",
	definition = function(object, index, k = 1, n = 1, ..., 
		test = FALSE) {

	which = object@which
	n_anno = length(object@anno_list)
	anno_size = object@anno_size
	gap = object@gap

	if(is.character(test)) {
        test2 = TRUE
    } else {
        test2 = test
        test = ""
    }

    if(test2) {
    	grid.newpage()
    	if(which == "column") pushViewport(viewport(width = unit(1, "npc") - unit(3, "cm"), height = object@height))
    	if(which == "row") pushViewport(viewport(height = unit(1, "npc") - unit(3, "cm"), width = object@width))
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
		if(length(len)) {
			index = seq_len(len[1])
		} 
		if(!length(index)) {
			stop("Cannot infer the number of observations of the annotation.")
		}
    }

	if(which == "column") {
		# start from the last annoation which is put on bottom
		for(i in seq_len(n_anno)) {
			pushViewport(viewport(y = sum(anno_size[seq(i, n_anno)]) + sum(gap[seq(i, n_anno)]) - gap[n_anno], 
				height = anno_size[i], just = c("center", "top")))
			oe = try(draw(object@anno_list[[i]], index, k, n))
			if(inherits(oe, "try-error")) {
				cat("Error when drawing annotation '", object@anno_list[[i]]@name, "'\n", sep = "")
				stop_wrap(oe)
			}
			upViewport()
		}
	} else if(which == "row") {
		for(i in seq_len(n_anno)) {
			pushViewport(viewport(x = sum(anno_size[seq_len(i)]) + sum(gap[seq_len(i)]) - gap[i], width = anno_size[i], just = c("right", "center")))
			oe = try(draw(object@anno_list[[i]], index, k, n))
			if(inherits(oe, "try-error")) {
				cat("Error when drawing annotation '", object@anno_list[[i]]@name, "'\n", sep = "")
				stop_wrap(oe)
			}
			upViewport()
		}
	}
	if(test2) {
        grid.text(test, y = unit(1, "npc") + unit(2, "mm"), just = "bottom")
        # grid.rect(unit(0, "npc") - object@extended[2], unit(0, "npc") - object@extended[1], 
        #     width = unit(1, "npc") + object@extended[2] + object@extended[4],
        #     height = unit(1, "npc") + object@extended[1] + object@extended[3],
        #     just = c("left", "bottom"), gp = gpar(fill = "transparent", col = "red", lty = 2))
    }
	upViewport()
})

# == title
# Print the HeatmapAnnotation object
#
# == param
# -object A `HeatmapAnnotation-class` object.
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
	len = len[len > 0]
	cat("  items:", ifelse(length(len), len[1], "unknown"), "\n")
	cat("  width:", as.character(object@width), "\n")
	cat("  height:", as.character(object@height), "\n")
    cat("  this object is", ifelse(object@subsetable, "", "not"), "subsetable\n")
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
			if(object@anno_list[[i]]@fun@fun_name != "") {
				paste0(object@anno_list[[i]]@fun@fun_name, "()")
			} else {
				"AnnotationFunction"
			}
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
# Number of Observations
#
# == param
# -object The `HeatmapAnnotation-class` object.
# -... other arguments.
#
nobs.HeatmapAnnotation = function(object, ...) {
	n_anno = length(object@anno_list)
	len = sapply(seq_len(n_anno), function(i) {
		if(inherits(object@anno_list[[i]]@fun, "AnnotationFunction")) {
			nobs(object@anno_list[[i]]@fun)
		} else {
			NA
		}
	})
	len = len[!is.na(len)]
	len = len[len > 0]
	if(length(len)) {
		return(len[1])
	} else {
		NA
	}
}

# == title
# Add row annotations or heatmaps as a heatmap list
#
# == param
# -object A `HeatmapAnnotation-class` object.
# -x A `Heatmap-class` object, a `HeatmapAnnotation-class` object or a `HeatmapList-class` object.
# -direction Whether it is a horizontal add or a vertical add?
#
# == details
# There is a helper function ``+.AdditiveUnit`` for horizontal add or `\%v\%` for vertical add.
#
# == value
# A `HeatmapList-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "add_heatmap",
    signature = "HeatmapAnnotation",
    definition = function(object, x, direction = c("horizontal", "vertical")) {

    direction = match.arg(direction)[1]

    ht_list = new("HeatmapList")
    ht_list@direction = direction
    
    ht_list = add_heatmap(ht_list, object, direction = direction)
    ht_list = add_heatmap(ht_list, x, direction = direction)
    return(ht_list)

})

# == title
# Concatenate Heatmap Annotations
#
# == param
# -... `HeatmapAnnotation-class` objects.
# -gap gap between the annotations.
#
# == details
# The heatmap annotations should be same number of observations.
# 
# == example
# ha1 = HeatmapAnnotation(foo = 1:10)
# ha2 = HeatmapAnnotation(bar = anno_points(10:1))
# ha = c(ha1, ha2)
# ha
# ha3 = HeatmapAnnotation(sth = cbind(1:10, 10:1))
# ha = c(ha1, ha2, ha3, gap = unit(c(1, 4), "mm"))
# ha
c.HeatmapAnnotation = function(..., gap = unit(0, "mm")) {
	anno_list = list(...)
	n = length(anno_list)
	if(length(unique(sapply(anno_list, function(x) x@which))) != 1) {
		stop("All annotations should be all row annotation or all column annotation.")
	}
	if(length(gap) == 1) gap = rep(gap, n)
	if(length(gap) == n - 1) gap = unit.c(gap, unit(0, "mm"))
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
	if(x@which == "column") {
		x@height = convertHeight(sum(x@anno_size) + sum(x@gap) - x@gap[length(x@gap)], "mm")
	} else {
		x@width = convertWidth(sum(x@anno_size) + sum(x@gap) - x@gap[length(x@gap)], "mm")
	}

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

# == title
# Annotation Names
#
# == param
# -x A `HeatmapAnnotation-class` object.
#
# == example
# ha = HeatmapAnnotation(foo = 1:10, bar = anno_points(10:1))
# names(ha)
names.HeatmapAnnotation = function(x) {
	names(x@anno_list)
}

# == title
# Assign Annotation Names
#
# == param
# -x A `HeatmapAnnotation-class` object.
# -value A vector of new names.
#
# == example
# ha = HeatmapAnnotation(foo = 1:10, bar = anno_points(10:1))
# names(ha) = c("A", "B")
# names(ha)
"names<-.HeatmapAnnotation" = function(x, value) {
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


# == title
# Subset the HeatmapAnnotation object
#
# == param
# -x A `HeatmapAnnotation-class` object.
# -i Index of observations.
# -j Index of annotations.
#
# == example
# ha = HeatmapAnnotation(foo = 1:10, bar = anno_points(10:1),
# 	sth = cbind(1:10, 10:1))
# ha[1:5, ]
# ha[, c("foo", "bar")]
# ha[, 1:2]
# ha[1:5, c("foo", "sth")]
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

        size(x2) = sum(x2@anno_size) + sum(x2@gap) - x2@gap[length(x2@gap)]

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

        size(x2) = sum(x2@anno_size) + sum(x2@gap) - x2@gap[length(x2@gap)]

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

# == title
# Number of Annotations
#
# == param
# -x A `HeatmapAnnotation-class` object.
#
length.HeatmapAnnotation = function(x) {
	length(x@anno_list)
}


# == title
# Resize the Width or Height of Heatmap Annotations
#
# == param
# -object A `HeatmapAnnotation-class` object.
# -annotation_height A vector of of annotation heights in `grid::unit` class.
# -annotation_width A vector of of annotation widths in `grid::unit` class.
# -height The height of the complete heatmap annotation.
# -width The width of the complete heatmap annotation.
# -anno_simple_size The size of one line of the simple annotation.
# -simple_anno_size_adjust Whether adjust the size of the simple annotation?
#
# == details
# The function only adjust height for column annotations and width for row annotations.
#
# the basic rule is:
# 1. if ``annotation_height`` is set, it needs to be a vector and ``height`` is disabled. If all
#    ``annotation_height`` are absolute units, ``height`` is ignored.
# 2. if ``annotation_height`` contains non-absolute units, ``height`` also need to be set and the
#    non-absolute unit should be set in a simple form such as 1:10 or ``unit(1, "null")``.
# 3. ``anno_simple_size`` is only used when ``annotation_height`` is NULL.
# 4. if only ``height`` is set, non-simple annotation is adjusted while keep simple anntation unchanged.
# 5. if only ``height`` is set and all annotations are simple annotations, all anntations are adjusted.
#      and ``anno_simple_size`` is disabled.
# 6. If ``simple_anno_size_adjust`` is ``FALSE``, the size of the simple annotations will not change.
#
setMethod(f = "resize",
	signature = "HeatmapAnnotation",
	definition = function(object, 
	annotation_height = NULL, 
	annotation_width = NULL,
	height = NULL, 
	width = NULL, 
	anno_simple_size = ht_opt$anno_simple_size,
	simple_anno_size_adjust = FALSE) {

	if(object@which == "column") {
		if(!missing(width) || !missing(annotation_width)) {
			stop("Please use width() directly")
		}
	}
	if(object@which == "colrowumn") {
		if(!missing(height) || !missing(annotation_height)) {
			stop("Please use height() directly")
		}
	}

	if(!simple_anno_size_adjust) {
		if(all(sapply(object@anno_list, is_simple_annotation))) {
			return(object)
		}
	}

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
	size = slot(object, size_name)
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
		slot(object, size_name) = unit(sum(annotation_size_adjusted) + sum(gap), "mm")
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
			# size_adjusted = convertUnitFun(size_adjusted, "mm", valueOnly = TRUE)
			if(is.null(anno_simple_size)) {
				anno_simple_size = 5
			} else {
				anno_simple_size = convertUnitFun(anno_simple_size, "mm", valueOnly = TRUE)
			}
			if(size_adjusted <= sum(gap)) {
				stop(paste0(size_name, " you set is smaller than sum of gaps."))
			}

			## fix the size of simple annotation and zoom function annotations
			ts = size_adjusted - sum(gap) - sum(anno_size[l_simple_anno]*anno_simple_size/5)
			if(ts < 0) {
				stop(paste0(size_name, " you set is too small."))
			}
			anno_size2[!l_simple_anno] = anno_size[!l_simple_anno]/sum(anno_size[!l_simple_anno]) * ts
			anno_size2[l_simple_anno] = anno_size[l_simple_anno]*anno_simple_size/5

			size_adjusted = unit(size_adjusted, "mm")
			anno_size2 = unit(anno_size2, "mm")
		}
		slot(object, size_name) = size_adjusted
		object@anno_size = anno_size2
	}

	for(i in seq_along(object@anno_list)) {
		if(size_name == "width") {
			width(object@anno_list[[i]]) = object@anno_size[i]
		} else {
			height(object@anno_list[[i]]) = object@anno_size[i]
		}
	}

	return(object)
})

