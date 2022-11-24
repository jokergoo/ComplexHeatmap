

# == title 
# Class for a Single Annotation
#
# == details
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
        label = "ANY",
		color_mapping = "ANY",  # a ColorMapping object or NULL
		legend_param = "ANY", # a list or NULL, it contains parameters for color_mapping_legend
		fun = "ANY",
		show_legend = "logical",
		which = "character",
		name_to_data_vp = "logical",
		name_param = "list",
        is_anno_matrix = "logical",
        color_is_random = "logical",
        width = "ANY",
        height = "ANY",
        extended = "ANY",
        subsettable = "logical"
	),
	prototype = list(
		color_mapping = NULL,
		fun = function(index) NULL,
		show_legend = TRUE,
        color_is_random = FALSE,
		name_to_data_vp = FALSE,
        extended = unit(c(0, 0, 0, 0), "mm"),
        subsettable = FALSE
	)
)

# == title
# Constructor Method for SingleAnnotation Class
#
# == param
# -name Name for the annotation. If it is not specified, an internal name is assigned.
# -value A vector or a matrix of discrete or continuous values.
# -col Colors corresponding to ``value``. If the mapping is discrete, the value of ``col``
#      should be a named vector; If the mapping is continuous, the value of ``col`` should be 
#      a color mapping function.
# -fun A user-defined function to add annotation graphics. The argument of this function should be at least 
#      a vector of index that corresponds to rows or columns. Normally the function should be 
#      constructed by `AnnotationFunction` if you want the annotation supports splitting. 
#      See **Details** for more explanation.
# -label Label for the annotation. By default is the annotation name.
# -na_col Color for ``NA`` values in the simple annotations.
# -which Whether the annotation is a row annotation or a column annotation?
# -show_legend If it is a simple annotation, whether show legend in the final heatmap?
# -gp Since simple annotation is represented as rows of grids. This argument controls graphic parameters for the simple annotation.
#     The ``fill`` parameter is ignored here.
# -border border, only work for simple annotation
# -legend_param Parameters for the legend. See `color_mapping_legend,ColorMapping-method` for all possible options.
# -show_name Whether show annotation name?
# -name_gp Graphic parameters for annotation name.
# -name_offset Offset to the annotation, a `grid::unit` object.
# -name_side 'right' and 'left' for column annotations and 'top' and 'bottom' for row annotations
# -name_rot Rotation of the annotation name.
# -simple_anno_size size of the simple annotation.
# -width The width of the plotting region (the viewport) that the annotation is drawn. If it is a row annotation,
#        the width must be an absolute unit.
# -height The height of the plotting region (the viewport) that the annotation is drawn. If it is a column annotation,
#        the width must be an absolute unit.
#
# == details
# A single annotation is a basic unit of complex heatmap annotations where the heamtap annotations
# are always a list of single annotations. An annotation can be simply heatmap-like (here we call
# it simple annotation) or more complex like points, lines, boxes (for which we call it complex annotation).
#
# In the `SingleAnnotation` constructor, ``value``, ``col``, ``na_col`` are used to construct a `anno_simple`
# annotation funciton which is generated internally by `AnnotationFunction`. The legend of the simple annotation
# can be automatcally generated,
#
# For construcing a complex annotation, users need to use ``fun`` which is a user-defind function. Normally it 
# is constucted by `AnnotationFunction`. One big advantage for using `AnnotationFunction` is the annotation function
# or the graphics drawn by the annotation function can be split according to row splitting or column splitting of
# the heatmap. Users can also provide a "pure" function which is a normal R function for the ``fun`` argument. 
# The function only needs one argument which is a vector of index for rows or columns depending whether it is 
# a row annotation or column annotation. The other two optional arguments are the current slice index and total
# number of slices. See **Examples** section for an example. If it is a normal R function, it will be constructed
# into the `AnnotationFunction-class` object internally.
#
# The `SingleAnnotation-class` is a simple wrapper on top of `AnnotationFunction-class` only with annotation 
# name added.
#
# The class also stored the "extended area" relative to the area for the annotation graphics. The extended areas
# are those created by annotation names and axes.
#
# == seealso
# There are following built-in annotation functions that can be directly used to generate complex annotations: 
# `anno_simple`, `anno_points`, `anno_lines`, `anno_barplot`, `anno_histogram`, `anno_boxplot`, `anno_density`, `anno_text`,
# `anno_joyplot`, `anno_horizon`, `anno_image`, `anno_block`, `anno_summary` and `anno_mark`.
# 
# == value
# A `SingleAnnotation-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# ha = SingleAnnotation(value = 1:10)
# draw(ha, test = "single column annotation")
#
# m = cbind(1:10, 10:1)
# colnames(m) = c("a", "b")
# ha = SingleAnnotation(value = m)
# draw(ha, test = "matrix as column annotation")
#
# anno = anno_barplot(matrix(nc = 2, c(1:10, 10:1)))
# ha = SingleAnnotation(fun = anno)
# draw(ha, test = "anno_barplot as input")
#
# fun = local({
#     # because there variables outside the function for use, we put it a local environment
#     value = 1:10
#     function(index, k = 1, n = 1) {
#         pushViewport(viewport(xscale = c(0.5, length(index) + 0.5), yscale = range(value)))
#         grid.points(seq_along(index), value[index])
#         grid.rect()
#         if(k == 1) grid.yaxis()
#         popViewport()
#     }
# })
# ha = SingleAnnotation(fun = fun, height = unit(4, "cm"))
# draw(ha, index = 1:10, test = "self-defined function")
SingleAnnotation = function(name, value, col, fun, 
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
    width = NULL, height = NULL) {

    .ENV$current_annotation_which = NULL
	which = match.arg(which)[1]
    .ENV$current_annotation_which = which
    
    on.exit(.ENV$current_annotation_which <- NULL)

    verbose = ht_opt$verbose

    # re-define some of the argument values according to global settings
    called_args = names(as.list(match.call())[-1])
    if("legend_param" %in% called_args) {
        for(opt_name in setdiff(c("title_gp", "title_position", "labels_gp", "grid_width", "grid_height", "border"), names(legend_param))) {
            opt_name2 = paste0("legend_", opt_name)
            if(!is.null(ht_opt(opt_name2)))
                legend_param[[opt_name]] = ht_opt(opt_name2)
        }
    } else {
        for(opt_name in c("title_gp", "title_position", "labels_gp", "grid_width", "grid_height", "border")) {
            opt_name2 = paste0("legend_", opt_name)
            if(!is.null(ht_opt(opt_name2)))
                legend_param[[opt_name]] = ht_opt(opt_name2)
        }
    }

	.Object = new("SingleAnnotation")
	.Object@which = which
    
	if(missing(name)) {
        name = paste0("anno", get_annotation_index() + 1)
        increase_annotation_index()
    }
    .Object@name = name
    if(is.null(label)) {
        label = name
    }
    .Object@label = label

    if(verbose) qqcat("create a SingleAnnotation with name '@{name}'\n")

    .Object@is_anno_matrix = FALSE
    use_mat_column_names = FALSE
            
    if(!missing(value)) {
        value2 = value
    
        if(verbose) qqcat("@{name}: annotation value is vector/matrix\n")
        if(is.logical(value)) {
            if(is.matrix(value)) {
                oa = attributes(value)
                value = as.character(value)
                attributes(value) = oa
            } else {
                value = as.character(value)
            }
            if(verbose) qqcat("@{name}: annotation value is logical, convert to character\n")
        }
        if(is.factor(value)) {
            value = as.vector(value)
            if(verbose) qqcat("@{name}: annotation value is factor, convert to character\n")
        }
        if(is.matrix(value)) {
            .Object@is_anno_matrix = TRUE
            attr(.Object@is_anno_matrix, "column_names") = colnames(value)
            attr(.Object@is_anno_matrix, "k") = ncol(value)
            if(length(colnames(value))) {
                use_mat_column_names = TRUE
            }
            use_mat_nc = ncol(value)
            if(verbose) qqcat("@{name}: annotation value is a matrix\n")
        }
    }

    # if SingleAnnotation is called by HeatmapAnnotation, following two variables are all TRUE
    is_name_offset_called = !is.null(name_offset)
    is_name_rot_called = !is.null(name_rot)
    anno_fun_extend = unit(c(0, 0, 0, 0), "mm")
    if(!missing(fun)) {
        if(inherits(fun, "AnnotationFunction")) {
            anno_fun_extend = fun@extended
            if(verbose) qqcat("@{name}: annotation is a AnnotationFunction object\n")

            if(!fun@show_name) show_name = fun@show_name
        } else {
            fun = AnnotationFunction(fun = fun, which = which)
            anno_fun_extend = fun@extended
            if(verbose) qqcat("@{name}: annotation is a user-defined function\n")
        }
    }

    if(!is.null(name_offset)) {
        if(is.character(name_offset)) {
            name_offset = to_unit(name_offset)
        }
    } else {
        name_offset = unit(1, "mm")
    }
    if(is.null(name_rot)) name_rot = ifelse(which == "column", 0, 90)

    name_rot = name_rot %% 360

    anno_name = as.character(label)
    if(which == "column") {
        if(verbose) qqcat("@{name}: it is a column annotation\n")
    	if(!name_side %in% c("left", "right")) {
    		stop_wrap(qq("@{name}: `name_side` should be 'left' or 'right' when it is a column annotation."))
    	}
        if(verbose) qqcat("@{name}: adjust positions of annotation names\n")
    	if(name_side == "left") {
            if(unit_to_numeric(anno_fun_extend[2]) > 0) {
                if(!is_name_offset_called) {
                    name_offset = name_offset + anno_fun_extend[2]
                }
                if(!is_name_rot_called) {
                    name_rot = 90
                }
            }

            if(use_mat_column_names) {
                name_x = unit(rep(0, use_mat_nc), "npc") - name_offset
                name_y = unit((use_mat_nc - seq_len(use_mat_nc) + 0.5)/use_mat_nc, "npc")

                anno_name = colnames(value)
            } else {
                name_x = unit(0, "npc") - name_offset
                name_y = unit(0.5, "npc")
            }
            if(name_rot == 0) {
                name_just = "right"
            } else if(name_rot == 90) {
                name_just = "bottom"
            } else if(name_rot == 180) {
                name_just = "left"
            } else if(name_rot == 270) {
                name_just = "top"
            } else if(name_rot < 90 || name_rot > 270) {
                name_just = "right"
            } else if(name_rot > 90 && name_rot < 270) {
                name_rot  = (180 - name_rot) %% 360
                name_just = "right"
            } else {
                name_just = "right"
            }
    	} else {
            if(unit_to_numeric(anno_fun_extend[4]) > 0) {
                if(!is_name_offset_called) {
                    name_offset = name_offset + anno_fun_extend[4]
                }
                if(!is_name_rot_called) {
                    name_rot = 90
                }
            }

            if(use_mat_column_names) {
                name_x = unit(rep(1, use_mat_nc), "npc") + name_offset
                name_y = unit((use_mat_nc - seq_len(use_mat_nc) + 0.5)/use_mat_nc, "npc")

                anno_name = colnames(value)
            } else {
        		name_x = unit(1, "npc") + name_offset
        		name_y = unit(0.5, "npc")
            }
            if(name_rot == 0) {
                name_just = "left"
            } else if(name_rot == 90) {
                name_just = "top"
            } else if(name_rot == 180) {
                name_just = "right"
            } else if(name_rot == 270) {
                name_just = "bottom"
            } else if(name_rot < 90 || name_rot > 270) {
                name_just = "left"
            } else if(name_rot > 90 && name_rot < 270) {
                name_rot  = (180 - name_rot) %% 360
                name_just = "left"
            } else {
                name_just = "left"
            }
    	}
    } else if(which == "row") {
        if(verbose) qqcat("@{name}: it is a row annotation\n")
    	if(!name_side %in% c("top", "bottom")) {
    		stop_wrap(qq("@{name}: `name_side` should be 'left' or 'right' when it is a column annotation."))
    	}
        if(verbose) qqcat("@{name}: adjust positions of annotation names\n")
    	if(name_side == "top") {
            if(unit_to_numeric(anno_fun_extend[3]) > 0) {
                if(!is_name_offset_called) {
                    name_offset = name_offset + anno_fun_extend[3]
                }
                if(!is_name_rot_called) {
                    name_rot = 0
                }
            }

            if(use_mat_column_names) {
                name_x = unit((seq_len(use_mat_nc) - 0.5)/use_mat_nc, "npc")
                name_y = unit(rep(1, use_mat_nc), "npc") + name_offset

                anno_name = colnames(value)
            } else {
        		name_x = unit(0.5, "npc")
        		name_y = unit(1, "npc") + name_offset
            }
            if(name_rot == 0) {
                name_just = "bottom"
            } else if(name_rot == 90) {
                name_just = "left"
            } else if(name_rot == 180) {
                name_just = "top"
            } else if(name_rot == 270) {
                name_just = "right"
            } else if(name_rot < 90) {
                name_just = "left"
            } else if(name_rot > 90 && name_rot < 180) {
                name_rot  = (90 - name_rot) %% 360
                name_just = "right"
            } else if(name_rot > 180 && name_rot < 270) {
                name_rot  = (- name_rot) %% 360
                name_just = "left"
            } else if(name_rot > 270) {
                name_rot  = (- name_rot) %% 360
                name_just = "left"
            } else {
                name_just = "bottom"
            }
    	} else {
            if(unit_to_numeric(anno_fun_extend[1]) > 0) {
                if(!is_name_offset_called) {
                    name_offset = name_offset + anno_fun_extend[1]
                }
                if(!is_name_rot_called) {
                    name_rot = 0
                }
            }
            if(use_mat_column_names) {
                name_x = unit((seq_len(use_mat_nc) - 0.5)/use_mat_nc, "npc")
                name_y = unit(rep(0, use_mat_nc), "npc") - name_offset

                anno_name = colnames(value)
            } else {
        		name_x = unit(0.5, "npc")
        		name_y = unit(0, "npc") - name_offset
            }
            if(name_rot == 0) {
                name_just = "top"
            } else if(name_rot == 90) {
                name_just = "right"
            } else if(name_rot == 180) {
                name_just = "bottom"
            } else if(name_rot == 270) {
                name_just = "left"
            } else if(name_rot < 90) {
                name_just = "right"
            } else if(name_rot > 270) {
                name_just = "left"
            } else if(name_rot > 90 && name_rot < 180) {
                name_rot  = (- name_rot) %% 360
                name_just = "left"
            } else if(name_rot > 180 && name_rot < 270) {
                name_rot  = (- name_rot) %% 360
                name_just = "right"
            } else {
                name_just = "left"
            }
    	}
    }

    name_param = list(show = show_name,
                      label = anno_name,
					  x = name_x,
					  y = name_y,
                      offset = name_offset,
					  just = name_just,
                      gp = check_gp(name_gp),
                      rot = name_rot,
                      side = name_side)

    # get defaults for name settings
    if(verbose) qqcat("@{name}: calcualte extensions caused by annotation name\n")
    extended = unit(c(0, 0, 0, 0), "mm")
    if(name_param$show) {
        if(which == "column") {
            if(name_param$rot %in% c(0, 180)) {
                text_width = convertWidth(max_text_width(name_param$label, gp = name_gp, rot = name_param$rot) + name_param$offset, "mm")
            } else if(name_param$rot %in% c(90, 270)) {
                text_width = convertHeight(max_text_height(name_param$label, gp = name_gp) + name_param$offset, "mm")
            } else {
                text_width = convertWidth(max_text_width(name_param$label, gp = name_gp, rot = name_param$rot) + name_param$offset, "mm")
            }
            if(name_param$side == "left") {
                extended[2] = text_width
            } else if(name_param$side == "right") {
                extended[4] = text_width
            }
        } else if(which == "row") {
            if(name_param$rot %in% c(0, 180)) {
                text_width = convertHeight(max_text_height(name_param$label, gp = name_gp, rot = name_param$rot) + name_param$offset, "mm")
            } else if(name_param$rot %in% c(90, 270)) {
                text_width = convertHeight(max_text_width(name_param$label, gp = name_gp) + name_param$offset, "mm")
            } else {
                text_width = convertHeight(max_text_height(name_param$label, gp = name_gp, rot = name_param$rot) + name_param$offset, "mm")
            }
            if(name_param$side == "bottom") {
                extended[1] = text_width
            } else if(name_param$side == "top") {
                extended[3] = text_width
            }
        }
    }
    for(i in 1:4) {
        extended[[i]] = unit(max(unit_to_numeric(anno_fun_extend[i]), unit_to_numeric(extended[i])), "mm")
    }
    .Object@extended = extended

    .Object@name_param = name_param

    gp = check_gp(gp)
    if(!is.null(gp$fill)) {
    	stop_wrap(qq("@{name}: You should not set `fill`."))
    }

    if(missing(fun)) {
        color_is_random = FALSE
    	if(missing(col)) {
    		col = default_col(value)
            color_is_random = TRUE
            if(verbose) qqcat("@{name}: use randomly generated colors\n")
    	}

        cm_name = name
        if(!inherits(.Object@label, "gridtext")) {
            cm_name = as.character(.Object@label)
        }

    	if(is.atomic(col)) {
    	    if(is.null(names(col))) {
                if(is.factor(value2)) {
                    names(col) = levels(value2)
                    if(verbose) qqcat("@{names}: add names for discrete color mapping\n")
                } else if(length(col) == length(unique(value))) {
                    names(col) = sort(unique(value))
                    if(verbose) qqcat("@{names}: add names for discrete color mapping\n")
                } else if(is.numeric(value)) {
                    col = colorRamp2(seq(min(value, na.rm = TRUE), max(value, na.rm = TRUE), length.out = length(col)), col)
                    if(verbose) qqcat("@{name}: assume as a continuous color mapping\n")
                }
            }
            if(is.function(col)) {
                color_mapping = ColorMapping(name = cm_name, col_fun = col, na_col = na_col)
            } else {
                full_col = col
                if(is.factor(value2)) {
                    col = col[intersect(c(levels(value2), "_NA_"), names(col))]
                } else {
                    col = col[intersect(c(sort(names(col)), "_NA_"), as.character(value))]
                }
        		if("_NA_" %in% names(col)) {
        			na_col = col["_NA_"]
        			col = col[names(col) != "_NA_"]
        		}
                color_mapping = ColorMapping(name = cm_name, colors = col, na_col = na_col, full_col = full_col)
            }
        } else if(is.function(col)) {
            color_mapping = ColorMapping(name = cm_name, col_fun = col, na_col = na_col)
        } else {
            stop_wrap("The color mapping should be a named vector or a function.")
        }

        .Object@color_mapping = color_mapping
        .Object@color_is_random = color_is_random
        if(is.null(legend_param)) legend_param = list()
        .Object@legend_param = legend_param
        value = value

        if(verbose) qqcat("@{name}: generate AnnotationFunction for simple annotation values by anno_simple()\n")
        .Object@fun = anno_simple(value, col = color_mapping, which = which, na_col = na_col, gp = gp, border = border, simple_anno_size = simple_anno_size)
        if(missing(width)) {
            .Object@width = .Object@fun@width
        } else {
            .Object@width = width
            .Object@fun@width = width
        }
        if(missing(height)) {
            .Object@height = .Object@fun@height
        } else {
            .Object@height = height
            .Object@fun@height = height
        }
		
		.Object@show_legend = show_legend
        .Object@subsettable = TRUE
    } else {
        
        f_which = fun@which
    	if(!is.null(f_which)) {
    		fun_name = fun@fun_name
    		if(f_which != which) {
                if(fun_name %in% c("anno_barplot", "anno_boxplot", "anno_density", "anno_histogram", "anno_points", "anno_text")) {
                    stop_wrap(qq("You are putting @{fun_name} as @{which} annotations, you need to set 'which' argument to '@{which}' as well, or use the helper function @{which}_@{fun_name}()."))
                } else {
                    stop_wrap(qq("You are putting @{fun_name} as @{which} annotations, you need to set 'which' argument to '@{which}' as well."))
                }
    		}
        }
        
        if(verbose) qqcat("@{name}: calcualte width/height of SingleAnnotation based on the annotation function\n")
    	.Object@fun = fun
    	.Object@show_legend = FALSE

        if(is.null(width)) {
            .Object@width = .Object@fun@width
        } else {
            .Object@width = width
            .Object@fun@width = width
        }
        if(is.null(height)) {
            .Object@height = .Object@fun@height
        } else {
            .Object@height = height
            .Object@fun@height = height
        }
        .Object@subsettable = .Object@fun@subsettable
    }

    return(.Object)
}

# == title
# Draw the Single Annotation
#
# == param
# -object A `SingleAnnotation-class` object.
# -index A vector of indices.
# -k The index of the slice.
# -n Total number of slices. ``k`` and ``n`` are used to adjust annotation names. E.g.
#    if ``k`` is 2 and ``n`` is 3, the annotation names are not drawn.
# -test Is it in test mode? The value can be logical or a text which is plotted as the title of plot.
# -anno_mark_param It contains specific parameters for drawing `anno_mark`.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw",
	signature = "SingleAnnotation",
	definition = function(object, index, k = 1, n = 1, test = FALSE,
        anno_mark_param = list()) {

    ## make the special anno_mark when the anotation is split
    if(object@fun@fun_name %in% c("anno_mark", "anno_zoom") && length(anno_mark_param) > 0) {
        if(k > 1) {
            return(invisible(NULL))
        } else {
            ## change values for .pos and .scale for anno_mark
            object@fun@var_env$.pos = anno_mark_param$.pos
            object@fun@var_env$.scale = anno_mark_param$.scale
            pushViewport(viewport(x = anno_mark_param$vp_x, y = anno_mark_param$vp_y, width = anno_mark_param$vp_width, height = anno_mark_param$vp_height, just = anno_mark_param$vp_just))
            draw(object@fun, index = anno_mark_param$index)
            upViewport()
            return(invisible(NULL))
        }
    }

    if(is.character(test)) {
        test2 = TRUE
    } else {
        test2 = test
        test = ""
    }

    verbose = ht_opt$verbose

    ## it draws annotation names, create viewports with names
    if(test2) {
        grid.newpage()
        pushViewport(viewport(width = unit(1, "npc") - unit(4, "cm"), 
                              height = unit(1, "npc") - unit(4, "cm")))
    }

    if(missing(index)) {
        if(has_AnnotationFunction(object)) {
            if(object@fun@n == 0) {
                stop_wrap("Cannot infer the number of Observations in the annotation function, you need to provide `index`.")
            }
            index = seq_len(object@fun@n)
        }
    }

    anno_height = object@height
    anno_width = object@width
    
	# names should be passed to the data viewport
	if(has_AnnotationFunction(object)) {
        if(object@which == "column") {
            data_scale = list(x = c(0.5, length(index) + 0.5), y = object@fun@data_scale)
        } else {
            data_scale = list(y = c(0.5, length(index) + 0.5), x = object@fun@data_scale)
        }
    } else {
        data_scale = list(x = c(0, 1), y = c(0, 1))
    }
	pushViewport(viewport(width = anno_width, height = anno_height, 
        name = paste("annotation", object@name, k, sep = "_"),
        xscale = data_scale$x, yscale = data_scale$y))
    
    if(verbose) qqcat("execute annotation function\n")
    draw(object@fun, index = index, k = k, n = n)
    
	# add annotation name
    draw_name = object@name_param$show
	if(object@name_param$show && n > 1) {
        if(object@which == "row") {
            if(k == n && object@name_param$side == "bottom") {
                draw_name = TRUE
            } else if(k == 1 && object@name_param$side == "top") {
                draw_name = TRUE
            } else {
                draw_name = FALSE
            }
        } else if(object@which == "column") {
            if(k == 1 && object@name_param$side == "left") {
                draw_name = TRUE
            } else if(k == n && object@name_param$side == "right") {
                draw_name = TRUE
            } else {
                draw_name = FALSE
            }
        }
    }

    if(draw_name) {
        if(verbose) qqcat("draw annotation name\n")
        if(is_matrix_annotation(object)) {
            if(!is.null(attr(object@is_anno_matrix, "column_names"))) {
                anno_mat_column_names = attr(object@is_anno_matrix, "column_names")
                grid.text(anno_mat_column_names, x = object@name_param$x, y = object@name_param$y, just = object@name_param$just, 
                    rot = object@name_param$rot, gp = object@name_param$gp)
            } else {
                if(object@which == "column") {
                    grid.text(object@label, x = object@name_param$x[1], y = unit(0.5, "npc"), just = object@name_param$just, 
                        rot = object@name_param$rot, gp = object@name_param$gp)
                } else {
                    grid.text(object@label, x = unit(0.5, "npc"), y = object@name_param$y[1], just = object@name_param$just, 
                        rot = object@name_param$rot, gp = object@name_param$gp)
                }
            }
        } else {
    		grid.text(object@label, x = object@name_param$x, y = object@name_param$y, just = object@name_param$just, 
    			rot = object@name_param$rot, gp = object@name_param$gp)
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

    if(test2) {
        upViewport()
    }
})

# == title
# Print the SingleAnnotation object
#
# == param
# -object A `SingleAnnotation-class` object.
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
	
    if(is_fun_annotation(object)) {
        if(has_AnnotationFunction(object)) {
            fun_name = object@fun@fun_name
            fun_name = paste0(fun_name, "()")
        } else {
            fun_name = "self-defined"
        }
		cat("A single annotation with", fun_name, "function\n")
		cat("  name:", object@name, "\n")
		cat("  position:", object@which, "\n")
        cat("  no legend\n")
        if(has_AnnotationFunction(object)) {
            n = object@fun@n
            if(!is.null(n)) cat("  items:", n, "\n")
        }  
	} else {
		cat("A single annotation with", object@color_mapping@type, "color mapping\n")
		cat("  name:", object@name, "\n")
		cat("  position:", object@which, "\n")
		cat("  show legend:", object@show_legend, "\n")
        cat("  items:", object@fun@n, "\n")
        if(is_matrix_annotation(object)) {
            cat("  a matrix with", attr(object@is_anno_matrix, "k"), "columns\n")
        }
        if(object@color_is_random) {
            cat("  color is randomly generated\n")
        }
	}

    cat("  width:", as.character(object@width), "\n")
    cat("  height:", as.character(object@height), "\n")
    cat("  this object is", ifelse(object@subsettable, "\b", "not"), "subsettable\n")
    dirt = c("bottom", "left", "top", "right")
    for(i in 1:4) {
        if(!identical(unit(0, "mm"), object@extended[i])) {
            cat(" ", as.character(object@extended[i]), "extension on the", dirt[i], "\n")
        }
    }
})


is_simple_annotation = function(single_anno) {
    !is_fun_annotation(single_anno) && !is_matrix_annotation(single_anno)
}

is_matrix_annotation = function(single_anno) {
    single_anno@is_anno_matrix
}

is_fun_annotation = function(single_anno) {
    is.null(single_anno@color_mapping)
}

has_AnnotationFunction = function(single_anno) {
    if(is.null(single_anno@fun)) {
        FALSE
    } else {
        inherits(single_anno@fun, "AnnotationFunction")
    }
}


## subset method for .SingleAnnotation-class
## column annotation only allows column subsetting and row annotaiton only allows row subsetting

# == title
# Subset an SingleAnnotation Object
#
# == param
# -x An `SingleAnnotation-class` object.
# -i A vector of indices.
#
# == details
# The SingleAnnotation class object is subsettable only if the containing `AnnotationFunction-class`
# object is subsettable. All the ``anno_*`` functions are subsettable, so if the SingleAnnotation object
# is constructed by one of these functions, it is also subsettable.
#
# == example
# ha = SingleAnnotation(value = 1:10)
# ha[1:5]
# draw(ha[1:5], test = "ha[1:5]")
"[.SingleAnnotation" = function(x, i) {
    # only allow subsetting for anno_* functions defined in ComplexHeatmap
    if(nargs() == 2) {
        x2 = x
        if(inherits(x@fun, "AnnotationFunction")) {
            if(x@fun@subsettable) {
                x2@fun = x@fun[i]
                if(x@which == "row") {
                    x2@width = x2@fun@width
                } else {
                    x2@height = x2@fun@height
                }
                return(x2)
            }
        }
        stop_wrap("This SingleAnnotation object is not allowed for subsetting.")

    } else if(nargs() == 1) {
        return(x)
    }
}

# == title
# Copy the SingleAnnotation object
#
# == param
# -object The `SingleAnnotation-class` object.
#
# == details
# Since the SingleAnnotation object always contains an `AnnotationFunction-class` object,
# it calls `copy_all,AnnotationFunction-method` to hard copy the variable environment.
setMethod(f = "copy_all",
    signature = "SingleAnnotation",
    definition = function(object) {

    x2 = object
    x2@fun = copy_all(object@fun)
    return(x2)
})


# == title
# Number of Observations
#
# == param
# -object The `SingleAnnotation-class` object.
# -... Other arguments.
#
# == details
# It returns the ``n`` slot of the annotaton function. If it does not exist, it returns ``NA``.
nobs.SingleAnnotation = function(object, ...) {
    if(is.na(object@fun@n)) {
        NA
    } else {
        if(object@fun@n > 0) {
            object@fun@n
        } else {
            NA
        }
    }
}

names.SingleAnnotation = function(x) {
    value = x@fun@var_env$value
    if(!is.null(dim(value))) {
        rownames(value)
    } else {
        names(value)
    }
}
