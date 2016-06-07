

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
		color_mapping = "ANY",  # a ColorMapping object or NULL
		color_mapping_param = "ANY", # a list or NULL, it contains parameters for color_mapping_legend
		fun = "function",
		show_legend = "logical",
		which = "character",
		name_to_data_vp = "logical",
		name_param = "list"
	),
	prototype = list(
		color_mapping = NULL,
		fun = function(index) NULL,
		show_legend = TRUE,
		name_to_data_vp = FALSE
	)
)

# == title
# Constructor method for SingleAnnotation class
#
# == param
# -name name for this annotation. If it is not specified, an internal name is assigned.
# -value A vector of discrete or continuous annotation.
# -col colors corresponding to ``value``. If the mapping is discrete mapping, the value of ``col``
#      should be a vector; If the mapping is continuous mapping, the value of ``col`` should be 
#      a color mapping function. 
# -fun a self-defined function to add annotation graphics. The argument of this function should only 
#      be a vector of index that corresponds to rows or columns.
# -na_col color for ``NA`` values in simple annotations.
# -which is the annotation a row annotation or a column annotation?
# -show_legend if it is a simple annotation, whether show legend when making the complete heatmap.
# -gp Since simple annotation is represented as a row of grids. This argument controls graphic parameters for the simple annotation.
# -legend_param parameters for the legend. See `color_mapping_legend,ColorMapping-method` for options.
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
# In the case that row annotations are splitted by rows, ``index`` corresponding to row orders in each row-slice
# and ``fun`` will be applied on each of the row slices.
#
# One thing that users should be careful is the difference of coordinates when the annotation is a row
# annotation or a column annotation. 
#
# == seealso
# There are following built-in annotation functions that can be used to generate complex annotations: 
# `anno_points`, `anno_barplot`, `anno_histogram`, `anno_boxplot`, `anno_density`, `anno_text` and `anno_link`.
# 
# == value
# A `SingleAnnotation-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
SingleAnnotation = function(name, value, col, fun, 
	na_col = "grey",
	which = c("column", "row"), 
	show_legend = TRUE, 
	gp = gpar(col = NA), 
	legend_param = list(),
	show_name = TRUE, 
	name_gp = gpar(fontsize = 12),
	name_offset = unit(2, "mm"),
	name_rot = 0,
	name_side = ifelse(which == "column", "right", "bottom")) {

	# re-define some of the argument values according to global settings
    called_args = names(as.list(match.call())[-1])
    if("legend_param" %in% called_args) {
        for(opt_name in setdiff(c("title_gp", "title_position", "labels_gp", "grid_width", "grid_height", "grid_border"), names(legend_param))) {
            opt_name2 = paste0("annotation_legend_", opt_name)
            if(!is.null(ht_global_opt(opt_name2)))
                legend_param[[opt_name]] = ht_global_opt(opt_name2)
        }
    } else {
        for(opt_name in c("title_gp", "title_position", "labels_gp", "grid_width", "grid_height", "grid_border")) {
            opt_name2 = paste0("annotation_legend_", opt_name)
            if(!is.null(ht_global_opt(opt_name2)))
                legend_param[[opt_name]] = ht_global_opt(opt_name2)
        }
    }

	.Object = new("SingleAnnotation")

	which = match.arg(which)[1]
	.Object@which = which

	if(missing(name)) {
        name = paste0("anno", get_annotation_index() + 1)
        increase_annotation_index()
    }
    .Object@name = name
    .Object@name_param = list(show = show_name,
    	                      gp = check_gp(name_gp),
    	                      offset = name_offset,
    	                      rot = name_rot,
    	                      side = name_side)

    gp = check_gp(gp)
    if(!is.null(gp$fill)) {
    	stop("You should not set `fill`.")
    }

    if(!missing(value)) {
	    if(is.logical(value)) {
	    	value = as.character(value)
	    }
	    if(is.factor(value)) {
            value = as.vector(value)
        }
	}

    if(missing(fun)) {
    	if(missing(col)) {
    		col = default_col(value)
    	}
    	if(is.atomic(col)) {
    		if("_NA_" %in% names(col)) {
    			na_col = col["_NA_"]
    			col = col[names(col) != "_NA_"]
    		}
            color_mapping = ColorMapping(name = name, colors = col, na_col = na_col)
        } else if(is.function(col)) {
            color_mapping = ColorMapping(name = name, col_fun = col, na_col = na_col)
        }

        .Object@color_mapping = color_mapping
        if(is.null(legend_param)) legend_param = list()
        .Object@color_mapping_param = legend_param
        value = value

        if(which == "column") {
	        .Object@fun = function(index) {
	        	n = length(index)
				x = (seq_len(n) - 0.5) / n
				fill = map_to_colors(color_mapping, value[index])
				#l = which(!is.na(value[index]))
				grid.rect(x, y = 0.5, width = 1/n, height = 1, gp = do.call("gpar", c(list(fill = fill), gp)))
			}
		} else {
			.Object@fun = function(index, k = NULL, N = NULL) {
				n = length(index)
				y = (n - seq_len(n) + 0.5) / n
				fill = map_to_colors(color_mapping, value[index])
				#l = which(!is.na(value[index]))
				grid.rect(x = 0.5, y, height = 1/n, width = 1, gp = do.call("gpar", c(list(fill = fill), gp)))
			}
		}

		.Object@show_legend = show_legend
    } else {
    	f_which = attr(fun, "which")
    	if(!is.null(f_which)) {
    		fun_name = attr(fun, "fun")
    		if(f_which != which) {
    			stop(paste0("You are putting ", fun_name, "() as ", which, " annotations, you need to set 'which' argument to '", which, "' as well,\nor use the helper function ", which, "_", fun_name, "()."))
    		}
    	}
    	.Object@fun = fun
    	.Object@show_legend = FALSE

    	anno_fun = attr(fun, "fun")
    	if(!is.null(anno_fun)) {
    		if(anno_fun %in% c("anno_points", "anno_barplot", "anno_boxplot")) {
    			.Object@name_to_data_vp = TRUE
    		}
    	}
    }

    if(which == "row") {
    	if(length(formals(.Object@fun)) == 1) {
    		formals(.Object@fun) = alist(index = , k = NULL, N = NULL, vp_name = NULL)
    	} else if(length(formals(.Object@fun)) == 2) {  # assume index and k are specified
    		formals(.Object@fun) = alist(index = , k = , N = NULL, vp_name = NULL)
    	}
    } else {
    	formals(.Object@fun) = alist(index = , vp_name = NULL)
    }

    return(.Object)
}

# == title
# Draw the single annotation
#
# == param
# -object a `SingleAnnotation-class` object.
# -index a vector of orders
# -k if row annotation is splitted, the value identifies which row slice. It is only used for the naems of the viewport
#    which contains the annotation graphics.
# -n total number of row slices
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
	definition = function(object, index, k = NULL, n = NULL) {

	# names should be passed to the data viewport
	if(object@name_to_data_vp) {
		if(is.null(k)) {
			pushViewport(viewport())
			object@fun(index, vp_name = paste("annotation", object@name, sep = "_"))
		} else {
			pushViewport(viewport())
			object@fun(index, k, n, vp_name = paste("annotation", object@name, k, sep = "_"))
		}
	} else {
		if(is.null(k)) {
			pushViewport(viewport(name = paste("annotation", object@name, sep = "_")))
			object@fun(index)
		} else {
			pushViewport(viewport(name = paste("annotation", object@name, k, sep = "_")))
			object@fun(index, k, n)
		}
	}
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
