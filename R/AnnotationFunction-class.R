
# == title
# The AnnotationFunction Class
#
# == details
# The heatmap annotation is basically graphics aligned to the heatmap columns
# if it is column annotation or heatmap rows if it is row annotation, while
# there is no restrictions for the graphic types, e.g. it can be heatmap-like
# annotation or points. Here the AnnotationFunction class is designed for
# creating complex and flexible annotation graphics. As the main part of the class, it uses
# a user-defined function to define the graphics. It also keeps information of
# the size of the plotting regions of the annotation. And most importantly, it
# allows subsetting of the annotation to draw a subset of the graphics, which
# is the base for the splitting of the annotations.
#
# See `AnnotationFunction` constructor for details.
#
AnnotationFunction = setClass("AnnotationFunction",
	slots = list(
		which = "character",
		fun_name = "character",
		width = "ANY",
		height = "ANY",
		n = "ANY",
		var_env = "environment",
		fun = "function",
		subset_rule = "list",
		subsetable = "logical",
		data_scale = "numeric",
		extended = "ANY",
		show_name = "logical"
	),
	prototype = list(
		fun_name = "",
		width = unit(1, "npc"),
		height = unit(1, "npc"),
		subset_rule = list(),
		subsetable = FALSE,
		data_scale = c(0, 1),
		n = NA_integer_,
		extended = unit(c(0, 0, 0, 0), "mm"),
		show_name = TRUE
	)
)


anno_width_and_height = function(which, width = NULL, height = NULL, 
	default = unit(10, "mm")) {

	if(which == "column") {
		if(is.null(height)) {
			height = default
		} else {
			if(!is_abs_unit(height)) {
				stop_wrap("height of the annotation can only be an absolute unit.")
			} else {
				height = convertHeight(height, "mm")
			}
		}
		if(is.null(width)) {
			width = unit(1, "npc")
		}
	}
	if(which == "row") {
		if(is.null(width)) {
			width = default
		} else {
			if(!is_abs_unit(width)) {
				stop_wrap("width of the annotation can only be an absolute unit.")
			} else {
				width = convertWidth(width, "mm")
			}
		}
		if(is.null(height)) {
			height = unit(1, "npc")
		}
	}
	return(list(height = height, width = width))
}


# == title
# Constructor of AnnotationFunction Class
#
# == param
# -fun A function which defines how to draw the annotation. See **Details** section.
# -fun_name The name of the function. It is only used for printing the object.
# -which Whether it is drawn as a column annotation or a row annotation?
# -var_import The names of the variables or the variable themselves that the annotation function depends on. See **Details** section.
# -n Number of observations in the annotation. It is not mandatory, but it is better to provide this information
#   so that the higher order `HeatmapAnnotation` knows it and it can perform check on the consistency of annotations and heatmaps.
# -data_scale The data scale on the data axis (y-axis for column annotation and x-axis for row annotation). It is only used
#             when `decorate_annotation` is used with "native" unit coordinates.
# -subset_rule The rule of subsetting variables in ``var_import``. It should be set when users want the final object to
#              be subsetable. See **Details** section.
# -subsetable Whether the object is subsetable?
# -show_name It is used to turn off the drawing of annotation names in `HeatmapAnnotation`. Annotations always have names
#        associated and normally they will be drawn beside the annotation graphics to tell what the annotation is about.
#        e.g. the annotation names put beside the points annotation graphics. However, for some of the annotations, the names
#        are not necessarily to be drawn, such as text annotations drawn by `anno_text` or an empty annotation drawn by `anno_empty`.
#        In this case, when ``show_names`` is set to ``FALSE``, there will be no annotation names drawn for the annotation.
# -width The width of the plotting region (the viewport) that the annotation is drawn. If it is a row annotation,
#        the width must be an absolute unit. Since the ``AnnotationFunction`` object is always contained by the `SingleAnnotation-class`object,
#        you can only set the width of row annotations or height of column annotations, while e.g. the height of the row annotation is always ``unit(1, "npc")``
#        which means it always fully filled in the parent ``SingleAnnotation`` and only in `SingleAnnotation` or even `HeatmapAnnotation`
#        can adjust the height of the row annotations.
# -height The height of the plotting region (the viewport) that the annotation is drawn. If it is a column annotation,
#        the width must be an absolute unit.
#
# == details
# We use a normal R function defines how to draw the annotation graphics. As
# expected, the main part of the AnnotationFunction class is this function.
# The function defines how to draw at specific positions which correspond to
# rows or columns in the heatmap. The function should have three arguments:
# ``index``, ``k`` and ``n`` (the names of the arguments can be arbitory)
# where ``k`` and ``n`` are optional. ``index`` corresponds to the indices of
# rows or columns of the heatmap. The value of ``index`` is not necessarily to
# be the whole row indices or column indices of the heatmap. It can be a
# subset of the indices if the annotation is split into slices according to
# the split of the heatmap. ``index`` is always reordered according to the
# reordering of heatmap rows or columns (e.g. by clustering). So, ``index``
# actually contains a list of row or column indices for the current slice
# after row or column reordering.
# 
# As mentioned, annotation can be split into slices. ``k`` corresponds to the
# current slice and ``n`` corresponds to the total number of slices. As you can image, 
# when ``n > 1``, the annotation function will be executed for all ``k``s. The
# information of ``k`` and ``n`` sometimes can be useful, for example, we want
# to add axis ot the right side of a column annotation, if this column annotation
# is split into several slices, the axis is only drawn when``k == n``.
#
# Since the function only allows ``index``, ``k`` and ``n``, the function
# sometimes uses several external variables which can not be defined inside
# the function, e.g. the data points for the annotation. These variables
# should be imported into the AnnotationFunction class so that the function
# can correctly find these variables (by ``var_import`` argument).
#
# One important feature for AnnotationFunction class is it can be subsetable.
# To allow subsetting of the object, users need to define the rules for the
# imported variables. The rules are simple function which
# accpets the variable and indices, and returns the subset of the variable.
# The subset rule functions implemented in this package are `subset_gp`,
# `subset_matrix_by_row` and `subset_vector`. These three functions are enough
# for most of the cases.
#
# In following, we defined three AnnotationFunction objects: 
#
# 1. It needs external variable and support subsetting
#
#	x = 1:10
#	anno1 = AnnotationFunction(
#		fun = function(index) {
#			n = length(index)
#			pushViewport(viewport())
#			grid.points(1:n, x[index])
#			popViewport()
#		},
#		var_imported = list(x = x),
#		n = 10,
#		subset_rule = list(x = subset_vector),
#		subsetable = TRUE
#	)
#
# 2. The data variable is defined inside the function and no need to import other variables.
#
#	anno2 = AnnotationFunction(
#		fun = function(index) {
#			x = 1:10
#			n = length(index)
#			pushViewport(viewport())
#			grid.points(1:n, x[index])
#			popViewport()
#		},
#		n = 10,
#		subsetable = TRUE
#	)
#
# 3. Only specify the function to the constructor. ``anno3`` is not subsettable.
#
#	anno3 = AnnotationFunction(
#		fun = function(index) {
#			x = 1:10
#			n = length(index)
#			pushViewport(viewport())
#			grid.points(1:n, x[index])
#			popViewport()
#		}
#	)
#
# As you can see from the examples, you need to push a viewport for graphics and finally pop the viewport.
#
# In the package, we have implemted quite a lot annotation function by `AnnotationFunction` constructor:
# `anno_empty`, `anno_image`, `anno_points`, `anno_lines`, `anno_barplot`, `anno_boxplot`, `anno_histogram`,
# `anno_density`, `anno_joyplot`, `anno_horizon`, `anno_text` and `anno_mark`. These built-in annotation functions
# support as both row annotations and column annotations and they are are all subsettable.
#
# == seealso
# The build-in annotation functions are already enough for most of the analysis, nevertheless, if users
# want to know more about how to construct the AnnotationFunction class manually, they can refer to
# ComplexHeatmap Complete Reference ().
AnnotationFunction = function(fun, fun_name = "", which = c("column", "row"), 
	var_import = list(), n = NA, data_scale = c(0, 1), subset_rule = list(), 
	subsetable = length(subset_rule) > 0, show_name = TRUE, width = NULL, height = NULL) {

	which = match.arg(which)[1]

	verbose = ht_opt$verbose
	
	anno = new("AnnotationFunction")

	anno@which = which
	anno@fun_name = fun_name

	if(verbose) qqcat("construct AnnotationFunction with '@{fun_name}()'\n")

	anno_size = anno_width_and_height(which, width, height, unit(1, "cm"))
	anno@width = anno_size$width
	anno@height = anno_size$height

	anno@show_name = show_name

	anno@n = n
	anno@data_scale = data_scale

	if(length(var_import)) {
		anno@var_env = new.env()
		if(is.character(var_import)) {
			for(nm in var_import) {
				anno@var_env[[nm]] = get(nm, envir = parent.frame())
			}
		} else if(inherits(var_import, "list")) {
			if(is.null(names(var_import))) {
				var_imported_nm = sapply(as.list(substitute(var_import))[-1], as.character)
				names(var_import) = var_imported_nm
			}

			for(nm in names(var_import)) {
				anno@var_env[[nm]] = var_import[[nm]]
			}
		} else {
			stop_wrap("`var_import` needs to be a character vector which contains variable names or a list of variables.")
		}
		environment(fun) = anno@var_env
	} else {
		anno@var_env = environment(fun)
	}

	if(length(as.list(formals(fun))) == 1) {
		formals(fun) = alist(index = , k = 1, n = 1)
	}
	anno@fun = fun
	
	if(is.null(subset_rule)) {
		for(nm in names(anno@var_env)) {
			if(is.matrix(anno@var_env[[nm]])) {
				anno@subset_rule[[nm]] = subset_matrix_by_row
			} else if(inherits(anno@var_env[[nm]], "gpar")) {
				anno@subset_rule[[nm]] = subset_gp
			} else if(is.vector(anno@var_env[[nm]])) {
				if(length(anno@var_env[[nm]]) > 1) {
					anno@subset_rule[[nm]] = subset_vector
				}
			}
		}
	} else {
		for(nm in names(subset_rule)) {
			anno@subset_rule[[nm]] = subset_rule[[nm]]
		}
	}

	if(missing(subsetable)) {
		# is user defined subset rule
		if(length(anno@subset_rule)) {
			anno@subsetable = TRUE
		}
	} else {
		anno@subsetable = subsetable
	}

	return(anno)
}

# == title
# Subset an AnnotationFunction Object
#
# == param
# -x An `AnnotationFunction-class` object.
# -i A vector of indices.
#
# == details
# One good thing for designing the `AnnotationFunction-class` is it can be subsetted,
# and this is the base for the splitting of the annotations.
#
# == example
# anno = anno_simple(1:10)
# anno[1:5]
# draw(anno[1:5], test = "subset of column annotation")
"[.AnnotationFunction" = function(x, i) {
	if(nargs() == 1) {
		return(x)
	} else {
		if(!x@subsetable) {
			stop_wrap("This object is not subsetable.")
		}
		x = copy_all(x)
		for(var in names(x@subset_rule)) {
			oe = try(x@var_env[[var]] <- x@subset_rule[[var]](x@var_env[[var]], i), silent = TRUE)
			if(inherits(oe, "try-error")) {
				message(paste0("An error when subsetting ", var))
				stop_wrap(oe)
			}
		}
		if(is.logical(i)) {
			x@n = sum(i)
		} else {
			x@n = length(i)
		}
		return(x)
	}
}

# == title
# Draw the AnnotationFunction Object
#
# == param
# -object The `AnnotationFunction-class` object.
# -index Index of observations.
# -k Current index of slice.
# -n Total number of slices.
# -test Is it in test mode? The value can be logical or a text which is plotted as the title of plot.
#
# == detail
# Normally it is called internally by the `SingleAnnotation` class.
#
# When ``test`` is set to ``TRUE``, the annotation graphic is directly drawn,
# which is generally for testing purpose.
# 
setMethod(f = "draw",
	signature = "AnnotationFunction",
	definition = function(object, index, k = 1, n = 1, test = FALSE) {
		
	if(is.character(test)) {
		test2 = TRUE
	} else {
		test2 = test
		test = ""
	}
	if(test2) {
        grid.newpage()
        pushViewport(viewport(width = 0.8, height = 0.8))
        if(is.na(object@n)) {
        	object@n = 1
        }
    }

    verbose = ht_opt$verbose
    if(verbose) qqcat("draw annotation generated by @{object@fun_name}\n")

    if(missing(index)) index = seq_len(object@n)

    anno_height = object@height
    anno_width = object@width

    # names should be passed to the data viewport
	pushViewport(viewport(width = anno_width, height = anno_height))
	vp_name1 = current.viewport()$name
	object@fun(index, k, n)
	if(test2) {
		grid.text(test, y = unit(1, "npc") + unit(2, "mm"), just = "bottom")

		if(!identical(unit(0, "mm"), object@extended[1])) {
			grid.rect(y = 1, height = unit(1, "npc") + object@extended[1], just = "top",
				gp = gpar(fill = "transparent", col = "red", lty = 2))
		} else if(!identical(unit(0, "mm"), object@extended[[2]])) {
			grid.rect(x = 1, width = unit(1, "npc") + object@extended[2], just = "right",
				gp = gpar(fill = "transparent", col = "red", lty = 2))
		} else if(!identical(unit(0, "mm"), object@extended[[3]])) {
			grid.rect(y = 0, height = unit(1, "npc") + object@extended[3], just = "bottom",
				gp = gpar(fill = "transparent", col = "red", lty = 2))
		} else if(!identical(unit(0, "mm"), object@extended[[4]])) {
			grid.rect(x = 0, width = unit(1, "npc") + object@extended[4], just = "left",
				gp = gpar(fill = "transparent", col = "red", lty = 2))
		}
		
	}
	vp_name2 = current.viewport()$name
	if(vp_name1 != vp_name2) {
		stop_wrap("Viewports should be the same before and after plotting the annotation graphics.")
	}
	popViewport()

	if(test2) {
		popViewport()
	}
	
})

# == title
# Copy the AnnotationFunction Object
#
# == param
# -object The `AnnotationFunction-class` object.
#
# == detail 
# In `AnnotationFunction-class`, there is an environment which
# stores some external variables for the annotation function (specified by the
# ``var_import`` argument by constructing the `AnnotationFunction-class`
# object. This `copy_all,AnnotationFunction-method` hard copies all the
# variables into a new isolated environment.
#
# The environment is at ``object@var_env``.
#
setMethod(f = "copy_all",
	signature = "AnnotationFunction",
	definition = function(object) {
		object2 = object
		object2@var_env = new.env()
		for(var in names(object@var_env)) {
			object2@var_env[[var]] = object@var_env[[var]]
		}
		environment(object2@fun) = object2@var_env
		return(object2)
})

# == title
# Print the AnnotationFunction Object
#
# == param
# -object The `AnnotationFunction-class` object.
#
setMethod(f = "show",
	signature = "AnnotationFunction",
	definition = function(object) {

	cat("An AnnotationFunction object\n")
	if(object@fun_name == "") {
		cat("  function: user-defined\n")
	} else {
		cat("  function: ", object@fun_name, "()\n", sep = "")
	}
	cat("  position:", object@which, "\n")
	cat("  items:", ifelse(object@n == 0, "unknown", object@n), "\n")
	cat("  width:", as.character(object@width), "\n")
	cat("  height:", as.character(object@height), "\n")
	var_imported = names(object@var_env)
	if(length(var_imported)) {
		cat("  imported variable:", paste(var_imported, collapse = ", "), "\n")
		var_subsetable = names(object@subset_rule)
		if(length(var_subsetable)) {
			cat("  subsetable variable:", paste(var_subsetable, collapse = ", "), "\n")
		}
	}
	cat("  this object is", ifelse(object@subsetable, "\b", "not"), "subsetable\n")
	dirt = c("bottom", "left", "top", "right")
	for(i in 1:4) {
		if(!identical(unit(0, "mm"), object@extended[i])) {
			cat(" ", as.character(object@extended[i]), "extension on the", dirt[i], "\n")
		}
	}
	
})

# == title
# Number of Observations
#
# == param
# -object The `AnnotationFunction-class` object.
# -... Other arguments.
#
# == details It returns the ``n`` slot in the object. If it does not exist, it
# returns ``NA``.
#
# == example
# anno = anno_points(1:10)
# nobs(anno)
nobs.AnnotationFunction = function(object, ...) {
	if(is.na(object@n)) {
		return(NA)
	} else if(object@n > 0) {
		object@n
	} else {
		NA
	}
}
