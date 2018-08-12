AnnotationFunction = setClass("AnnotationFunction",
	slots = list(
		which = "character",
		fun_name = "character",
		width = "ANY",
		height = "ANY",
		n = "numeric",
		var_env = "environment",
		fun = "function",
		subset_rule = "list",
		subsetable = "logical",
		data_scale = "numeric",
		extended = "ANY"
	),
	prototype = list(
		fun_name = "",
		width = unit(1, "npc"),
		height = unit(1, "npc"),
		subset_rule = list(),
		subsetable = FALSE,
		data_scale = c(0, 1),
		n = 0,
		extended = unit(c(0, 0, 0, 0), "mm")
	)
)

anno_width_and_height = function(which, width = NULL, height = NULL, 
	default = unit(1, "cm")) {

	if(which == "column") {
		if(is.null(height)) {
			height = default
		} else {
			if(!is_abs_unit(height)) {
				stop("height can only be an absolute unit.")
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
				stop("width can only be an absolute unit.")
			}
		}
		if(is.null(height)) {
			height = unit(1, "npc")
		}
	}
	return(list(height = height, width = width))
}


AnnotationFunction = function(fun, fun_name = "", which = c("column", "row"), 
	var_imported = list(), n = 0, data_scale = c(0, 1), subset_rule = list(), 
	subsetable = FALSE, width = NULL, height = NULL) {

	which = match.arg(which)[1]

	anno = new("AnnotationFunction")

	anno@which = which
	anno@fun_name = fun_name

	anno_size = anno_width_and_height(which, width, height, unit(1, "cm"))
	anno@width = anno_size$width
	anno@height = anno_size$height

	anno@n = n
	anno@data_scale = data_scale

	anno@var_env = new.env()
	if(is.character(var_imported)) {
		for(nm in var_imported) {
			anno@var_env[[nm]] = get(nm, envir = parent.frame())
		}
	} else if(inherits(var_imported, "list")) {
		if(is.null(names(var_imported))) {
			var_imported_nm = sapply(as.list(substitute(var_imported))[-1], as.character)
			names(var_imported) = var_imported_nm
		}

		for(nm in names(var_imported)) {
			anno@var_env[[nm]] = var_imported[[nm]]
		}
	} else {
		stop_wrap("`var_import` needs to be a character vector which contains variable names or a list of variables")
	}
	
	environment(fun) = anno@var_env
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


"[.AnnotationFunction" = function(x, i) {
	if(nargs() == 1) {
		return(x)
	} else {
		if(!x@subsetable) {
			stop("This object is not subsetable.")
		}
		x = copy_all(x)
		for(var in names(x@subset_rule)) {
			oe = try(x@var_env[[var]] <- x@subset_rule[[var]](x@var_env[[var]], i), silent = TRUE)
			if(inherits(oe, "try-error")) {
				message(paste0("An error when subsetting ", var))
				stop(oe)
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

setMethod(f = "draw",
	signature = "AnnotationFunction",
	definition = function(object, index, test = FALSE) {
		
	if(is.character(test)) {
		test2 = TRUE
	} else {
		test2 = test
	}
	if(test2) {
        grid.newpage()
        pushViewport(viewport(width = 0.8, height = 0.8))
    }

    if(missing(index)) index = seq_len(object@n)

    anno_height = object@height
    anno_width = object@width
    
    # names should be passed to the data viewport
	pushViewport(viewport(width = anno_width, height = anno_height))
	object@fun(index)
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
	popViewport()

	if(test2) {
		popViewport()
	}
	
})

setMethod(f = "copy_all",
	signature = "AnnotationFunction",
	definition = function(object, i) {
		object2 = object
		object2@var_env = new.env()
		for(var in names(object@var_env)) {
			object2@var_env[[var]] = object@var_env[[var]]
		}
		environment(object2@fun) = object2@var_env
		return(object2)
})

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

anno_empty = function(which = c("column", "row"), border = TRUE, width = NULL, height = NULL) {
	
	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	anno_size = anno_width_and_height(which, width, height, unit(1, "cm"))
	
	fun = function(index) {
		if(border) grid.rect()
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_empty",
		which = which,
		var_import = list(border),
		subset_rule = list(),
		subsetable = TRUE,
		height = anno_size$height,
		width = anno_size$width
	)
	return(anno) 
}

subset_matrix_by_row = function(x, i) x[i, , drop = FALSE]
subset_vector = function(x, i) x[i]

anno_simple = function(x, col, na_col = "grey", 
	which = c("column", "row"), border = FALSE, gp = gpar(col = NA),
	pch = NULL, pt_size = unit(1, "snpc")*0.8, pt_gp = gpar(), 
	width = NULL, height = NULL) {

	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	if(is.data.frame(x)) x = as.matrix(x)
	if(is.matrix(x)) {
		if(ncol(x) == 1) {
			x = x[, 1]
		}
	}
	input_is_matrix = is.matrix(x)

	anno_size = anno_width_and_height(which, width, height, 
		unit(5, "mm")*ifelse(input_is_matrix, ncol(x), 1))
	
	if(missing(col)) {
		col = default_col(x)
	}
	if(is.atomic(col)) {
		color_mapping = ColorMapping(name = "foo", colors = col, na_col = na_col)
    } else if(is.function(col)) {
        color_mapping = ColorMapping(name = "foo", col_fun = col, na_col = na_col)
    } else if(inherits(col, "ColorMapping")) {
    	color_mapping = col
    } else {
    	stop_wrap("`col` should be a named vector/a color mapping function/a ColorMapping object.")
    }

    value = x
    gp = subset_gp(gp, 1)  # gp controls border

    if(is.matrix(value)) {
		n = nrow(value)
		nr = n
		nc = ncol(value)
	} else {
		n = length(value)
		nr = n
		nc = 1
	}
	
    if(!is.null(pch)) {
    	if(input_is_matrix) {
		    pch = normalize_graphic_param_to_mat(pch, ifelse(is.matrix(x), ncol(x), 1), n, "pch")
		    pt_size = pt_size[1]*(1/nc)
		    pt_gp = subset_gp(pt_gp, 1)
		} else {
			if(length(pch) == 1) pch = rep(pch, n)
			if(length(pt_size) == 1) pt_size = rep(pt_size, n)
			pt_gp = recycle_gp(pt_gp, n)
		}
	}

	row_fun = function(index) {
		
		n = length(index)
		y = (n - seq_len(n) + 0.5) / n
        if(is.matrix(value)) {
            nc = ncol(value)
            for(i in seq_len(nc)) {
                fill = map_to_colors(color_mapping, value[index, i])
                grid.rect(x = (i-0.5)/nc, y, height = 1/n, width = 1/nc, 
                	gp = do.call("gpar", c(list(fill = fill), gp)))
                if(!is.null(pch)) {
					l = !is.na(pch[, i])
					grid.points(x = rep((i-0.5)/nc, sum(l)), y = y[l], pch = pch[l, i], 
						size = pt_size, gp = pt_gp)
				}
            }
        } else {
			fill = map_to_colors(color_mapping, value[index])
			grid.rect(x = 0.5, y, height = 1/n, width = 1, gp = do.call("gpar", c(list(fill = fill), gp)))
			if(!is.null(pch)) {
				l = !is.na(pch)
				grid.points(x = rep(0.5, sum(l)), y = y[l], pch = pch[l], size = pt_size[l], 
					gp = subset_gp(pt_gp, which(l)))
			}
        }
        if(border) grid.rect(gp = gpar(fill = "transparent"))
	}

	column_fun = function(index) {
		n = length(index)
		x = (seq_len(n) - 0.5) / n
        if(is.matrix(value)) {
            nc = ncol(value)
            for(i in seq_len(nc)) {
                fill = map_to_colors(color_mapping, value[index, i])
                grid.rect(x, y = (nc-i +0.5)/nc, width = 1/n, height = 1/nc, gp = do.call("gpar", c(list(fill = fill), gp)))
                if(!is.null(pch)) {
					l = !is.na(pch[, i])
					grid.points(x[l], y = rep((nc-i +0.5)/nc, sum(l)), pch = pch[l, i], size = pt_size, gp = pt_gp)
				}
            }
        } else {
			fill = map_to_colors(color_mapping, value[index])
			grid.rect(x, y = 0.5, width = 1/n, height = 1, gp = do.call("gpar", c(list(fill = fill), gp)))
			if(!is.null(pch)) {
				l = !is.na(pch)
				grid.points(x[l], y = rep(0.5, sum(l)), pch = pch[l], size = pt_size[l], gp = subset_gp(pt_gp, which(l)))
			}
        }
        if(border) grid.rect(gp = gpar(fill = "transparent"))
	}

	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_simple",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		n = n,
		data_scale = c(0.5, nc + 0.5),
		var_import = list(value, gp, border, color_mapping, pt_gp, pt_size, pch)
	)

	anno@subset_rule = list()
	if(input_is_matrix) {
		anno@subset_rule$value = subset_matrix_by_row
		if(!is.null(pch)) {
			anno@subset_rule$pch = subset_matrix_by_row
		}
	} else {
		anno@subset_rule$value = subset_vector
		if(!is.null(pch)) {
			anno@subset_rule$pch = subset_vector
			anno@subset_rule$pt_size = subset_vector
			anno@subset_rule$pt_gp = subset_gp
		}
	}

	anno@subsetable = TRUE

	return(anno)      
}

anno_image = function(image, which = c("column", "row"), border = TRUE, 
	gp = gpar(fill = NA, col = NA), space = unit(1, "mm"), width = NULL, height = NULL) {

	allowed_image_type = c("png", "svg", "pdf", "eps", "jpeg", "jpg", "tiff")

	if(inherits(image, "character")) { ## they are file path
		image_type = tolower(gsub("^.*\\.(\\w+)$", "\\1", image))
		if(! all(image_type %in% allowed_image_type)) {
			stop("image file should be of png/svg/pdf/eps/jpeg/jpg/tiff.")
		}
	} else {
		stop("`image` should be a vector of path.")
	}

	n_image = length(image)
	image_list = vector("list", n_image)
	image_class = vector("character", n_image)
	for(i in seq_along(image)) {
		if(image_type[i] == "png") {
			if(!requireNamespace("png")) {
				stop("Need png package to read png images.")
			}
			image_list[[i]] = png::readPNG(image[i])
			image_class[i] = "raster"
		} else if(image_type[i] %in% c("jpeg", "jpg")) {
			if(!requireNamespace("jpeg")) {
				stop("Need jpeg package to read jpeg/jpg images.")
			}
			image_list[[i]] = jpeg::readJPEG(image[i])
			image_class[i] = "raster"
		} else if(image_type[i] == "tiff") {
			if(!requireNamespace("tiff")) {
				stop("Need tiff package to read tiff images.")
			}
			image_list[[i]] = tiff::readTIFF(image[i])
			image_class[i] = "raster"
		} else if(image_type[i] %in% c("pdf", "eps")) {
			if(!requireNamespace("grImport")) {
				stop("Need grImport package to read pdf/eps images.")
			}
			temp_file = tempfile()
			getFromNamespace("PostScriptTrace", ns = "grImport")(image[[i]], temp_file)
			image_list[[i]] = grImport::readPicture(temp_file)
			file.remove(temp_file)
			image_class[i] = "grImport::Picture"
		} else if(image_type[i] == "svg") {
			if(!requireNamespace("grImport2")) {
				stop("Need grImport2 package to read svg images.")
			}
			if(!requireNamespace("rsvg")) {
				stop("Need rsvg package to convert svg images.")
			}
			temp_file = tempfile()
			rsvg::rsvg_svg(image[i], temp_file)
			image_list[[i]] = grImport2::readPicture(temp_file)
			file.remove(temp_file)
			image_class[i] = "grImport2::Picture"
		}
	}
	yx_asp = sapply(image_list, function(x) {
		if(inherits(x, "array")) {
			nrow(x)/ncol(x)
		} else if(inherits(x, "Picture")) {
			max(x@summary@yscale)/max(x@summary@xscale)
		}
	})

	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	space = space[1]

	anno_size = anno_width_and_height(which, width, height, unit(1, "cm"))

	gp = recycle_gp(gp, n_image)
	
	column_fun = function(index) {
		n = length(index)

		pushViewport(viewport())
		asp = convertHeight(unit(1, "npc") - space*2, "mm", valueOnly = TRUE)/convertWidth(unit(1/n, "npc") - space*2, "mm", value = TRUE)
		grid.rect(x = (1:n - 0.5)/n, width = 1/n, gp = subset_gp(gp, index))
		for(i in seq_len(n)) {
			if(yx_asp[ index[i] ] > asp) {
				height = unit(1, "npc") - space*2
				width = convertHeight(height, "mm")*yx_asp[ index[i] ]
			} else {
				width = unit(1/n, "npc") - space*2
				height = yx_asp[ index[i] ]*convertWidth(width, "mm")
			}
			if(image_class[ index[i] ] == "raster") {
				grid.raster(image_list[[i]], x = (i-0.5)/n, width = width, height = height)
			} else if(image_class[ index[i] ] == "grImport::Picture") {
				grid.picture = getFromNamespace("grid.picture", ns = "grImport")
				grid.picture(image_list[[i]], x = (i-0.5)/n, width = width, height = height)
			} else if(image_class[ index[i] ] == "grImport2::Picture") {
				grid.picture = getFromNamespace("grid.picture", ns = "grImport2")
				grid.picture(image_list[[i]], x = (i-0.5)/n, width = width, height = height)
			}
		}
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}

	row_fun = function(index) {
		n = length(index)

		pushViewport(viewport())
		asp = convertHeight(unit(1/n, "npc") - space*2, "mm", valueOnly = TRUE)/convertWidth(unit(1, "npc") - space*2, "mm", value = TRUE)
		grid.rect(y = (n - 1:n + 0.5)/n, height = 1/n, gp = subset_gp(gp, index))
		for(i in seq_len(n)) {
			if(yx_asp[ index[i] ] > asp) {
				height = unit(1/n, "npc") - space*2
				width = convertHeight(height, "mm")*(1/yx_asp[ index[i] ])
			} else {
				width = unit(1, "npc") - space*2
				height = yx_asp[ index[i] ]*convertWidth(width, "mm")
			}
			if(image_class[ index[i] ] == "raster") {
				grid.raster(image_list[[i]], y = (n - i + 0.5)/n, width = width, height = height)
			} else if(image_class[ index[i] ] == "grImport::Picture") {
				grid.picture = getFromNamespace("grid.picture", ns = "grImport")
				grid.picture(image_list[[i]], y = (n - i + 0.5)/n, width = width, height = height)
			} else if(image_class[ index[i] ] == "grImport2::Picture") {
				grid.picture = getFromNamespace("grid.picture", ns = "grImport2")
				grid.picture(image_list[[i]], y = (n - i + 0.5)/n, width = width, height = height)
			}
		}
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}
	
	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_image",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		n = n_image,
		data_scale = c(0.5, 1.5),
		var_import = list(gp, border, space, yx_asp, image_list, image_class)
	)

	anno@subset_rule$gp = subset_vector
	anno@subset_rule$image_list = subset_vector
	anno@subset_rule$image_class = subset_vector

	anno@subsetable = TRUE

	return(anno)   
}

default_axis_param = function(which) {
	list(
		at = NULL, 
		labels = NULL, 
		labels_rot = ifelse(which == "column", 0, 90), 
		gp = gpar(fontsize = 8), 
		side = ifelse(which == "column", "left", "bottom"), 
		facing = "outside"
	)
}

validate_axis_param = function(axis_param, which) {
	dft = default_axis_param(which)
	for(nm in names(axis_param)) {
		dft[[nm]] = axis_param[[nm]]
	}
	return(dft)
}

construct_axis_grob = function(axis_param, which, data_scale) {
	axis_param_default = default_axis_param(which)

	for(nm in setdiff(names(axis_param_default), names(axis_param))) {
		axis_param[[nm]] = axis_param_default[[nm]]
	}
	
	if(is.null(axis_param$at)) {
		at = pretty_breaks(data_scale)
		axis_param$at = at
		axis_param$labels = at
	}
	if(is.null(axis_param$labels)) {
		axis_param$labels = axis_param$at
	}
	axis_grob = do.call(annotation_axis_grob, axis_param)
	
	return(axis_grob)
}

# == title
# Using points as annotation
#
# == param
# -x a vector of numeric values.
# -which is the annotation a column annotation or a row annotation?
# -border whether show border of the annotation compoment
# -gp graphic parameters.
# -pch point type.
# -size point size.
# -ylim data ranges.
# -axis whether add axis.
# -axis_side if it is placed as column annotation, value can only be "left" or "right".
#            If it is placed as row annotation, value can only be "bottom" or "top".
# -axis_gp graphic parameters for axis
# -axis_direction if the annotation is row annotation, should the axis be from left to right (default) or follow the reversed direction?
# -... for future use.
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_points = function(x, which = c("column", "row"), border = TRUE, gp = gpar(), pch = 16, 
	size = unit(2, "mm"), ylim = NULL, extend = 0.05, axis = TRUE,
	axis_param = default_axis_param(which),
	width = NULL, height = NULL) {

	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	if(is.data.frame(x)) x = as.matrix(x)
	if(is.matrix(x)) {
		if(ncol(x) == 1) {
			x = x[, 1]
		}
	}
	input_is_matrix = is.matrix(x)

	anno_size = anno_width_and_height(which, width, height, unit(1, "cm"))

	if(is.matrix(x)) {
		n = nrow(x)
		nr = n
		nc = ncol(x)
	} else {
		n = length(x)
		nr = n
		nc = 1
	}

	if(is.atomic(x)) {
		gp = recycle_gp(gp, n)
		if(length(pch) == 1) pch = rep(pch, n)
		if(length(size) == 1) size = rep(size, n)
	} else if(input_is_matrix) {
		gp = recycle_gp(gp, nc)
		if(length(pch) == 1) pch = rep(pch, nc)
		if(length(size) == 1) size = rep(size, nc)
	}
	
	if(is.null(ylim)) {
		data_scale = range(x, na.rm = TRUE)
	} else {
		data_scale = ylim
	}
	data_scale = data_scale + c(-extend, extend)*(data_scale[2] - data_scale[1])

	value = x

	axis_param = validate_axis_param(axis_param, which)
	axis_grob = if(axis) construct_axis_grob(axis_param, which, data_scale) else NULL

	row_fun = function(index) {
		n = length(index)

		pushViewport(viewport(xscale = data_scale, yscale = c(0.5, n+0.5)))
		if(is.matrix(value)) {
			for(i in seq_len(ncol(value))) {
				grid.points(value[index, i], n - seq_along(index) + 1, gp = subset_gp(gp, i), 
					default.units = "native", pch = pch[i], size = size[i])
			}
		} else {
			grid.points(value[index], n - seq_along(index) + 1, gp = gp, default.units = "native", 
				pch = pch[index], size = size[index])
		}
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}

	column_fun = function(index) {
		n = length(index)
		
		pushViewport(viewport(yscale = data_scale, xscale = c(0.5, n+0.5)))
		if(is.matrix(value)) {
			for(i in seq_len(ncol(value))) {
				grid.points(seq_along(index), value[index, i], gp = subset_gp(gp, i), default.units = "native", pch = pch[i], size = size[i])
			}
		} else {
			grid.points(seq_along(index), value[index], gp = gp, default.units = "native", pch = pch[index], size = size[index])
		}
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}

	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_points",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		n = n,
		data_scale = data_scale,
		var_import = list(value, gp, border, pch, size, axis, axis_param, axis_grob, data_scale)
	)

	anno@subset_rule$gp = subset_vector
	if(input_is_matrix) {
		anno@subset_rule$value = subset_matrix_by_row
	} else {
		anno@subset_rule$value = subset_vector
		anno@subset_rule$gp = subset_gp
		anno@subset_rule$size = subset_vector
		anno@subset_rule$pch = subset_vector
	}

	anno@subsetable = TRUE

	anno@extended = update_anno_extend(anno, axis_grob, axis_param)
		
	return(anno) 
}

update_anno_extend = function(anno, axis_grob, axis_param) {
	extended = anno@extended
	if(axis_param$facing == "outside") {
		if(axis_param$side == "left") {
			extended[[2]] = convertWidth(grobWidth(axis_grob), "mm", valueOnly = TRUE)
		} else if(axis_param$side == "right") {
			extended[[4]] = convertWidth(grobWidth(axis_grob), "mm", valueOnly = TRUE)
		} else if(axis_param$side == "top") {
			extended[[3]] = convertHeight(grobHeight(axis_grob), "mm", valueOnly = TRUE)
		} else if(axis_param$side == "bottom") {
			extended[[1]] = convertHeight(grobHeight(axis_grob), "mm", valueOnly = TRUE)
		}
	}
	return(extended)
}

anno_lines = function(x, which = c("column", "row"), border = TRUE, gp = gpar(), 
	add_points = TRUE, pch = 16, size = unit(2, "mm"), pt_gp = gpar(), ylim = NULL, 
	extend = 0.05, axis = TRUE, axis_param = default_axis_param(which),
	width = NULL, height = NULL) {

	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	if(is.data.frame(x)) x = as.matrix(x)
	if(is.matrix(x)) {
		if(ncol(x) == 1) {
			x = x[, 1]
		}
	}
	input_is_matrix = is.matrix(x)

	anno_size = anno_width_and_height(which, width, height, unit(1, "cm"))

	if(is.matrix(x)) {
		n = nrow(x)
		nr = n
		nc = ncol(x)
	} else {
		n = length(x)
		nr = n
		nc = 1
	}

	if(is.atomic(x)) {
		gp = recycle_gp(gp, 1)
		pt_gp = recycle_gp(pt_gp, n)
		if(length(pch) == 1) pch = rep(pch, n)
		if(length(size) == 1) size = rep(size, n)
	} else if(input_is_matrix) {
		gp = recycle_gp(gp, nc)
		pt_gp = recycle_gp(pt_gp, nc)
		if(length(pch) == 1) pch = rep(pch, nc)
		if(length(size) == 1) size = rep(size, nc)
	}
	
	if(is.null(ylim)) {
		data_scale = range(x, na.rm = TRUE)
	} else {
		data_scale = ylim
	}
	data_scale = data_scale + c(-extend, extend)*(data_scale[2] - data_scale[1])

	value = x

	axis_param = validate_axis_param(axis_param, which)
	axis_grob = if(axis) construct_axis_grob(axis_param, which, data_scale) else NULL

	row_fun = function(index) {
		n = length(index)

		pushViewport(viewport(xscale = data_scale, yscale = c(0.5, n+0.5)))
		if(is.matrix(value)) {
			for(i in seq_len(ncol(value))) {
				grid.lines(value[index, i], n - seq_along(index) + 1, gp = subset_gp(gp, i), 
					default.units = "native")
				if(add_points) {
					grid.points(value[index, i], n - seq_along(index) + 1, gp = subset_gp(pt_gp, i), 
						default.units = "native", pch = pch[i], size = size[i])
				}
			}
		} else {
			grid.lines(value[index, i], n - seq_along(index) + 1, gp = gp, 
				default.units = "native")
			if(add_points) {
				grid.points(value[index], n - seq_along(index) + 1, gp = gp, default.units = "native", 
					pch = pch[index], size = size[index])
			}
		}
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}

	column_fun = function(index) {
		n = length(index)
		
		pushViewport(viewport(yscale = data_scale, xscale = c(0.5, n+0.5)))
		if(is.matrix(value)) {
			for(i in seq_len(ncol(value))) {
				grid.lines(seq_along(index), value[index, i], gp = subset_gp(gp, i), 
					default.units = "native")
				if(add_points) {
					grid.points(seq_along(index), value[index, i], gp = subset_gp(pt_gp, i), 
						default.units = "native", pch = pch[i], size = size[i])
				}
			}
		} else {
			grid.lines(seq_along(index), value[index], gp = gp, default.units = "native")
			if(add_points) {
				grid.points(seq_along(index), value[index], gp = pt_gp, default.units = "native", 
					pch = pch[index], size = size[index])
			}
		}
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}

	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_points",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		n = n,
		data_scale = data_scale,
		var_import = list(value, gp, border, pch, size, pt_gp, axis, axis_param, axis_grob, data_scale, add_points)
	)

	anno@subset_rule$gp = subset_vector
	if(input_is_matrix) {
		anno@subset_rule$value = subset_matrix_by_row
	} else {
		anno@subset_rule$value = subset_vector
		anno@subset_rule$gp = subset_gp
		anno@subset_rule$pt_gp = subset_gp
		anno@subset_rule$size = subset_vector
		anno@subset_rule$pch = subset_vector
	}

	anno@subsetable = TRUE

	anno@extended = update_anno_extend(anno, axis_grob, axis_param)
		
	return(anno) 
}

# == title
# Using barplot as annotation
#
# == param
# -x a vector of numeric values. If the value is a matrix, columns of the matrix will be represented as
#    stacked barplots. Note for stacked barplots, each row in the matrix should only contain values with same sign (either all positive or all negative).
# -baseline baseline for bars. The value should be "min" or "max", or a numeric value. It is enforced to be zero
#       for stacked barplots.
# -which is the annotation a column annotation or a row annotation?
# -border whether show border of the annotation compoment
# -bar_width relative width of the bars, should less than one
# -gp graphic parameters. If it is the stacked barplots, the length of the graphic parameter should 
#     be same as the number of stacks.
# -ylim data ranges.
# -axis whether add axis
# -axis_side if it is placed as column annotation, value can only be "left" or "right".
#            If it is placed as row annotation, value can only be "bottom" or "top".
# -axis_gp graphic parameters for axis
# -axis_direction if the annotation is row annotation, should the axis be from left to right (default) or follow the reversed direction?
# -... for future use.
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_barplot = function(x, baseline = 0, which = c("column", "row"), border = TRUE, bar_width = 0.6,
	gp = gpar(fill = "#CCCCCC"), ylim = NULL, extend = 0.05, axis = TRUE, 
	axis_param = default_axis_param(which),
	width = NULL, height = NULL) {

	if(inherits(x, "list")) x = do.call("cbind", x)
	if(inherits(x, "data.frame")) x = as.matrix(x)
	if(inherits(x, "matrix")) {
		sg = apply(x, 1, function(xx) all(sign(xx) %in% c(1, 0)) || all(sign(xx) %in% c(-1, 0)))
		if(!all(sg)) {
			stop_wrap("Since `x` is a matrix, the sign of each row should be either all positive or all negative.")
		}
	}
	# convert everything to matrix
	if(is.null(dim(x))) x = matrix(x, ncol = 1)
	nc = ncol(x)
	if(missing(gp)) {
		gp = gpar(fill = grey(seq(0, 1, length = nc+2))[-c(1, nc+2)])
	}

	data_scale = range(rowSums(x, na.rm = TRUE), na.rm = TRUE)
	if(!is.null(ylim)) data_scale = ylim
	if(baseline == "min") {
		data_scale = data_scale + c(0, extend)*(data_scale[2] - data_scale[1])
	} else if(baseline == "max") {
		data_scale = data_scale + c(-extend, 0)*(data_scale[2] - data_scale[1])
	} else {
		if(is.numeric(baseline)) {
			if(baseline == 0 && all(rowSums(x) == 1)) {
				data_scale = c(0, 1)
			} else if(baseline <= min(x)) {
				data_scale = c(baseline, extend*(data_scale[2] - baseline) + data_scale[2])
			} else if(baseline >= rowSums(x)) {
				data_scale = c(-extend*(baseline - data_scale[1]) + data_scale[1], baseline)
			} else {
				data_scale = data_scale + c(-extend, extend)*(data_scale[2] - data_scale[1])
			}
		}
	}

	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	anno_size = anno_width_and_height(which, width, height, unit(1, "cm"))

	if(nc == 1) {
		gp = recycle_gp(gp, nrow(x))
	} else  {
		gp = recycle_gp(gp, nc)
	}

	value = x
	axis_param = validate_axis_param(axis_param, which)
	axis_grob = if(axis) construct_axis_grob(axis_param, which, data_scale) else NULL

	row_fun = function(index) {
		n = length(index)
		
		pushViewport(viewport(xscale = data_scale, yscale = c(0.5, n+0.5)))
		if(ncol(value) == 1) {
			width = value[index] - baseline
			x_coor = width/2+baseline
			grid.rect(x = x_coor, y = n - seq_along(index) + 1, width = abs(width), height = 1*bar_width, default.units = "native", gp = subset_gp(gp, index))
		} else {
			for(i in seq_len(ncol(value))) {
				width = value[index, i]
				x_coor = rowSums(value[index, seq_len(i-1), drop = FALSE]) + width/2
				grid.rect(x = x_coor, y = n - seq_along(index) + 1, width = abs(width), height = 1*bar_width, default.units = "native", gp = subset_gp(gp, i))
			}
		}
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}
	column_fun = function(index) {
		n = length(index)
	
		pushViewport(viewport(yscale = data_scale, xscale = c(0.5, n+0.5)))
		if(ncol(value) == 1) {
			height = value[index] - baseline
			y_coor = height/2+baseline
			grid.rect(y = y_coor, x = seq_along(index), height = abs(height), width = 1*bar_width, default.units = "native", gp = subset_gp(gp, index))
		} else {
			for(i in seq_len(ncol(value))) {
				height = value[index, i]
				y_coor = rowSums(value[index, seq_len(i-1), drop = FALSE]) + height/2
				grid.rect(y = y_coor, x = seq_along(index), height = abs(height), width = 1*bar_width, default.units = "native", gp = subset_gp(gp, i))
			}
		}
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}
	
	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}
	n = nrow(value)

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_barplot",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		n = n,
		data_scale = data_scale,
		var_import = list(value, gp, border, bar_width, baseline, axis, axis_param, axis_grob, data_scale)
	)

	anno@subset_rule$value = subset_matrix_by_row
	if(ncol(value) == 1) {
		anno@subset_rule$gp = subset_gp
	}
		
	anno@subsetable = TRUE

	anno@extended = update_anno_extend(anno, axis_grob, axis_param)

	return(anno) 
}

# == title
# Using boxplot as annotation
#
# == param
# -x a matrix or a list. If ``x`` is a matrix and if ``which`` is ``column``, statistics for boxplot
#    is calculated by columns, if ``which`` is ``row``, the calculation is by rows.
# -which is the annotation a column annotation or a row annotation?
# -border whether show border of the annotation compoment
# -gp graphic parameters
# -ylim data ranges.
# -outline whether draw outliers
# -pch point type
# -size point size
# -axis whether add axis
# -axis_side if it is placed as column annotation, value can only be "left" or "right".
#            If it is placed as row annotation, value can only be "bottom" or "top".
# -axis_gp graphic parameters for axis
# -axis_direction if the annotation is row annotation, should the axis be from left to right (default) or follow the reversed direction?
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_boxplot = function(x, which = c("column", "row"), border = TRUE,
	gp = gpar(fill = "#CCCCCC"), ylim = NULL, extend = 0.05, outline = TRUE, box_width = 0.6,
	pch = 1, size = unit(2, "mm"), axis = TRUE, axis_param = default_axis_param(which),
	width = NULL, height = NULL) {

	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	anno_size = anno_width_and_height(which, width, height, unit(2, "cm"))

	## convert matrix all to list (or data frame)
	if(is.matrix(x)) {
		if(which == "column") {
			value = as.data.frame(x)
		} else if(which == "row") {
			value = as.data.frame(t(x))
		}
	} else {
		value = x
	}

	if(is.null(ylim)) {
		if(!outline) {
			boxplot_stats = boxplot(value, plot = FALSE)$stats
			data_scale = range(boxplot_stats)
		} else {
			data_scale = range(value, na.rm = TRUE)
		}
	} else {
		data_scale = ylim
	}
	data_scale = data_scale + c(-extend, extend)*(data_scale[2] - data_scale[1])

	n = length(value)
	gp = recycle_gp(gp, n)
	if(length(pch) == 1) pch = rep(pch, n)
	if(length(size) == 1) size = rep(size, n)

	axis_param = validate_axis_param(axis_param, which)
	axis_grob = if(axis) construct_axis_grob(axis_param, which, data_scale) else NULL

	row_fun = function(index) {

		n_all = length(value)
		value = value[index]
		boxplot_stats = boxplot(value, plot = FALSE)$stats
		
		n = length(index)
		gp = subset_gp(gp, index)
		pushViewport(viewport(xscale = data_scale, yscale = c(0.5, n+0.5)))
		
		grid.rect(x = boxplot_stats[2, ], y = n - seq_along(index) + 1,  
			height = 1*box_width, width = boxplot_stats[4, ] - boxplot_stats[2, ], just = "left", 
			default.units = "native", gp = gp)

		grid.segments(boxplot_stats[5, ], n - seq_along(index) + 1 - 0.5*box_width, 
			          boxplot_stats[5, ], n - seq_along(index) + 1 + 0.5*box_width, 
			          default.units = "native", gp = gp)
		grid.segments(boxplot_stats[5, ], n - seq_along(index) + 1,
			          boxplot_stats[4, ], n - seq_along(index) + 1, 
			          default.units = "native", gp = gp)
		grid.segments(boxplot_stats[1, ], n - seq_along(index) + 1, 
			          boxplot_stats[2, ], n - seq_along(index) + 1, 
			          default.units = "native", gp = gp)
		grid.segments(boxplot_stats[1, ], n - seq_along(index) + 1 - 0.5*box_width, 
			          boxplot_stats[1, ], n - seq_along(index) + 1 + 0.5*box_width, 
			          default.units = "native", gp = gp)
		grid.segments(boxplot_stats[3, ], n - seq_along(index) + 1 - 0.5*box_width, 
			          boxplot_stats[3, ], n - seq_along(index) + 1 + 0.5*box_width, 
			          default.units = "native", gp = gp)
		if(outline) {
			for(i in seq_along(value)) {
				l1 = value[[i]] > boxplot_stats[5,i]
				if(sum(l1)) grid.points(y = rep(n - i + 1, sum(l1)), x = value[[i]][l1], 
					default.units = "native", gp = subset_gp(gp, i), pch = pch[i], size = size[i])
				l2 = value[[i]] < boxplot_stats[1,i]
				if(sum(l2)) grid.points(y = rep(n - i + 1, sum(l2)), x = value[[i]][l2], 
					default.units = "native", gp = subset_gp(gp, i), pch = pch[i], size = size[i])
			}
		}
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}
	column_fun = function(index) {
		value = value[index]
		boxplot_stats = boxplot(value, plot = FALSE)$stats

		n = length(index)
		gp = subset_gp(gp, index)
		pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = data_scale))
		grid.rect(x = seq_along(index), y = boxplot_stats[2, ], 
			height = boxplot_stats[4, ] - boxplot_stats[2, ], width = 1*box_width, just = "bottom", 
			default.units = "native", gp = gp)
		
		grid.segments(seq_along(index) - 0.5*box_width, boxplot_stats[5, ],
			          seq_along(index) + 0.5*box_width, boxplot_stats[5, ], 
			          default.units = "native", gp = gp)
		grid.segments(seq_along(index), boxplot_stats[5, ],
			          seq_along(index), boxplot_stats[4, ], 
			          default.units = "native", gp = gp)
		grid.segments(seq_along(index), boxplot_stats[1, ],
			          seq_along(index), boxplot_stats[2, ], 
			          default.units = "native", gp = gp)
		grid.segments(seq_along(index) - 0.5*box_width, boxplot_stats[1, ],
			          seq_along(index) + 0.5*box_width, boxplot_stats[1, ], 
			          default.units = "native", gp = gp)
		grid.segments(seq_along(index) - 0.5*box_width, boxplot_stats[3, ],
			          seq_along(index) + 0.5*box_width, boxplot_stats[3, ], 
			          default.units = "native", gp = gp)
		if(outline) {	
			for(i in seq_along(value)) {
				l1 = value[[i]] > boxplot_stats[5,i]
				if(sum(l1)) grid.points(x = rep(i, sum(l1)), y = value[[i]][l1], 
					default.units = "native", gp = subset_gp(gp, i), pch = pch[i], size = size[i])
				l2 = value[[i]] < boxplot_stats[1,i]
				if(sum(l2)) grid.points(x = rep(i, sum(l2)), y = value[[i]][l2], 
					default.units = "native", gp = subset_gp(gp, i), pch = pch[i], size = size[i])
			}
		}
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}
	
	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_boxplot",
		which = which,
		n = n,
		width = anno_size$width,
		height = anno_size$height,
		data_scale = data_scale,
		var_import = list(value, gp, border, box_width, axis, axis_param, axis_grob, data_scale, pch, size, outline)
	)

	anno@subset_rule$value = subset_vector
	anno@subset_rule$gp = subset_gp
	anno@subset_rule$pch = subset_vector
	anno@subset_rule$size = subset_vector
	
	anno@subsetable = TRUE

	anno@extended = update_anno_extend(anno, axis_grob, axis_param)

	return(anno) 
}

# == title
# Using histogram as annotation
#
# == param
# -x a matrix or a list. If ``x`` is a matrix and if ``which`` is ``column``, statistics for histogram
#    is calculated by columns, if ``which`` is ``row``, the calculation is by rows.
# -which is the annotation a column annotation or a row annotation?
# -gp graphic parameters
# -... pass to `graphics::hist`
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_histogram = function(x, which = c("column", "row"), n_breaks = 11, 
	border = FALSE, gp = gpar(fill = "#CCCCCC"), 
	axis = TRUE, axis_param = default_axis_param(which), 
	width = NULL, height = NULL) {
	
	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	anno_size = anno_width_and_height(which, width, height, unit(4, "cm"))

	## convert matrix all to list (or data frame)
	if(is.matrix(x)) {
		if(which == "column") {
			value = as.data.frame(x)
		} else if(which == "row") {
			value = as.data.frame(t(x))
		}
	} else {
		value = x
	}

	n = length(value)
	x_range =range(unlist(value), na.rm = TRUE)
	histogram_stats = lapply(value, hist, plot = FALSE, breaks = seq(x_range[1], x_range[2], length = n_breaks))
	histogram_breaks = lapply(histogram_stats, function(x) x$breaks)
	histogram_counts = lapply(histogram_stats, function(x) x$counts)

	xscale = range(unlist(histogram_breaks), na.rm = TRUE)
	xscale = xscale + c(0, 0.05)*(xscale[2] - xscale[1])
	yscale = c(0, max(unlist(histogram_counts)))
	yscale[2] = yscale[2]*1.05

	gp = recycle_gp(gp, n)
	axis_param = validate_axis_param(axis_param, which)
	axis_grob = if(axis) construct_axis_grob(axis_param, which, xscale) else NULL

	row_fun = function(index) {
		
		n_all = length(value)
		value = value[index]
		
		n = length(index)
		histogram_breaks = histogram_breaks[index]
		histogram_counts = histogram_counts[index]

		gp = subset_gp(gp, index)
		for(i in seq_len(n)) {
			n_breaks = length(histogram_breaks[[i]])
			pushViewport(viewport(x = unit(0, "npc"), y = unit((n-i)/n, "npc"), height = unit(1/n, "npc"), just = c("left", "bottom"), xscale = xscale, yscale = yscale))
			grid.rect(x = histogram_breaks[[i]][-1], y = 0, width = histogram_breaks[[i]][-1] - histogram_breaks[[i]][-n_breaks], height = histogram_counts[[i]], just = c("right", "bottom"), default.units = "native", gp = subset_gp(gp, i))	
			popViewport()
		}
		pushViewport(viewport(xscale = xscale))
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}
	column_fun = function(index) {
		
		n_all = length(value)
		value = value[index]
		
		foo = yscale
		yscale = xscale
		xscale = foo
		histogram_breaks = histogram_breaks[index]
		histogram_counts = histogram_counts[index]

		n = length(index)
		
		gp = subset_gp(gp, index)
		for(i in seq_len(n)) {
			n_breaks = length(histogram_breaks[[i]])
			pushViewport(viewport(y = unit(0, "npc"), x = unit(i/n, "npc"), width = unit(1/n, "npc"), 
				just = c("right", "bottom"), xscale = xscale, yscale = yscale))
			grid.rect(y = histogram_breaks[[i]][-1], x = 0, height = histogram_breaks[[i]][-1] - histogram_breaks[[i]][-n_breaks], 
				width = histogram_counts[[i]], just = c("left", "top"), default.units = "native", gp = subset_gp(gp, index[i]))	
			popViewport()
		}
		pushViewport(viewport(yscale = yscale))
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}
	
	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_histogram",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		n = n,
		data_scale = xscale,
		var_import = list(value, gp, border, axis, axis_param, axis_grob, xscale, yscale,
			histogram_breaks, histogram_counts)
	)

	anno@subset_rule$value = subset_vector
	anno@subset_rule$gp = subset_gp
	anno@subset_rule$histogram_breaks = subset_vector
	anno@subset_rule$histogram_counts = subset_vector
	
	anno@subsetable = TRUE

	anno@extended = update_anno_extend(anno, axis_grob, axis_param)

	return(anno) 
}

# == title
# Using kernel density as annotation
#
# == param
# -x a matrix or a list. If ``x`` is a matrix and if ``which`` is ``column``, statistics for density
#    is calculated by columns, if ``which`` is ``row``, the calculation is by rows.
# -which is the annotation a column annotation or a row annotation?
# -gp graphic parameters. Note it is ignored if ``type`` equals to ``heatmap``.
# -type which type of graphics is used to represent density distribution.
# -... pass to `stats::density`
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_density = function(x, which = c("column", "row"), gp = gpar(fill = "#CCCCCC"),
	type = c("lines", "violin", "heatmap"), 
	heatmap_colors = rev(brewer.pal(name = "RdYlBu", n = 11)), 
	joyplot_scale = 1, border = TRUE,
	axis = TRUE, axis_param = default_axis_param(which),
	width = NULL, height = NULL) {
	
	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	anno_size = anno_width_and_height(which, width, height, unit(4, "cm"))

	## convert matrix all to list (or data frame)
	if(is.matrix(x)) {
		if(which == "column") {
			value = as.data.frame(x)
		} else if(which == "row") {
			value = as.data.frame(t(x))
		}
	} else {
		value = x
	}

	n = length(value)
	gp = recycle_gp(gp, n)
	type = match.arg(type)[1]

	n_all = length(value)
	density_stats = lapply(value, density)
	density_x = lapply(density_stats, function(x) x$x)
	density_y = lapply(density_stats, function(x) x$y)
	
	min_density_x = min(unlist(density_x))
	max_density_x = max(unlist(density_x))
	
	xscale = range(unlist(density_x), na.rm = TRUE)
	xscale = xscale + c(0, 0.05)*(xscale[2] - xscale[1])
	if(type == "lines") {
		yscale = c(0, max(unlist(density_y)))
		yscale[2] = yscale[2]*1.05
	} else if(type == "violin") {
		yscale = max(unlist(density_y))
		yscale = c(-yscale*1.05, yscale*1.05)
	} else if(type == "heatmap") {
		xscale = range(unlist(density_x), na.rm = TRUE)
		yscale = c(0, 1)
		min_y = min(unlist(density_y))
		max_y = max(unlist(density_y))
		col_fun = colorRamp2(seq(min_y, max_y, 
			length = length(heatmap_colors)), heatmap_colors)
	}

	axis_param = validate_axis_param(axis_param, which)
	axis_grob = if(axis) construct_axis_grob(axis_param, which, xscale) else NULL

	row_fun = function(index) {
		
		n = length(index)
		value = value[index]
		
		gp = subset_gp(gp, index)
		density_x = density_x[index]
		density_y = density_y[index]

		for(i in seq_len(n)) {
			pushViewport(viewport(x = unit(0, "npc"), y = unit((n-i)/n, "npc"), 
				just = c("left", "bottom"), height = unit(1/n, "npc"), xscale = xscale, 
				yscale = yscale))
			if(type == "lines") {
				grid.polygon(x = density_x[[i]], y = density_y[[i]]*joyplot_scale, 
					default.units = "native", gp = subset_gp(gp, i))
			} else if(type == "violin") {
				grid.polygon(x = c(density_x[[i]], rev(density_x[[i]])), 
					y = c(density_y[[i]], -rev(density_y[[i]])), default.units = "native", 
					gp = subset_gp(gp, i))
				box_stat = boxplot(value[[i]], plot = FALSE)$stat
				grid.lines(box_stat[1:2, 1], c(0, 0), default.units = "native", 
					gp = subset_gp(gp, i))
				grid.lines(box_stat[4:5, 1], c(0, 0), default.units = "native", 
					gp = subset_gp(gp, i))
				grid.points(box_stat[3, 1], 0, default.units = "native", pch = 3, 
					size = unit(1, "mm"), gp = subset_gp(gp, i))
			} else if(type == "heatmap") {
				n_breaks = length(density_x[[i]])
				grid.rect(x = density_x[[i]][-1], y = 0, 
					width = density_x[[i]][-1] - density_x[[i]][-n_breaks], height = 1, 
					just = c("right", "bottom"), default.units = "native", 
					gp = gpar(fill = col_fun((density_y[[i]][-1] + density_y[[i]][-n_breaks])/2), 
						col = NA))
				grid.rect(x = density_x[[i]][1], y = 0, width = density_x[[i]][1] - min_density_x, 
					height = 1, just = c("right", "bottom"), default.units = "native", 
					gp = gpar(fill = col_fun(0), col = NA))
				grid.rect(x = density_x[[i]][n_breaks], y = 0, 
					width = max_density_x - density_x[[i]][n_breaks], height = 1, 
					just = c("left", "bottom"), default.units = "native", 
					gp = gpar(fill = col_fun(0), col = NA))
			}
			popViewport()
		}
		pushViewport(viewport(xscale = xscale))
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}
	column_fun = function(index) {

		n_all = length(value)
		value = value[index]
		
		foo = yscale
		yscale = xscale
		xscale = foo

		density_x = density_x[index]
		density_y = density_y[index]
		
		yscale = range(unlist(density_x), na.rm = TRUE)
		yscale = yscale + c(0, 0.05)*(yscale[2] - yscale[1])
		if(type == "lines") {
			xscale = c(0, max(unlist(density_y)))
			xscale[2] = xscale[2]*1.05
		} else if(type == "violin") {
			xscale = max(unlist(density_y))
			xscale = c(-xscale*1.05, xscale*1.05)
		} else if(type == "heatmap") {
			yscale = range(unlist(density_x), na.rm = TRUE)
			xscale = c(0, 1)
			min_y = min(unlist(density_y))
			max_y = max(unlist(density_y))
			col_fun = colorRamp2(seq(min_y, max_y, 
				length = length(heatmap_colors)), heatmap_colors)
		}

		n = length(index)
		gp = subset_gp(gp, index)

		for(i in rev(seq_len(n))) {
			pushViewport(viewport(y = unit(0, "npc"), x = unit(i/n, "npc"), width = unit(1/n, "npc"), 
				just = c("right", "bottom"), xscale = xscale, yscale = yscale))
			if(type == "lines") {
				grid.polygon(y = density_x[[i]], x = density_y[[i]]*joyplot_scale, 
					default.units = "native", gp = subset_gp(gp, index[i]))
			} else if(type == "violin") {
				grid.polygon(y = c(density_x[[i]], rev(density_x[[i]])), 
					x = c(density_y[[i]], -rev(density_y[[i]])), default.units = "native", 
					gp = subset_gp(gp, index[i]))
				box_stat = boxplot(value[[i]], plot = FALSE)$stat
				grid.lines(y = box_stat[1:2, 1], x = c(0, 0), default.units = "native", 
					gp = subset_gp(gp, i))
				grid.lines(y = box_stat[4:5, 1], x = c(0, 0), default.units = "native", 
					gp = subset_gp(gp, i))
				grid.points(y = box_stat[3, 1], x = 0, default.units = "native", pch = 3, 
					size = unit(1, "mm"), gp = subset_gp(gp, i))	
			} else if(type == "heatmap") {
				n_breaks = length(density_x[[i]])
				grid.rect(y = density_x[[i]][-1], x = 0, 
					height = density_x[[i]][-1] - density_x[[i]][-n_breaks], width = 1, 
					just = c("left", "top"), default.units = "native", 
					gp = gpar(fill = col_fun((density_y[[i]][-1] + density_y[[i]][-n_breaks])/2), 
						col = NA))
				grid.rect(y = density_x[[i]][1], x = 0, height = density_x[[i]][1] - min_density_x, 
					width = 1, just = c("left", "top"), default.units = "native", 
					gp = gpar(fill = col_fun(0), col = NA))
				grid.rect(y = density_x[[i]][n_breaks], x = 0, 
					height = max_density_x - density_x[[i]][n_breaks], width = 1, 
					just = c("left", "bottom"), default.units = "native", 
					gp = gpar(fill = col_fun(0), col = NA))
			}
			popViewport()
		}
		pushViewport(viewport(yscale = yscale))
		if(axis) grid.draw(axis_grob)
		if(border) grid.rect(gp = gpar(fill = "transparent"))
		popViewport()
	}
	
	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_density",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		n = n,
		data_scale = xscale,
		var_import = list(value, gp, border, type, axis, axis_param, axis_grob, xscale, yscale, density_x,
			density_y, min_density_x, max_density_x, joyplot_scale)
	)

	if(type == "heatmap") {
		anno@var_env$col_fun = col_fun
	}

	anno@subset_rule$value = subset_vector
	anno@subset_rule$gp = subset_gp
	anno@subset_rule$density_x = subset_vector
	anno@subset_rule$density_y = subset_vector
	
	anno@subsetable = TRUE

	anno@extended = update_anno_extend(anno, axis_grob, axis_param)

	return(anno)
}

# == title
# Using text as annotation
#
# == param
# -x a vector of text
# -which is the annotation a column annotation or a row annotation?
# -gp graphic parameters.
# -rot rotation of text
# -just justification of text, pass to `grid::grid.text`
# -offset if it is a row annotation, ``offset`` corresponds to the x-coordinates of text.
#         and if it is a column annotation, ``offset`` corresponds to the y-coordinates of text.
#         The value should be a `grid::unit` object.
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_text = function(x, which = c("column", "row"), gp = gpar(), 
	rot = guess_rot(), just = guess_just(), 
	offset = guess_location(), location = guess_location(),
	width = NULL, height = NULL) {

	if(exists(".__under_SingleAnnotation__", envir = parent.frame())) {
		which = get("which", envir = parent.frame())
	} else {
		which = match.arg(which)[1]
	}

	n = length(x)
	gp = recycle_gp(gp, n)

	guess_rot = function() {
		ifelse(which == "column", 90, 0)
	}

	guess_just = function() {
		ifelse(which == "column", "right", "left")
	}

	guess_location = function() {
		unit(ifelse(which == "column", 1, 0), "npc")
	}

	rot = rot[1] %% 360
	just = just[1]
	if(!missing(offset)) {
		warning("`offset` is deprecated, use `location` instead.")
		if(missing(location)) {
			location = offset
		}
	}
	location = location[1]
	if(!inherits(location, "unit")) {
		location = unit(location, "npc")
	}

	if(which == "column") {
		if("right" %in% just) {
			location = location - 0.5*grobHeight(textGrob("A", gp = gp))*abs(cos(rot/180*pi))
		} else if("left" %in% just) {
			location = location + 0.5*grobHeight(textGrob("A", gp = gp))*abs(cos(rot/180*pi))
		}
	}

	if(which == "column") {
		if(missing(height)) {
			height = max_text_width(x, gp = gp)*abs(sin(rot/180*pi)) + grobHeight(textGrob("A", gp = gp))*abs(cos(rot/180*pi))
			height = convertHeight(height, "mm")
		}
		if(missing(width)) {
			width = unit(1, "npc")
		}
	}
	if(which == "row") {
		if(missing(width)) {
			width = max_text_width(x, gp = gp)*cos(rot/180*pi) + grobHeight(textGrob("A", gp = gp))*sin(rot/180*pi)
			width = convertWidth(width, "mm")
		}
		if(missing(height)) {
			height = unit(1, "npc")
		}
	}

	anno_size = list(width = width, height = height)

	value = x

	row_fun = function(index) {
		n = length(index)
		grid.text(value[index], location, (n - seq_along(index) + 0.5)/n, gp = subset_gp(gp, index), just = just, rot = rot)
	}
	column_fun = function(index, k = NULL, N = NULL, vp_name = NULL) {
		n = length(index)
		grid.text(value[index], (seq_along(index) - 0.5)/n, location, gp = subset_gp(gp, index), just = just, rot = rot)
	}

	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_text",
		which = which,
		width = width,
		height = height,
		n = n,
		var_import = list(value, gp, just, rot, location)
	)

	anno@subset_rule$value = subset_vector
	anno@subset_rule$gp = subset_gp

	anno@subsetable = TRUE

	return(anno)
}

anno_joyplot = function(x, which = c("column", "row"), gp = gpar(fill = "#000000"),
	scale = 2, transparency = 0.6,
	axis = TRUE, axis_param = default_axis_param(which),
	width = NULL, height = NULL) {
	
	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	anno_size = anno_width_and_height(which, width, height, unit(4, "cm"))

	## convert matrix all to list (or data frame)
	if(is.matrix(x) || is.data.frame(x)) {
		value = vector("list", ncol(x))
		for(i in seq_len(ncol(x))) {
			value[[i]] = cbind(seq_len(nrow(x), x[, i]))
		}
	} else if(inherits(x, "list")){
		if(all(sapply(x, is.atomic))) {
			if(length(unique(sapply(x, length))) == 1) {
				value = vector("list", length(x))
				for(i in seq_len(length(x))) {
					value[[i]] = cbind(seq_along(x[[i]]), x[[i]])
				}
			} else {
				stop("Since x is a list, x need to be a list of two-column matrices.")
			}
		} else {
			value = x
		}
	} else {
		stop("The input should be a list of two-column matrices or a matrix/data frame.")
	}

	xscale = range(lapply(value, function(x) x[, 1]), na.rm = TRUE)
	xscale = xscale + c(-0.05, 0.05)*(xscale[2] - xscale[1])
	yscale = range(lapply(value, function(x) x[, 2]), na.rm = TRUE)
	yscale[1] = 0
	yscale[2] = yscale[2]*1.05

	n = length(value)

	if(!"fill" %in% names(gp)) {
		gp$fill = "#000000"
	} 
	gp = recycle_gp(gp, n)
	gp$fill = add_transparency(gp$fill, transparency)
	axis_param = validate_axis_param(axis_param, which)
	axis_grob = if(axis) construct_axis_grob(axis_param, which, xscale) else NULL

	row_fun = function(index) {

		n_all = length(value)
		value = value[index]
		
		n = length(index)
		gp = subset_gp(gp, index)

		for(i in seq_len(n)) {
			pushViewport(viewport(x = unit(0, "npc"), y = unit((n-i)/n, "npc"), 
				just = c("left", "bottom"), height = unit(1/n, "npc"), xscale = xscale, 
				yscale = yscale))
			
			x0 = value[[i]][, 1]
			y0 = value[[i]][, 2]*scale
			x0 = c(x0[1], x0, x0[length(x0)])
			y0 = c(0, y0, 0)
			gppp = subset_gp(gp, i); gppp$col = NA
			grid.polygon(x = x0, y = y0, default.units = "native", gp = gppp)
			grid.lines(x = x0, y = y0, default.units = "native", 
				gp = subset_gp(gp, i))
			
			popViewport()
		}
		pushViewport(viewport(xscale = xscale))
		if(axis) grid.draw(axis_grob)
		popViewport()
	}
	column_fun = function(index) {

		n_all = length(value)
		value = value[index]
		
		foo = yscale
		yscale = xscale
		xscale = foo
		
		n = length(index)
		
		gp = subset(gp, index)

		for(i in seq_len(n)) {
			pushViewport(viewport(y = unit(0, "npc"), x = unit(i/n, "npc"), 
				width = unit(1/n, "npc"), just = c("right", "bottom"), xscale = xscale, 
				yscale = yscale))
			
			x0 = value[[i]][, 2]*scale
			y0 = value[[i]][ ,1]
			x0 = c(0, x0, 0)
			y0 = c(y0[1], y0, y0[length(y0)])
			gppp = subset_gp(gp, i); gppp$col = NA
			grid.polygon(y = y0, x = x0, default.units = "native", gp = gppp)
			grid.lines(y = y0, x = x0, default.units = "native", 
				gp = subset_gp(gp, i))
			
			popViewport()
		}
		pushViewport(viewport(yscale = yscale))
		if(axis) grid.draw(axis_grob)
		popViewport()
	}
	
	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_joyplot",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		n = n,
		data_scale = xscale,
		var_import = list(value, gp, axis, axis_param, axis_grob, scale, yscale, xscale)
	)

	anno@subset_rule$value = subset_vector
	anno@subset_rule$gp = subset_gp

	anno@subsetable = TRUE

	anno@extended = update_anno_extend(anno, axis_grob, axis_param)

	return(anno)
}


anno_horizon = function(x, which = c("column", "row"), 
	gp = gpar(pos_fill = "#D73027", neg_fill = "#313695"),
	n_slice = 4, slice_size = NULL, negative_from_top = FALSE, 
	normalize = TRUE, border = FALSE, gap = unit(0, "mm"),
	axis = TRUE, axis_param = default_axis_param(which),
	width = NULL, height = NULL) {

	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	anno_size = anno_width_and_height(which, width, height, unit(4, "cm"))

	## convert matrix all to list (or data frame)
	if(is.matrix(x) || is.data.frame(x)) {
		value = vector("list", ncol(x))
		for(i in seq_len(ncol(x))) {
			value[[i]] = cbind(seq_len(nrow(x), x[, i]))
		}
	} else if(inherits(x, "list")){
		if(all(sapply(x, is.atomic))) {
			if(length(unique(sapply(x, length))) == 1) {
				value = vector("list", length(x))
				for(i in seq_len(length(x))) {
					value[[i]] = cbind(seq_along(x[[i]]), x[[i]])
				}
			} else {
				stop("Since x is a list, x need to be a list of two-column matrices.")
			}
		} else {
			value = x
		}
	} else {
		stop("The input should be a list of two-column matrices or a matrix/data frame.")
	}

	if(is.null(gp$pos_fill)) gp$pos_fill = "#D73027"
	if(is.null(gp$neg_fill)) gp$neg_fill = "#313695"

	if("fill" %in% names(gp)) {
		foo = unlist(lapply(value, function(x) x[, 2]))
		if(all(foo >= 0)) {
			gp$pos_fill = gp$fill
		} else if(all(foo <= 0)) {
			gp$neg_fill = gp$fill
		} else {
			gp = gpar(pos_fill = "#D73027", neg_fill = "#313695")
		}
	}

	if(which == "column") {
		stop("anno_horizon() does not support column annotation. If you want, please email me.")
	}

	if(normalize) {
		value = lapply(value, function(m) {
			m[, 2] = m[, 2]/max(abs(m[, 2]))
			m
		})
	}

	n = length(value)
	xscale = range(lapply(value, function(x) x[, 1]), na.rm = TRUE)
	yscale = range(lapply(value, function(x) abs(x[, 2])), na.rm = TRUE)
	
	axis_param = validate_axis_param(axis_param, which)
	axis_grob = if(axis) construct_axis_grob(axis_param, which, xscale) else NULL

	row_fun = function(index) {

		n_all = length(value)
		value = value[index]
		
		if(is.null(slice_size)) {
			slice_size = yscale[2]/n_slice
		} 
		n_slice = ceiling(yscale[2]/slice_size)
		
		n = length(index)
		
		gp = subset_gp(gp, index)

		for(i in seq_len(n)) {
			pushViewport(viewport(x = unit(0, "npc"), y = unit((n-i)/n, "npc"), just = c("left", "bottom"), 
				height = unit(1/n, "npc") - gap))
			sgp = subset_gp(gp, i)
			horizon_chart(value[[i]][, 1], value[[i]][, 2], n_slice = n_slice, slice_size = slice_size, 
				negative_from_top = negative_from_top, pos_fill = sgp$pos_fill, neg_fill = sgp$neg_fill)
			grid.rect(gp = gpar(fill = "transparent"))
			
			popViewport()
		}
		pushViewport(viewport(xscale = xscale))
		if(axis) grid.draw(axis_grob)
		popViewport()
	}
	column_fun = function(index) {

	}
	
	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_horizon",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		n = n,
		data_scale = xscale,
		var_import = list(value, gp, border, axis, axis_param, axis_grob, n_slice, slice_size,
			negative_from_top, xscale, yscale, gap)
	)

	anno@subset_rule$value = subset_vector
	anno@subset_rule$gp = subset_gp

	anno@subsetable = TRUE

	anno@extended = update_anno_extend(anno, axis_grob, axis_param)

	return(anno)
}

horizon_chart = function(x, y, n_slice = 4, slice_size, pos_fill = "#D73027", neg_fill = "#313695",
	negative_from_top = FALSE) {

	if(missing(slice_size)) {
		slice_size = max(abs(y))/n_slice
	}
	n_slice = ceiling(max(abs(y))/slice_size)

	if(n_slice == 0) {
		return(invisible(NULL))
	}

	pos_col_fun = colorRamp2(c(0, n_slice), c("white", pos_fill))
	neg_col_fun = colorRamp2(c(0, n_slice), c("white", neg_fill))
	pushViewport(viewport(xscale = range(x), yscale = c(0, slice_size)))
	for(i in seq_len(n_slice)) {
		l1 = y >= (i-1)*slice_size & y < i*slice_size
		l2 = y < (i-1)*slice_size
		l3 = y >= i*slice_size
		if(any(l1)) {
			x2 = x
			y2 = y
			y2[l1] = y2[l1] - slice_size*(i-1)
			y2[l3] = slice_size
			x2[l2] = NA
			y2[l2] = NA

			add_horizon_polygon(x2, y2, gp = gpar(fill = pos_col_fun(i), col = NA), 
				default.units = "native")
		}
	}
	y = -y
	for(i in seq_len(n_slice)) {
		l1 = y >= (i-1)*slice_size & y < i*slice_size
		l2 = y < (i-1)*slice_size
		l3 = y >= i*slice_size
		if(any(l1)) {
			x2 = x
			y2 = y
			y2[l1] = y2[l1] - slice_size*(i-1)
			y2[l3] = slice_size
			x2[l2] = NA
			y2[l2] = NA
			add_horizon_polygon(x2, y2, slice_size = slice_size, from_top = negative_from_top, 
				gp = gpar(fill = neg_col_fun(i), col = NA), default.units = "native")
		}
	}
	popViewport()
}

# x and y may contain NA, split x and y by NA gaps, align the bottom to y = 0
add_horizon_polygon = function(x, y, slice_size = NULL, from_top = FALSE, ...) {
	ltx = split_vec_by_NA(x)
	lty = split_vec_by_NA(y)

	for(i in seq_along(ltx)) {
		x0 = ltx[[i]]
		y0 = lty[[i]]
		if(from_top) {
			x0 = c(x0[1], x0, x0[length(x0)])
			y0 = c(slice_size, slice_size - y0, slice_size)
		} else {
			x0 = c(x0[1], x0, x0[length(x0)])
			y0 = c(0, y0, 0)
		}
		grid.polygon(x0, y0, ...)
	}
}

# https://stat.ethz.ch/pipermail/r-help/2010-April/237031.html
split_vec_by_NA = function(x) {
	idx = 1 + cumsum(is.na(x))
	not.na = !is.na(x)
	split(x[not.na], idx[not.na])
}


# == title
# Row annotation which is represented as points
#
# == param
# -... pass to `anno_points`
#
# == details
# A wrapper of `anno_points` with pre-defined ``which`` to ``row``.
#
# == value
# See help page of `anno_points`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
row_anno_points = function(...) {
	if(exists(".__under_SingleAnnotation__", envir = parent.frame())) {
		message("From this version of ComplexHeatmap, you can directly use `anno_points()` for row annotation if you call it in `rowAnnotation()`.")
	}
	anno_points(..., which = "row")
}

# == title
# Column annotation which is represented as points
#
# == param
# -... pass to `anno_points`
#
# == details
# A wrapper of `anno_points` with pre-defined ``which`` to ``column``.
#
# == value
# See help page of `anno_points`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
column_anno_points = function(...) {
	anno_points(..., which = "column")
}

# == title
# Row annotation which is represented as barplots
#
# == param
# -... pass to `anno_barplot`
#
# == details
# A wrapper of `anno_barplot` with pre-defined ``which`` to ``row``.
#
# == value
# See help page of `anno_barplot`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
row_anno_barplot = function(...) {
	anno_barplot(..., which = "row")
}

# == title
# Column annotation which is represented as barplots
#
# == param
# -... pass to `anno_barplot`
#
# == details
# A wrapper of `anno_barplot` with pre-defined ``which`` to ``column``.
#
# == value
# See help page of `anno_barplot`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
column_anno_barplot = function(...) {
	anno_barplot(..., which = "column")
}

# == title
# Row annotation which is represented as boxplots
#
# == param
# -... pass to `anno_boxplot`
#
# == details
# A wrapper of `anno_boxplot` with pre-defined ``which`` to ``row``.
#
# == value
# See help page of `anno_boxplot`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
row_anno_boxplot = function(...) {
	anno_boxplot(..., which = "row")
}

# == title
# Column annotation which is represented as boxplots
#
# == param
# -... pass to `anno_boxplot`
#
# == details
# A wrapper of `anno_boxplot` with pre-defined ``which`` to ``column``.
#
# == value
# See help page of `anno_boxplot`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
column_anno_boxplot = function(...) {
	anno_boxplot(..., which = "column")
}

# == title
# Row annotation which is represented as histogram
#
# == param
# -... pass to `anno_histogram`
#
# == details
# A wrapper of `anno_histogram` with pre-defined ``which`` to ``row``.
#
# == value
# See help page of `anno_histogram`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
row_anno_histogram = function(...) {
	anno_histogram(..., which = "row")
}

# == title
# Column annotation which is represented as histogram
#
# == param
# -... pass to `anno_histogram`
#
# == details
# A wrapper of `anno_histogram` with pre-defined ``which`` to ``column``.
#
# == value
# See help page of `anno_histogram`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
column_anno_histogram = function(...) {
	anno_histogram(..., which = "column")
}

# == title
# Row annotation which is represented as density plot
#
# == param
# -... pass to `anno_density`
#
# == details
# A wrapper of `anno_density` with pre-defined ``which`` to ``row``.
#
# == value
# See help page of `anno_density`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
row_anno_density = function(...) {
	anno_density(..., which = "row")
}

# == title
# Column annotation which is represented as density plot
#
# == param
# -... pass to `anno_density`
#
# == details
# A wrapper of `anno_density` with pre-defined ``which`` to ``column``.
#
# == value
# See help page of `anno_density`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
column_anno_density = function(...) {
	anno_density(..., which = "column")
}

# == title
# Row annotation which is represented as text
#
# == param
# -... pass to `anno_text`
#
# == details
# A wrapper of `anno_text` with pre-defined ``which`` to ``row``.
#
# == value
# See help page of `anno_text`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
row_anno_text = function(...) {
	anno_text(..., which = "row")
}

# == title
# Column annotation which is represented as text
#
# == param
# -... pass to `anno_text`
#
# == details
# A wrapper of `anno_text` with pre-defined ``which`` to ``column``.
#
# == value
# See help page of `anno_text`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
column_anno_text = function(...) {
	anno_text(..., which = "column")
}

# == title
# Link annotation with labels
#
# == param
# -at numeric index in the original matrix
# -labels corresponding labels
# -which column annotation or row annotation
# -side side of the labels. If it is a column annotation, permitted values are "top" and "bottom";
#       If it is a row annotation, permitted values are "left" and "right".
# -lines_gp graphic settings for the segments
# -labels_gp graphic settings for the labels
# -padding padding between labels if they are attached to each other
# -link_width, width of the segments.
# -extend by default, the region for the labels has the same width (if it is a column annotation) or
#         same height (if it is a row annotation) as the heatmap. The size can be extended by this options.
#         The value can be a proportion number or  a `grid::unit` object. The length can be either one or two.
#
# == details
# Sometimes there are many rows or columns in the heatmap and we want to mark some of the rows.
# This annotation function is used to mark these rows and connect labels and corresponding rows
# with links.
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
anno_mark = function(at, labels, which = c("column", "row"), side = ifelse(which == "column", "top", "right"),
	lines_gp = gpar(), labels_gp = gpar(), padding = 0.25, link_width = NULL, extend = 0) {

	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	if(!is.numeric(at)) {
		stop(paste0("`at` should be numeric ", which, " index corresponding to the matrix."))
	}
	
	n = length(at)

	# od = order(at)
	# at = at[od]
	# labels = labels[od]

	lines_gp = recycle_gp(lines_gp, n)
	labels_gp = recycle_gp(labels_gp, n)

	# lines_gp = subset_gp(lines_gp, od)
	# labels_gp = subset_gp(labels_gp, od)
	labels2index = structure(seq_along(at), names = labels)
	at2labels = structure(labels, names = at)

	if(length(extend) == 1) extend = rep(extend, 2)
	if(length(extend) > 2) extend = extend[1:2]
	if(!inherits(extend, "unit")) extend = unit(extend, "npc")

	if(which == "column") {
		if(missing(height)) {
			height = max_text_width(labels, gp = labels_gp) + unit(1, "mm")
		}
		if(missing(width)) {
			width = unit(1, "npc")
		}
	}
	if(which == "row") {
		if(missing(width)) {
			width = max_text_width(labels, gp = labels_gp) + unit(1, "mm")
		}
		if(missing(height)) {
			height = unit(1, "npc")
		}
	}

	### recalculate at in the whole parent viewport


	row_fun = function(index) {
		n = length(index)

		# adjust at and labels
		at = intersect(index, at)
		labels = rev(at2labels[as.character(at)])
		
		labels_gp = subset_gp(labels_gp, labels2index[labels])
		lines_gp = subset_gp(lines_gp, labels2index[labels])

		pushViewport(viewport(xscale = c(0, 1), yscale = c(0.5, n+0.5)))
		if(inherits(extend, "unit")) extend = convertHeight(extend, "native", valueOnly = TRUE)
		if(length(labels)) {
			text_height = convertHeight(grobHeight(textGrob(labels, gp = labels_gp))*(1+padding), "native", valueOnly = TRUE)
			i2 = rev(which(index %in% at))
			h1 = n-i2+1 - text_height*0.5
			h2 = n-i2+1 + text_height*0.5
			pos = rev(smartAlign(h1, h2, c(0.5 - extend[1], n+0.5 + extend[2])))
			h = (pos[, 1] + pos[, 2])/2

			if(is.null(link_width)) {
				if(convertWidth(unit(1, "npc") - max_text_width(labels, gp = labels_gp), "mm", valueOnly = TRUE) < 0) {
					link_width = unit(0.5, "npc")
				} else {
					link_width = unit(1, "npc") - max_text_width(labels, gp = labels_gp)
				}
			}
			n2 = length(labels)
			if(side == "right") {
				grid.text(labels, rep(link_width, n2), h, default.units = "native", gp = labels_gp, just = "left")
				link_width = link_width - unit(1, "mm")
				grid.segments(unit(rep(0, n2), "npc"), n-i2+1, rep(link_width*(1/3), n2), n-i2+1, default.units = "native", gp = lines_gp)
				grid.segments(rep(link_width*(1/3), n2), n-i2+1, rep(link_width*(2/3), n2), h, default.units = "native", gp = lines_gp)
				grid.segments(rep(link_width*(2/3), n2), h, rep(link_width, n2), h, default.units = "native", gp = lines_gp)
			} else {
				grid.text(labels, unit(1, "npc")-rep(link_width, n2), h, default.units = "native", gp = labels_gp, just = "right")
				link_width = link_width - unit(1, "mm")
				grid.segments(unit(rep(1, n2), "npc"), n-i2+1, unit(1, "npc")-rep(link_width*(1/3), n2), n-i2+1, default.units = "native", gp = lines_gp)
				grid.segments(unit(1, "npc")-rep(link_width*(1/3), n2), n-i2+1, unit(1, "npc")-rep(link_width*(2/3), n2), h, default.units = "native", gp = lines_gp)
				grid.segments(unit(1, "npc")-rep(link_width*(2/3), n2), h, unit(1, "npc")-rep(link_width, n2), h, default.units = "native", gp = lines_gp)
			}
		}
		upViewport()
	}
	column_fun = function(index) {
		n = length(index)
		
		# adjust at and labels
		at = intersect(index, at)
		labels = at2labels[as.character(at)]
		
		labels_gp = subset_gp(labels_gp, labels2index[labels])
		lines_gp = subset_gp(lines_gp, labels2index[labels])

		pushViewport(viewport(yscale = c(0, 1), xscale = c(0.5, n+0.5)))
		if(inherits(extend, "unit")) extend = convertWidth(extend, "native", valueOnly = TRUE)
		text_height = convertWidth(grobHeight(textGrob(labels, gp = labels_gp))*(1+padding), "native", valueOnly = TRUE)
		i2 = which(index %in% at)
		h1 = i2 - text_height*0.5
		h2 = i2 + text_height*0.5
		pos = smartAlign(h1, h2, c(0.5 - extend[1], n+0.5 + extend[2]))
		h = (pos[, 1] + pos[, 2])/2
		if(is.null(link_width)) {
			if(convertHeight(unit(1, "npc") - max_text_width(labels, gp = labels_gp), "mm", valueOnly = TRUE) < 0) {
				link_width = unit(0.5, "npc")
			} else {
				link_width = unit(1, "npc") - max_text_width(labels, gp = labels_gp)
			}
		}
		n2 = length(labels)
		if(side == "top") {
			grid.text(labels, h, rep(link_width, n2), default.units = "native", gp = labels_gp, rot = 90, just = "left")
			link_width = link_width - unit(1, "mm")
			grid.segments(i2, unit(rep(0, n2), "npc"), i2, rep(link_width*(1/3), n2), default.units = "native", gp = lines_gp)
			grid.segments(i2, rep(link_width*(1/3), n2), h, rep(link_width*(2/3), n2), default.units = "native", gp = lines_gp)
			grid.segments(h, rep(link_width*(2/3), n2), h, rep(link_width, n), default.units = "native", gp = lines_gp)
		} else {
			grid.text(labels, h, rep(max_text_width(labels, gp = labels_gp), n2), default.units = "native", gp = labels_gp, rot = 90, just = "right")
			link_width = link_width - unit(1, "mm")
			grid.segments(i2, unit(rep(1, n2), "npc"), i2, unit(1, "npc")-rep(link_width*(1/3), n2), default.units = "native", gp = lines_gp)
			grid.segments(i2, unit(1, "npc")-rep(link_width*(1/3), n2), h, unit(1, "npc")-rep(link_width*(2/3), n2), default.units = "native", gp = lines_gp)
			grid.segments(h, unit(1, "npc")-rep(link_width*(2/3), n2), h, unit(1, "npc")-rep(link_width, n2), default.units = "native", gp = lines_gp)
		}
		upViewport()
	}
	
	attr(f, "which") = which
	attr(f, "fun") = "anno_mark"
	attr(f, "width") = width
	attr(f, "height") = height
	attr(f, "parent_variable") = c("value", "gp", "border", "axis", "axis_side", "axis_gp", "n_bins", "bin_size", "negative_from_top")
	attr(f, "parent_variable_subsetable") = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
	
	return(f)
}

anno_link = function(...) {
	warning("anno_link() is deprecated, please use anno_mark() instead.")
	anno_mark(...)
}

# == title
# Column annotation which is represented as links
#
# == param
# -... pass to `anno_link`
#
# == details
# A wrapper of `anno_link` with pre-defined ``which`` to ``row``.
#
# == value
# See help page of `anno_link`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
row_anno_link = function(...) {
	anno_link(..., which = "row")
}

# == title
# Column annotation which is represented as links
#
# == param
# -... pass to `anno_link`
#
# == details
# A wrapper of `anno_link` with pre-defined ``which`` to ``column``.
#
# == value
# See help page of `anno_link`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
column_anno_link = function(...) {
	anno_link(..., which = "column")
}


normalize_graphic_param_to_mat = function(x, nc, nr, name) {
	if(is.matrix(x)) {
		if(nrow(x) == nr && ncol(x) == nc) {
			return(x)
		} else {
			stop(paste0(name, "needs to be a matrix with ", nc, " columns and ", nr, " rows."))
		}
	} else {
		if(length(x) == nc) {
			return(matrix(rep(x, each = nr), nc = nc))
		} else if(length(x) == nr) {
			return(matrix(rep(x, times = nc), nc = nc))
		} else if(length(x) == 1) {
			return(matrix(x, nc = nc, nr = nr))
		} else {
			stop(paste0("Since ", name, " is a vector, it should have length of ", nc, " or ", nr, "."))
		}
	}
}
