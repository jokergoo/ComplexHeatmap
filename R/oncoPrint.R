
# == title
# Make oncoPrint
#
# == param
# -mat The value should be a character matrix which encodes mulitple alterations 
#      or a list of matrices for which every matrix contains binary
#      value representing whether the alteration is present or absent. 
#      When the value is a list, the names of the list represent alteration types.
#      You can use `unify_mat_list` to make all matrix having same row names and column names.
# -name Name of the oncoPrint. Not necessary to specify.
# -get_type If different alterations are encoded in the matrix as complex strings, this self-defined function
#           determines how to extract them. It only works when ``mat`` is a matrix. The default value is `default_get_type`.
# -alter_fun A single function or a list of functions which defines how to add graphics for different alterations.
#      You can use `alter_graphic` to automatically generate for rectangles and points.
# -alter_fun_is_vectorized Whether ``alter_fun`` is implemented vectorized. Internally the function will guess.
# -col A vector of color for which names correspond to alteration types.
# -top_annotation Annotation put on top of the oncoPrint. By default it is barplot which shows the number of genes with a certain alteration in each sample.
# -right_annotation Annotation put on the right of the oncoPrint. By default it is barplot which shows the number of samples with a certain alteration in each gene.
# -left_annotation Annotation put on the left of the oncoPrint.
# -bottom_annotation Annotation put at the bottom of the oncoPrint.
# -show_pct whether show percent values on the left of the oncoprint?
# -pct_gp Graphic paramters for percent values
# -pct_digits Digits for the percent values.
# -pct_side Side of the percent values to the oncoPrint. This argument is currently disabled.
# -pct_include Alteration types that are included for the calculation of percent values.
# -row_labels Labels as the row names of the oncoPrint.
# -show_row_names Whether show row names?
# -row_names_side Side of the row names to the oncoPrint. This argument is currently disabled.
# -row_names_gp Graphic parameters for the row names.
# -row_split Pass to `Heatmap`.
# -column_labels Pass to `Heatmap`.
# -column_names_gp Pass to `Heatmap`.
# -column_split Pass to `Heatmap`.
# -row_order Order of rows. By default rows are sorted by the number of occurence of the alterations.
# -cluster_rows If it is set, it must be a dendrogram/hclust object.
# -cluster_columns If it is set, it must be a dendrogram/hclust object.
# -column_order Order of columns. By default the columns are sorted to show the mutual exclusivity of alterations.
# -remove_empty_columns If there is no alteration in some samples, whether remove them on the oncoPrint?
# -remove_empty_rows If there is no alteration in some samples, whether remove them on the oncoPrint?
# -show_column_names Whether show column names?
# -heatmap_legend_param pass to `Heatmap`.
# -... Pass to `Heatmap`.
#
# == details
# The 'memo sort' method is from https://gist.github.com/armish/564a65ab874a770e2c26 . Thanks to
# B. Arman Aksoy for contributing the code.
#
# https://jokergoo.github.io/ComplexHeatmap-reference/book/oncoprint.html gives details for configuring
# a oncoPrint.
#
# == value
# A `Heatmap-class` object which means you can add other heatmaps or annotations to it.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
oncoPrint = function(mat, name,
	get_type = default_get_type,
	alter_fun, 
	alter_fun_is_vectorized = NULL,
	col = NULL, 

	top_annotation = HeatmapAnnotation(cbar = anno_oncoprint_barplot()),
	right_annotation = rowAnnotation(rbar = anno_oncoprint_barplot()),
	left_annotation = NULL,
	bottom_annotation = NULL,

	show_pct = TRUE, 
	pct_gp = gpar(fontsize = 10), 
	pct_digits = 0,
	pct_side = "left",
	pct_include = NULL,

	row_labels = NULL,
	show_row_names = TRUE,
	row_names_side = "right",
	row_names_gp = pct_gp,
	row_split = NULL,

	column_labels = NULL,
	column_names_gp = gpar(fontsize = 10),
	column_split = NULL,

	row_order = NULL,
	column_order = NULL,
	cluster_rows = FALSE,
	cluster_columns = FALSE,
	
	remove_empty_columns = FALSE,
	remove_empty_rows = FALSE,
	show_column_names = FALSE,
	heatmap_legend_param = NULL,
	...) {

	dev.null()
    on.exit(dev.off2())

	arg_list = as.list(match.call())[-1]
	arg_names = names(arg_list)

	if("alter_fun_list" %in% arg_names) {
		stop_wrap("`alter_fun_list` is removed from the arguments.")
	}
	if("axis_gp" %in% arg_names) {
		stop_wrap("`axis_gp` is removed from the arguments. Please set `axis_param(gp = ...)` in `anno_oncoprint_barplot()` when you define the `top_annotation` or `right_annotation`.")
	}
	if("show_row_barplot" %in% arg_names) {
		stop_wrap("`show_row_barplot` is removed from the arguments. Please directly remove `anno_oncoprint_barplot()` in `right_annotation` to remove barplots on the right of the oncoPrint.")
	}
	if("row_barplot_width" %in% arg_names) {
		stop_wrap("`row_barplot_width` is removed from the arguments. Please directly set `width` in `anno_oncoprint_barplot()` in `right_annotation`.")
	}
	if("top_annotation_height" %in% arg_names) {
		stop_wrap("`top_annotation_height` is removed from the arguments. Please directly set `height` in `anno_oncoprint_barplot()` in `top_annotation`.")
	}
	if("bottom_annotation_height" %in% arg_names) {
		stop_wrap("`bottom_annotation_height` is removed from the arguments. Please directly set `height` in `bottom_annotation`.")
	}
	if("barplot_ignore" %in% arg_names) {
		stop_wrap("`barplot_ignore` is removed from the arguments. The subset of alterations now can be controlled in `anno_oncoprint_barplot()`.")
	}

	if(inherits(col, "function")) {
		stop_wrap("`col` should be specified as a vector.")
	}

	# convert mat to mat_list
	if(inherits(mat, "data.frame")) {
		mat = as.matrix(mat)
	}
	if(inherits(mat, "matrix")) {
		get_type2 = function(x) gsub("^\\s+|\\s+$", "", get_type(x))
		all_type = unique(unlist(lapply(mat, get_type2)))
		all_type = all_type[!is.na(all_type)]
		all_type = all_type[grepl("\\S", all_type)]

		## check whether there are NA values in the matrix
		if(any(is.na(mat))) {
			message_wrap("Found NA values in the matrix and treat as no alteration. If `NA` means no alteration, you can explicitly set it to empty strings like ''. If `NA` is an alteration type, you should format it to a string like `'NA'` and define graphics for it.")
		}

		mat_list = lapply(all_type, function(type) {
			m = sapply(mat, function(x) type %in% get_type2(x))
			dim(m) = dim(mat)
			dimnames(m) = dimnames(mat)
			m
		})
		names(mat_list) = all_type
	} else if(inherits(mat, "list")) {
		mat_list = mat

		all_type = names(mat_list)
		mat_list = lapply(mat_list, function(x) {
				if(!is.matrix(x)) {
					stop_wrap("Expect a list of matrix (not data frames).")
				}
				oattr = attributes(x)
				x = as.logical(x)
				attributes(x) = oattr
				x
			})

		if(length(unique(sapply(mat_list, nrow))) > 1) {
			stop_wrap("All matrix in 'mat_list' should have same number of rows.")
		}

		if(length(unique(sapply(mat_list, ncol))) > 1) {
			stop_wrap("All matrix in 'mat_list' should have same number of columns.")
		}
	} else {
		stop_wrap("Incorrect type of 'mat'")
	}

	message_wrap(paste0("All mutation types: ", paste(all_type, collapse = ", "), "."))

	# type as the third dimension
	if(is.null(dimnames(mat_list[[1]]))) {
		dimnames = c(list(NULL), list(NULL), list(all_type))
	} else {
		dimnames = c(dimnames(mat_list[[1]]), list(all_type))
	}
	arr = array(FALSE, dim = c(dim(mat_list[[1]]), length(all_type)), dimnames = dimnames)
	for(i in seq_along(all_type)) {
		arr[, , i] = mat_list[[i]]
	}

	if(missing(name)) {
        name = paste0("oncoPrint_", get_oncoprint_index() + 1)
        increase_oncoprint_index()
    } else if(is.null(name)) {
        name = paste0("oncnPrint_", get_oncoprint_index() + 1)
        increase_oncoprint_index()
    }

	oncoprint_row_order = function() {
		order(rowSums(count_matrix), n_mut, decreasing = TRUE)
	}

	oncoprint_column_order = function() {
		scoreCol = function(x) {
			score = 0
			for(i in 1:length(x)) {
				if(x[i]) {
					score = score + 2^(length(x)-i*1/x[i])
				}
			}
			return(score)
		}
		scores = apply(count_matrix[row_order, ,drop = FALSE], 2, scoreCol)
		order(scores, decreasing=TRUE)
	}

	if(missing(alter_fun)) {
		if(length(mat_list) == 1) {
			alter_fun = list(
				background = function(x, y, w, h, j, i) {
					grid.rect(x, y, w, h, gp = gpar(fill = "#CCCCCC", col = NA))
				},
				function(x, y, w, h, j, i) {
					grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = "red", col = NA))
				}
			)
			alter_fun_is_vectorized = TRUE
			names(alter_fun) = c("background", names(mat_list))
			col = "red"
		} else if(length(mat_list) == 2) {
			alter_fun = list(
				background = function(x, y, w, h, j, i) {
					grid.rect(x, y, w, h, gp = gpar(fill = "#CCCCCC", col = NA))
				},
				function(x, y, w, h, j, i) {
					grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = "red", col = NA))
				},
				function(x, y, w, h, j, i) {
					grid.rect(x, y, w*0.9, h*0.4, gp = gpar(fill = "blue", col = NA))
				}
			)
			alter_fun_is_vectorized = TRUE
			names(alter_fun) = c("background", names(mat_list))
			col = c("red", "blue")
		} else {
			stop_wrap("`alter_fun` should be specified.")
		}
		names(col) = names(mat_list)
		warning_wrap("Using default `alter_fun` graphics and reset `col`.")
	}

	if(is.list(alter_fun)) {

		# validate the list first
		if(is.null(alter_fun$background)) alter_fun$background = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "#CCCCCC", col = NA))
		sdf = setdiff(all_type, names(alter_fun))
		if(length(sdf) > 0) {
			stop_wrap(paste0("You should define graphic function for: ", paste(sdf, collapse = ", ")))
		}

		alter_fun = alter_fun[unique(c("background", intersect(names(alter_fun), all_type)))]
		all_type = setdiff(names(alter_fun), "background")
		arr = arr[, , all_type, drop = FALSE]

		if(is.null(alter_fun_is_vectorized)) {
			alter_fun_is_vectorized = guess_alter_fun_is_vectorized(alter_fun)
		}

		if(alter_fun_is_vectorized) {
			layer_fun = function(j, i, x, y, w, h, fill) {
				alter_fun$background(x, y, w, h)
				for(nm in all_type) {
					m = arr[, , nm, drop = FALSE]
					l = pindex(m, i, j)
					if(sum(l)) {
						alter_fun[[nm]](x[l], y[l], w[l], h[l])
					}
				}
			}
			cell_fun = NULL
		} else {
			layer_fun = NULL
			cell_fun = function(j, i, x, y, w, h, fill) {
				alter_fun$background(x, y, w, h)
				for(nm in all_type) {
					if(arr[i, j, nm]) {
						alter_fun[[nm]](x, y, w, h)
					}
				}
			}
		}
	} else if(is.function(alter_fun)) {
		
		if(length(formals(alter_fun)) == 5) {
			af = function(x, y, w, h, v, j, i) alter_fun(x, y, w, h, v)
		} else {
			af = alter_fun
		}

		if(is.null(alter_fun_is_vectorized)) {
			alter_fun_is_vectorized = FALSE
		}

		if(alter_fun_is_vectorized) {
			layer_fun = function(j, i, x, y, w, h, fill) {
				v = pindex(arr, i, j)
				v = as.vector(v)
				names(v) = dimnames[[3]]
				af(x, y, w, h, v, j, i)
			}
			cell_fun = NULL
		} else {
			layer_fun = NULL
			cell_fun = function(j, i, x, y, w, h, fill) {
				v = arr[i, j, ]
				names(v) = dimnames[[3]]
				af(x, y, w, h, v, j, i)
			}
		}
	} else {
		stop_wrap("You need to set `alter_fun`.")
	}

	col = col[intersect(names(col), all_type)]

	count_matrix = apply(arr, c(1, 2), sum)
	n_mut = rowSums(apply(arr, 1:2, any))

	if(is.null(row_order)) {
		row_order = oncoprint_row_order()
	}
	if(is.null(column_order)) {
		column_order = oncoprint_column_order()
	}

	if(is.null(row_order)) row_order = seq_len(nrow(count_matrix))
	if(is.null(column_order)) column_order = seq_len(ncol(count_matrix))
	if(is.character(column_order)) {
		column_order = structure(seq_len(dim(arr)[2]), names = dimnames(arr)[[2]])[column_order]
	}
	names(column_order) = as.character(column_order)

	l_non_empty_column = rowSums(apply(arr, c(2, 3), sum)) > 0
	l_non_empty_row = rowSums(apply(arr, c(1, 3), sum)) > 0

	if(is.null(row_labels)) row_labels = dimnames(arr)[[1]]
	if(remove_empty_columns) {
		arr = arr[, l_non_empty_column, , drop = FALSE]
		column_order = structure(seq_len(sum(l_non_empty_column)), names = which(l_non_empty_column))[as.character(intersect(column_order, which(l_non_empty_column)))]
		if(!is.null(column_labels)) column_labels = column_labels[l_non_empty_column]
		if(!is.null(column_split)) {
			if(is.atomic(column_split)) column_split = data.frame(column_split)
			column_split = column_split[l_non_empty_column, , drop = FALSE]
		}
		column_names_gp = subset_gp(column_names_gp, l_non_empty_column)
	}
	if(is.null(column_labels)) column_labels = dimnames(arr)[[2]]
	if(remove_empty_rows) {
		arr = arr[l_non_empty_row, , , drop = FALSE]
		row_order = structure(seq_len(sum(l_non_empty_row)), names = which(l_non_empty_row))[as.character(intersect(row_order, which(l_non_empty_row)))]
		if(!is.null(row_labels)) row_labels = row_labels[l_non_empty_row]
		if(!is.null(row_split)) {
			if(is.atomic(row_split)) row_split = data.frame(row_split)
			row_split = row_split[l_non_empty_row, , drop = FALSE]
		}
		row_names_gp = subset_gp(row_names_gp, l_non_empty_row)
	}

	# validate col
	sdf = setdiff(all_type, names(col))
	if(length(sdf) > 0) {
		message_wrap(paste0("Colors are not defined for: ", paste(sdf, collapse = ", "), ". They won't be shown in the barplots."))
	}

	# for each gene, percent of samples that have alterations
	if(is.null(pct_include)) {
		pct_include = dimnames(arr)[[3]]
	}
	pct_num = rowSums(apply(arr[, , dimnames(arr)[[3]] %in% pct_include, drop = FALSE], 1:2, any)) / ncol(mat_list[[1]])
	pct = paste0(round(pct_num * 100, digits = pct_digits), "%")

	### now the annotations
	if(length(col) == 0) {
		if(missing(top_annotation)) top_annotation = NULL
		if(missing(right_annotation)) right_annotation = NULL
	}
	top_annotation = top_annotation
	right_annotation = right_annotation

	if(show_pct && show_row_names) {
		if(pct_side == row_names_side) {
			stop_wrap("Percent values and row names should be at different side of the oncoPrint.")
		}
	}

	if(show_pct) {
		if(pct_side == "left") {
			pct_ha = rowAnnotation(pct = anno_text(pct, just = "right", location = unit(1, "npc"), gp = pct_gp, width = max_text_width(pct, gp = pct_gp) + unit(1, "mm")),
					show_annotation_name = FALSE)
		} else {
			pct_ha = rowAnnotation(pct = anno_text(pct, just = "left", location = unit(0, "npc"), gp = pct_gp, width = max_text_width(pct, gp = pct_gp) + unit(1, "mm")),
					show_annotation_name = FALSE)
		}
		names(pct_ha) = paste0("pct_", random_str())
	} else {
		pct_ha = NULL
	}
	if(show_row_names) {
		if(row_names_side == "right") {
			rn_ha = rowAnnotation(rownames = anno_text(row_labels, gp = row_names_gp, just = "left", location = unit(0, "npc"), width = max_text_width(row_labels, gp = row_names_gp) + unit(1, "mm")),
				show_annotation_name = FALSE)
		} else {
			rn_ha = rowAnnotation(rownames = anno_text(row_labels, gp = row_names_gp, just = "right", location = unit(1, "npc"), width = max_text_width(row_labels, gp = row_names_gp) + unit(1, "mm")),
				show_annotation_name = FALSE)
		}
		names(rn_ha) = paste0("rownames_", random_str())
	} else {
		rn_ha = NULL
	}

	
	if(!is.null(top_annotation)) {
		if(inherits(top_annotation, "AnnotationFunction")) {
	        stop_wrap("The annotation function `anno_*()` should be put inside `HeatmapAnnotation()`.")
	    }
	}
	if(!is.null(bottom_annotation)) {
	    if(inherits(bottom_annotation, "AnnotationFunction")) {
	        stop_wrap("The annotation function `anno_*()` should be put inside `HeatmapAnnotation()`.")
	    }
	}
	if(!is.null(left_annotation)) {
		if(inherits(left_annotation, "AnnotationFunction")) {
	        stop_wrap("The annotation function `anno_*()` should be put inside `rowAnnotation()`.")
	    }
	}
	if(!is.null(right_annotation)) {
	    if(inherits(right_annotation, "AnnotationFunction")) {
	        stop_wrap("The annotation function `anno_*()` should be put inside `rowAnnotation()`.")
	    }
	}
	
	if(is.null(left_annotation)) {
		if(pct_side == "left") {
			left_annotation = pct_ha
		}
		if(row_names_side == "left") {
			left_annotation = rn_ha
		}
	} else {
		if(remove_empty_rows) {
			left_annotation = left_annotation[l_non_empty_row, ]
		}
		if(pct_side == "left") {
			left_annotation = c(left_annotation, pct_ha, gap = unit(1, "mm"))
		}
		if(row_names_side == "left") {
			left_annotation = c(left_annotation, rn_ha, gap = unit(1, "mm"))
		}
	}

	if(is.null(right_annotation)) {
		if(pct_side == "right") {
			right_annotation = pct_ha
		}
		if(row_names_side == "right") {
			right_annotation = rn_ha
		}
	} else {
		if(remove_empty_rows) {
			right_annotation = right_annotation[l_non_empty_row, ]
		}
		if(pct_side == "right") {
			if(!is.null(pct_ha)) right_annotation = c(pct_ha, right_annotation, gap = unit(1, "mm"))
		}
		if(row_names_side == "right") {
			if(!is.null(rn_ha)) right_annotation = c(rn_ha, right_annotation, gap = unit(1, "mm"))
		}
	}
	if(remove_empty_columns) {
		if(!is.null(top_annotation)) {
			top_annotation = top_annotation[l_non_empty_column, ]
		}
		if(!is.null(bottom_annotation)) {
			bottom_annotation = bottom_annotation[l_non_empty_column, ]
		}
	}
	
	#####################################################################
	# the main matrix
	if(length(col) == 0) {
		col = c("mutation" = "black")
	}

	pheudo = apply(arr, 1:2, function(x) {
		if(all(!x)) {
			return("")
		} else {
			paste(all_type[x], collapse = ";")
		}
	})
	dim(pheudo) = dim(arr)[1:2]
	dimnames(pheudo) = dimnames(arr)[1:2]
	
	if(length(arg_list)) {
		if(any(arg_names %in% c("rect_gp", "cell_fun"))) {
			stop_wrap("'rect_gp', 'cell_fun' are not allowed to use in `oncoPrint()`.")
		}

		if("cluster_rows" %in% arg_names) {
			if(!inherits(cluster_rows, c("dendrogram", "hclust"))) {
				stop_wrap("`cluster_rows` can only be a dendrogram/hclust object if it is set.")
			}
		}
		if("cluster_columns" %in% arg_names) {
			if(!inherits(cluster_columns, c("dendrogram", "hclust"))) {
				stop_wrap("`cluster_columns` can only be a dendrogram/hclust object if it is set.")
			}
		}
	}

	if(is.list(alter_fun)) {
		if(is.null(alter_fun$background)) {
			background_fun = function(x, y, w, h) NULL
		} else {
			background_fun = alter_fun$background
		}

		alter_fun2 = alter_fun[names(alter_fun) != "background"]
		alter_fun3 = alter_fun2
		for(i in seq_along(alter_fun2)) {
			alter_fun3[[i]] = local({
				i = i
				function(x, y, w, h) {
					background_fun(x, y, w, h)
					alter_fun2[[i]](x, y, w, h)
				}
			})
		}
	} else {
		all_type_binary = structure(rep(FALSE, length(all_type)), names = all_type)
		background_fun = function(x, y, w, h) {
			alter_fun(x, y, w, h, all_type_binary)
		}
		alter_fun3 = list()
		for(nm in all_type) {
			alter_fun3[[nm]] = local({
				all_type_binary2 = all_type_binary
				all_type_binary2[nm] = TRUE
				function(x, y, w, h) {
					alter_fun(x, y, w, h, all_type_binary2)
				}
			})
		}
	}

	if(is.null(heatmap_legend_param)) {
		heatmap_legend_param = list(
			title = "Alterations",
			at = names(alter_fun3),
			graphics = alter_fun3
		)
		col2 = structure(rep(NA, length(alter_fun3)), names = names(alter_fun3))
		col2[names(col)] = col
		col = col2
	} else {
		if(! "graphics" %in% names(heatmap_legend_param)) {
			if(is.null(heatmap_legend_param$at)) heatmap_legend_param$at = names(alter_fun3)
			if(is.null(heatmap_legend_param$labels)) heatmap_legend_param$labels = heatmap_legend_param$at

			# adjust order of alter_fun3 with at
			if(!is.null(heatmap_legend_param$at)) {
				ind = which(heatmap_legend_param$at %in% names(alter_fun3))
				heatmap_legend_param$at = heatmap_legend_param$at[ind]
				heatmap_legend_param$labels = heatmap_legend_param$labels[ind]

				alter_fun3 = alter_fun3[heatmap_legend_param$at]
			}

			heatmap_legend_param$graphics = alter_fun3
			col2 = structure(rep(NA, length(alter_fun3)), names = names(alter_fun3))
			col2[names(col)] = col
			col = col2
		}
	}

	ht = Heatmap(pheudo, name = name, col = col, 
		rect_gp = gpar(type = "none"), 
		cluster_rows = cluster_rows, cluster_columns = cluster_columns, 
		row_order = row_order, column_order = column_order,
		row_split = row_split, 
		column_labels = column_labels,
		column_names_gp = column_names_gp,
		column_split = column_split,
		cell_fun = cell_fun, layer_fun = layer_fun,
		top_annotation = top_annotation,
		bottom_annotation = bottom_annotation,
		left_annotation = left_annotation,
		right_annotation = right_annotation,
		show_row_names = FALSE,
		show_column_names = show_column_names,
		heatmap_legend_param = heatmap_legend_param,
		...
	)
	ht@heatmap_param$oncoprint_env = environment()
	ht@heatmap_param$type = "oncoPrint"

	return(ht)
}

# == title
# Automatically generate alter_fun
#
# == param
# -graphic Graphic to draw.
# -width Relative width of the rectangle.
# -height Relative height of the rectangle.
# -horiz_margin Horizontal margin. E.g. if you want 1mm margin on top and 1mm margin
#        at bottom of the rectangle, set this value to ``unit(1, 'mm')``.
# -vertical_margin Vertical margin.
# -fill Filled color.
# -col Border color.
# -pch Pch for points
# -... Pass to `grid::gpar`
#
# == details
# This function aims to simplify the definition of functions in ``alter_fun``. Now it only
# supports rectangles and points.
#
# == example
# mat = read.table(textConnection(
# "s1,s2,s3
# g1,snv;indel,snv,indel
# g2,,snv;indel,snv
# g3,snv,,indel;snv"), row.names = 1, header = TRUE, sep = ",", stringsAsFactors = FALSE)
# mat = as.matrix(mat)
# col = c(snv = "red", indel = "blue")
#
# oncoPrint(mat, 
# 	alter_fun = list(
# 		snv = alter_graphic("rect", width = 0.9, height = 0.9, fill = col["snv"]),
# 		indel = alter_graphic("rect", width = 0.9, height = 0.9, fill = col["indel"])
# 	), col = col)
#
alter_graphic = function(graphic = c("rect", "point"),
	width = 1, height = 1, 
	horiz_margin = unit(1, "pt"), vertical_margin = unit(1, "pt"),
	fill = "red", col = NA, pch = 16, ...) {

	graphic = match.arg(graphic)[1]

	if(graphic == "rect") {
		if(!is.numeric(width)) {
			stop_wrap("`width` should be nummeric.")
		}
		if(!is.numeric(height)) {
			stop_wrap("`height` should be nummeric.")
		}
		if(width != 1) {
			if(missing(horiz_margin)) {
				horiz_margin = unit(0, "pt")
			}
		}
		if(height != 1) {
			if(missing(vertical_margin)) {
				vertical_margin = unit(0, "pt")
			}
		}
		fun = function(x, y, w, h) {
			w = w*width
			h = h*height
			grid.rect(x, y, w - horiz_margin*2, h - vertical_margin*2,
				gp = gpar(fill = fill, col = col, ...))
		}
	} else if(graphic == "point") {
		fun = function(x, y, w, h) {
			grid.points(x, y, pch = pch, gp = gpar(fill = fill, col = col, ...))
		}
	}
	return(fun)
}

ONCOPRINT_ENV = new.env()
ONCOPRINT_ENV$fun_env = NULL

# == title
# Unify a List of Matrix 
#
# == param
# -mat_list A list of matrix. All of them should have dimension names.
# -default Default values for the newly added rows and columns.
#
# == details
# All matrix will be unified to have same row names and column names.
#
# == value
# A list of matrix
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
unify_mat_list = function(mat_list, default = 0) {
	common_rn = unique(unlist(lapply(mat_list, rownames)))
	common_cn = unique(unlist(lapply(mat_list, colnames)))

	mat_list2 = lapply(seq_along(mat_list), function(i) {
		mat = matrix(default, nrow = length(common_rn), ncol = length(common_cn))
		dimnames(mat) = list(common_rn, common_cn)
		mat[rownames(mat_list[[i]]), colnames(mat_list[[i]])] = mat_list[[i]]
		mat
	})
	names(mat_list2) = names(mat_list)
	return(mat_list2)
}



# == title
# Barplot Annotation for oncoPrint
#
# == param
# -type A vector of the alteration types in the data. It can be a subset of all alteration types if you don't want to show them all.
# -which Is it a row annotation or a column annotation?
# -bar_width Width of the bars.
# -beside Will bars be stacked or be positioned beside each other?
# -ylim Data range.
# -show_fraction Whether to show the numbers or the fractions?
# -axis Whether draw axis?
# -axis_param Parameters for controlling axis.
# -width Width of the annotation.
# -height Height of the annotation.
# -border Whether draw the border?
#
# == detail
# This annotation function should always be used with `oncoPrint`.
# 
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_oncoprint_barplot = function(type = NULL, which = c("column", "row"),
	bar_width = 0.6, beside = FALSE, ylim = NULL, show_fraction = FALSE, axis = TRUE,
	axis_param = if(which == "column") default_axis_param("column") else list(side = "top", labels_rot = 0),
	width = NULL, height = NULL, border = FALSE) {

	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	anno_size = anno_width_and_height(which, width, height, unit(2, "cm"))

	column_fun = function(index, k, n) {
		pf = get("object", envir = parent.frame(7))@heatmap_param$oncoprint_env
		arr = pf$arr
		all_type = pf$all_type
		col = pf$col

		if(is.null(type)) type = names(col)

		all_type = intersect(all_type, type)
		if(length(all_type) == 0) {
			stop_wrap("find no overlap, check your `type` argument.")
		}
		arr = arr[, , all_type, drop = FALSE]
		col = col[all_type]

		if(show_fraction) {
			v = apply(arr, c(2, 3), sum)/dim(arr)[1]
		} else {
			v = apply(arr, c(2, 3), sum)
		}
		v = v[, !is.na(col), drop = FALSE]
		col = col[!is.na(col)]
		fun = anno_barplot(v, gp = gpar(fill = col, col = NA), which = "column", ylim = ylim,
			baseline = 0, height = anno_size$height, border = border, bar_width = bar_width, beside = beside,
			axis = axis, axis_param = axis_param)@fun
		fun(index, k, n)
	}
	row_fun = function(index, k, n) {
		pf = get("object", envir = parent.frame(7))@heatmap_param$oncoprint_env
		arr = pf$arr
		all_type = pf$all_type
		col = pf$col

		if(is.null(type)) type = names(col)

		all_type = intersect(all_type, type)
		if(length(all_type) == 0) {
			stop_wrap("find no overlap, check your `type` argument.")
		}
		arr = arr[, , all_type, drop = FALSE]
		col = col[all_type]

		if(show_fraction) {
			v = apply(arr, c(1, 3), sum)/dim(arr)[2]
		} else {
			v = apply(arr, c(1, 3), sum)
		}
		v = v[, !is.na(col), drop = FALSE]
		col = col[!is.na(col)]
		fun = anno_barplot(v, gp = gpar(fill = col, col = NA), which = "row", ylim = ylim,
			baseline = 0, width = anno_size$width, border = border, bar_width = bar_width, beside = beside,
			axis = axis, axis_param = axis_param)@fun
		fun(index, k, n)
	}

	if(which == "row") {
		fun = row_fun
	} else if(which == "column") {
		fun = column_fun
	}

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_oncoprint_barplot",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		var_import = list(border, type, bar_width, beside, axis, axis_param, anno_size, ylim, show_fraction)
	)
		
	anno@subsettable = TRUE
	anno@show_name = FALSE

	if(exists("arr", envir = parent.frame(1))) {
		arr = get("arr", envir = parent.frame(1))
		if(which == "row") {
			data_scale = c(0, max(apply(arr, 1, sum)))
		} else {
			data_scale = c(0, max(apply(arr, 2, sum)))
		}
	} else {
		data_scale = c(0, 100)
	}

	axis_param = validate_axis_param(axis_param, which)
	axis_grob = if(axis) construct_axis_grob(axis_param, which, data_scale) else NULL
	anno@extended = update_anno_extend(anno, axis_grob, axis_param)

	return(anno) 
}

guess_alter_fun_is_vectorized = function(alter_fun) {
	n = 50
	if(is.list(alter_fun)) {

		# check whether grid.polygon is called
		if(any(sapply(alter_fun, function(f) any(grepl("grid\\.polygon\\(", as.character(body(f))))))) {
			return(FALSE)
		}

		x = unit(1:n/n, "npc")
		y = unit(1:n/n, "npc")
		w = unit(1:n, "mm")
		h = unit(1:n, "mm")
		dev.null()
		oe = try({
			for(i in seq_along(alter_fun)) {
				alter_fun[[i]](x, y, w, h)
			}
		}, silent = TRUE)
		dev.off2()
		if(inherits(oe, "try-error")) {
			return(FALSE)
		} else {
			message_wrap("`alter_fun` is assumed vectorizable. If it does not generate correct plot, please set `alter_fun_is_vectorized = FALSE` in `oncoPrint()`.")
			return(TRUE)
		}
	} else {
		return(FALSE)
	}
}

# == title
# Default get_type for oncoPrint()
#
# == param
# -x A strings which encode multiple altertations.
#
# == details
# It recognizes following separators: ``;:,|``.
#
default_get_type = function(x) {
	x = strsplit(x, "\\s*[;:,|]\\s*")[[1]]
	# x[!x %in% c("na", "NA")]
	x
}

# == title
# Test alter_fun for oncoPrint()
#
# == param
# -fun The ``alter_fun`` for `oncoPrint`. The value can be a list of functions or a single function. See https://jokergoo.github.io/ComplexHeatmap-reference/book/oncoprint.html#define-the-alter-fun
# -type A vector of alteration types. It is only used when ``fun`` is a single function.
# -asp_ratio The aspect ratio (width/height) for the small rectangles.
#
# == details
# This function helps you to have a quick view of how the graphics for each alteration type
# and combinations look like.
#
# == example
# alter_fun = list(
# 	mut1 = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "red", col = NA)),
# 	mut2 = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "blue", col = NA)),
# 	mut3 = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "yellow", col = NA)),
# 	mut4 = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "purple", col = NA)),
# 	mut5 = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(lwd = 2)),
# 	mut6 = function(x, y, w, h) grid.points(x, y, pch = 16),
# 	mut7 = function(x, y, w, h) grid.segments(x - w*0.5, y - h*0.5, x + w*0.5, y + h*0.5, gp = gpar(lwd = 2))
# )
# test_alter_fun(alter_fun)
test_alter_fun = function(fun, type, asp_ratio = 1) {
	background_fun = NULL
	if(inherits(fun, "list")) {
		fun_type = "list"
		type = names(fun)

		if("background" %in% type) {
			background_fun = fun$background
		}
		type = setdiff(type, "background")

		if(length(type) == 0) {
			stop_wrap("'type' should be of the names of the function list defined in `fun`.")
		}

		cat("`alter_fun` is defined as a list of functions.\n")
		cat("Functions are defined for following alteration types:\n")
		cat(paste(strwrap(paste(names(fun), collapse = ", "), initial = "  "), collapse = "\n"), "\n")
		if(!is.null(background_fun)) {
			cat("Background is also defined.\n")
		}
	} else{
		fun_type = "function"
		if(length(as.list(formals(fun))) != 5) {
			stop_wrap("If `alter_fun` is defined as a single function, it needs to have five arguments. Check example at https://jokergoo.github.io/ComplexHeatmap-reference/book/oncoprint.html#define-the-alter-fun.")
		}

		if(missing(type)) {
			stop_wrap("You need to provide a vector of alteration types for `type` argument to test.")
		}

		type = setdiff(type, "background")
	}
	
	tl = lapply(type, function(x) x)
	names(tl) = type
	if(length(type) >= 2) {
		tl2 = as.list(as.data.frame(combn(type, 2), stringsAsFactors = FALSE))
	} else {
		tl2 = NULL
	}
	if(length(type) >= 3) {
		tl2 = c(tl2, as.list(as.data.frame(combn(type, 3), stringsAsFactors = FALSE)))
	}

	if(!is.null(tl2)) {
		tl2 = tl2[sample(length(tl2), min(length(tl), length(tl2)), prob = sapply(tl2, length))]
		tl2 = tl2[order(sapply(tl2, length))]
		names(tl2) = sapply(tl2, paste, collapse = "+")
	}

	# draw the examples
	grid_width = asp_ratio*max_text_height("A")*2
	grid_height = max_text_height("A")*2 + unit(2, "mm")
	text_width_1 = max_text_width(names(tl))
	w = text_width_1 + unit(1, "mm") + grid_width
	if(!is.null(tl2)) {
		text_width_2 = max_text_width(names(tl2))
		w = w + unit(5, "mm") + text_width_2 + unit(1, "mm") + grid_width
	}
	n = length(tl)
	h = grid_height*n

	grid.newpage()
	pushViewport(viewport(width = w, height = h))
	for(i in 1:n) {
		grid.text(names(tl)[i], text_width_1, (n - i + 0.5)/n, just = "right")
		if(is.null(background_fun)) {
			grid.rect(text_width_1 + unit(1, "mm") + grid_width*0.5, unit((n - i + 0.5)/n, "npc"), grid_width, grid_height - unit(2, "mm"), gp = gpar(fill = "#CCCCCC", col = NA))
		} else {
			background_fun(text_width_1 + unit(1, "mm") + grid_width*0.5, unit((n - i + 0.5)/n, "npc"), grid_width, grid_height - unit(2, "mm"))
		}
		if(fun_type == "list") {
			fun[[ tl[[i]] ]](text_width_1 + unit(1, "mm") + grid_width*0.5, unit((n - i + 0.5)/n, "npc"), grid_width, grid_height - unit(2, "mm"))
		} else {
			fun(text_width_1 + unit(1, "mm") + grid_width*0.5, unit((n - i + 0.5)/n, "npc"), grid_width, grid_height - unit(2, "mm"), tl[[i]])
		}
	}
	if(!is.null(tl2)) {
		n2 = length(tl2)
		for(i in 1:n2) {
			grid.text(names(tl2)[i], text_width_1 + unit(1, "mm") + grid_width + unit(5, "mm") + text_width_2, (n - i + 0.5)/n, just = "right")
			if(is.null(background_fun)) {
				grid.rect(text_width_1 + unit(2, "mm") + unit(5, "mm") + grid_width + text_width_2 + grid_width*0.5, unit((n - i + 0.5)/n, "npc"), grid_width, grid_height - unit(2, "mm"), gp = gpar(fill = "#CCCCCC", col = NA))
			} else {
				background_fun(text_width_1 + unit(2, "mm") + unit(5, "mm") + grid_width + text_width_2 + grid_width*0.5, unit((n - i + 0.5)/n, "npc"), grid_width, grid_height - unit(2, "mm"))
			}
			if(fun_type == "list") {
				for(j in tl2[[i]]) {
					fun[[ j ]](text_width_1 + unit(2, "mm") + unit(5, "mm") + grid_width + text_width_2 + grid_width*0.5, unit((n - i + 0.5)/n, "npc"), grid_width, grid_height - unit(2, "mm"))
				}
			} else {
				fun(text_width_1 + unit(2, "mm") + grid_width + unit(5, "mm") + text_width_2 + grid_width*0.5, unit((n - i + 0.5)/n, "npc"), grid_width, grid_height - unit(2, "mm"), tl2[[i]])
			}
		}
	}
	popViewport()
	
}
