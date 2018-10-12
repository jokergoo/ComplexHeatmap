
# == title
# Make oncoPrint
#
# == param
# -mat a character matrix which encodes mulitple alterations or a list of matrix for which every matrix contains binary
#      value representing the alteration is present or absent. When it is a list, the names of the list represent alteration types.
#      You can use `unify_mat_list` to make all matrix having same row names and column names.
# -get_type If different alterations are encoded in the matrix, this self-defined function
#           determines how to extract them. Only work when ``mat`` is a matrix.
# -alter_fun a single function or a list of functions which define how to add graphics for different alterations.
#                 If it is a list, the names of the list should cover all alteration types.
# -alter_fun_is_vectorized Whether ``alter_fun`` is implemented vectorized. Internally the function will guess.
# -col a vector of color for which names correspond to alteration types.
# -top_annotation Annotation put on top of the oncoPrint. By default it is barplot which shows the number of genes having the alteration in each sample.
# -right_annotation Annotation put on the right of hte oncoPrint. By default it is barplto which shows the number of samples having the alteration in each gene.
# -show_pct whether show percent values on the left of the oncoprint
# -pct_gp graphic paramters for percent row annotation
# -pct_digits digits for percent values
# -pct_side side of pct
# -show_row_names Whether show row names?
# -row_names_side side of the row names
# -row_names_gp Graphic parameters of row names.
# -row_order row order
# -column_order column order
# -remove_empty_columns if there is no alteration in that sample, whether remove it on the heatmap
# -remove_empty_rows if there is no alteration in that sample, whether remove it on the heatmap
# -show_column_names Whether show column names?
# -heatmap_legend_param pass to `Heatmap`
# -... pass to `Heatmap`, so can set ``bottom_annotation`` here.
#
# == details
# The 'memo sort' method is from https://gist.github.com/armish/564a65ab874a770e2c26 . Thanks to
# B. Arman Aksoy for contributing the code.
#
# For more explanation, please go to the vignette.
#
# == value
# A `Heatmap-class` object which means you can add other heatmaps or annotations to it.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
oncoPrint = function(mat, 
	get_type = function(x) x,
	alter_fun, 
	alter_fun_is_vectorized = NULL,
	col, 

	top_annotation = HeatmapAnnotation(column_barplot = anno_oncoprint_barplot(),
		show_annotation_name = FALSE),
	right_annotation = rowAnnotation(row_barplot = anno_oncoprint_barplot(
			axis_param = list(side = "top", labels_rot = 0)),
		show_annotation_name = FALSE),

	show_pct = TRUE, 
	pct_gp = gpar(fontsize = 10), 
	pct_digits = 0,
	pct_side = "left",
	show_row_names = TRUE,
	row_names_side = "right",
	row_names_gp = pct_gp,

	row_order = NULL,
	column_order = NULL,
	
	remove_empty_columns = FALSE,
	remove_empty_rows = FALSE,
	show_column_names = FALSE,
	heatmap_legend_param = list(title = "Alterations"),
	...) {

	.in_oncoprint = TRUE

	arg_list = list(...)
	arg_names = names(arg_list)

	# convert mat to mat_list
	if(inherits(mat, "data.frame")) {
		mat = as.matrix(mat)
	}
	if(inherits(mat, "matrix")) {
		all_type = unique(unlist(lapply(mat, get_type)))
		all_type = all_type[!is.na(all_type)]
		all_type = all_type[grepl("\\S", all_type)]

		mat_list = lapply(all_type, function(type) {
			m = sapply(mat, function(x) type %in% get_type(x))
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
					stop("Expect a list of matrix (not data frames).")
				}
				oattr = attributes(x)
				x = as.logical(x)
				attributes(x) = oattr
				x
			})

		if(length(unique(sapply(mat_list, nrow))) > 1) {
			stop("All matrix in 'mat_list' should have same number of rows.")
		}

		if(length(unique(sapply(mat_list, ncol))) > 1) {
			stop("All matrix in 'mat_list' should have same number of columns.")
		}
	} else {
		stop("Incorrect type of 'mat'")
	}

	cat("All mutation types:", paste(all_type, collapse = ", "), "\n")


	# type as the third dimension
	arr = array(FALSE, dim = c(dim(mat_list[[1]]), length(all_type)), dimnames = c(dimnames(mat_list[[1]]), list(all_type)))
	for(i in seq_along(all_type)) {
		arr[, , i] = mat_list[[i]]
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
			af = list(
				background = function(x, y, w, h, j, i) {
					grid.rect(x, y, w, h, gp = gpar(fill = "#CCCCCC", col = NA))
				},
				function(x, y, w, h, j, i) {
					grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = "red", col = NA))
				}
			)
			alter_fun_is_vectorized = TRUE
			names(af) = c("background", names(mat_list))
			col = "red"
		} else if(length(mat_list) == 2) {
			af = list(
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
			names(af) = c("background", names(mat_list))
			col = c("red", "blue")
		} else {
			stop("`alter_fun` should be specified.")
		}
		names(col) = names(mat_list)
		warning("Using default `alter_fun` graphics and reset `col`.")
	}

	if(is.list(alter_fun)) {

		# validate the list first
		if(is.null(alter_fun$background)) alter_fun$background = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "#CCCCCC", col = NA))
		sdf = setdiff(all_type, names(alter_fun))
		if(length(sdf) > 0) {
			stop(paste0("You should define graphic function for: ", paste(sdf, collapse = ", ")))
		}

		alter_fun = alter_fun[unique(c("background", intersect(names(alter_fun), all_type)))]

		if(is.null(alter_fun_is_vectorized)) {
			alter_fun_is_vectorized = guess_alter_fun_is_vectorized(alter_fun)
		}

		if(alter_fun_is_vectorized) {
			layer_fun = function(j, i, x, y, w, h, fill) {
				alter_fun$background(x, y, w, h)
				for(nm in all_type) {
					m = arr[, , nm]
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
				af(x, y, w, h, v, j, i)
			}
			cell_fun = NULL
		} else {
			layer_fun = NULL
			cell_fun = function(j, i, x, y, w, h, fill) {
				v = arr[i, j, ]
				af(x, y, w, h, v, j, i)
			}
		}
	} else {
		stop("You need to set `alter_fun`.")
	}

	col = col[intersect(names(col), all_type)]


	count_matrix = apply(arr, c(1, 2), sum)
	n_mut = rowSums(apply(arr, 1:2, any))

	if(!"row_order" %in% arg_names) {
		row_order = oncoprint_row_order()
	}
	if(!"column_order" %in% arg_names) {
		column_order = oncoprint_column_order()
	}
	
	if(is.null(row_order)) row_order = seq_len(nrow(count_matrix))
	if(is.null(column_order)) column_order = seq_len(ncol(count_matrix))
	if(is.character(column_order)) {
		column_order = structure(seq_len(dim(arr)[2]), names = dimnames(arr)[[2]])[column_order]
	}
	names(column_order) = as.character(column_order)
	if(remove_empty_columns) {
		l = rowSums(apply(arr, c(2, 3), sum)) > 0
		arr = arr[, l, , drop = FALSE]
		column_order = structure(seq_len(sum(l)), names = which(l))[as.character(intersect(column_order, which(l)))]
	}
	if(remove_empty_rows) {
		l = rowSums(apply(arr, c(1, 3), sum)) > 0
		arr = arr[l, , , drop = FALSE]
		row_order = structure(seq_len(sum(l)), names = which(l))[as.character(intersect(row_order, which(l)))]
	}

	# validate col
	sdf = setdiff(all_type, names(col))
	if(length(sdf) > 0) {
		stop(paste0("You should define colors for:", paste(sdf, collapse = ", ")))
	}

	# for each gene, percent of samples that have alterations
	pct_num = rowSums(apply(arr, 1:2, any)) / ncol(mat_list[[1]])
	pct = paste0(round(pct_num * 100, digits = pct_digits), "%")

	### now the annotations
	if("left_annotation" %in% arg_names) {
		stop("'left_annotation' are not allowed to specify, you can add...")
	}
	left_annotation = NULL
	if(show_pct) {
		left_annotation = rowAnnotation(pct = anno_text(pct, just = "right", location = unit(1, "npc"), gp = pct_gp),
			show_annotation_name = FALSE)
	}
	if(show_row_names) {
		ha_row_names = rowAnnotation(rownames = anno_text(dimnames(arr)[[1]], gp = pct_gp, just = "left", location = unit(0, "npc")),
			show_annotation_name = FALSE)
		right_annotation = c(ha_row_names, right_annotation)
	}

	#####################################################################
	# the main matrix
	pheudo = c(all_type, rep(NA, nrow(arr)*ncol(arr) - length(all_type)))
	dim(pheudo) = dim(arr)[1:2]
	dimnames(pheudo) = dimnames(arr)[1:2]
	
	if(length(arg_list)) {
		if(any(arg_names %in% c("rect_gp", "cluster_rows", "cluster_columns", "cell_fun"))) {
			stop("'rect_gp', 'cluster_rows', 'cluster_columns', 'cell_fun' are not allowed to use in `oncoPrint()`.")
		}
	}

	ht = Heatmap(pheudo, col = col, 
		rect_gp = gpar(type = "none"), 
		cluster_rows = FALSE, cluster_columns = FALSE, 
		row_order = row_order, column_order = column_order,
		cell_fun = cell_fun, layer_fun = layer_fun,
		top_annotation = top_annotation,
		left_annotation = left_annotation,
		right_annotation = right_annotation,
		show_row_names = FALSE,
		show_column_names = show_column_names,
		heatmap_legend_param = heatmap_legend_param,
		...
	)

	return(ht)
}

# == title
# Unify a list of matrix 
#
# == param
# -mat_list a list of matrix, all of them should have dimension names
# -default default values for the newly added rows and columns
#
# == details
# All matrix will be unified to have same row names and column names
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
# Barplot annotation for oncoPrint
#
# == param
# -type A vector of the alteration types in your data. It can be a subset of all alteration types if you don't want to show them all.
# -which Is ti a row annotation or a column annotation?
# -width Wisth of the annotation.
# -height Height of the annotation.
# -border Whether draw the border?
# -... Other parameters passed to `anno_barplot`.
#
# == detail
# This annotation function should always use with `oncoPrint`.
# 
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_oncoprint_barplot = function(type = all_type, which = c("column", "row"),
	width = NULL, height = NULL, border = FALSE, ...) {

	if(is.null(.ENV$current_annotation_which)) {
		which = match.arg(which)[1]
	} else {
		which = .ENV$current_annotation_which
	}

	anno_size = anno_width_and_height(which, width, height, unit(2, "cm"))

	# get variables fron oncoPrint() function
	pf = parent.frame()
	if(!exists(".in_oncoprint", envir = pf, inherits = FALSE)) {
		stop_wrap("`anno_oncoprint_barplot()` should only be used with `oncoPrint()`.")
	}
	arr = get("arr", envir = pf, inherits = FALSE)
	all_type = get("all_type", envir = pf, inherits = FALSE)
	col = get("col", envir = pf, inherits = FALSE)

	type = type
	all_type = intersect(all_type, type)
	arr = arr[, , all_type, drop = FALSE]
	col = col[all_type]

	if(which == "column") {
		count = apply(arr, c(2, 3), sum)
		anno_barplot(count, gp = gpar(fill = col, col = NA), which = "column",
			baseline = 0, height = anno_size$height, border = border, ...)
	} else {
		count = apply(arr, c(1, 3), sum)
		anno_barplot(count, gp = gpar(fill = col, col = NA), which = "row",
			baseline = 0, width = anno_size$width, border = border, ...)
	}
}

guess_alter_fun_is_vectorized = function(alter_fun) {
	n = 50
	if(is.list(alter_fun)) {
		x = 1:n
		y = 1:n
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
			return(TRUE)
		}
	} else {
		return(FALSE)
	}
}

