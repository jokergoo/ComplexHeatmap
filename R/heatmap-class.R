
###############################
# class for single heatmap
#
# this class would be a private class. The plotting of heatmap
# is really taken care by HeatmapList class.
#
# the draw method only plots the heatmap body, cluster, annotation and dimnames
#

Heatmap = setRefClass("Heatmap",
    fields = list(
    	name = "character",
    	matrix = "matrix",  # original order
    	row_hclust = "ANY",
    	column_hclust = "ANY",
    	column_anno = "ANY", # annotation data frame, order of columns are same as matrix
    	column_anno_color_mapping = "list", # a list of ColorMapping class objects
    	matrix_color_mapping = "ANY",

    	# settings for positin of each component
    	title = "character",
    	
    	layout_title_top_height = "ANY",
    	layout_column_hclust_top_height = "ANY",
    	layout_column_anno_top_height = "ANY",
    	layout_colnames_top_height = "ANY",
    	layout_title_bottom_height = "ANY",
    	layout_column_hclust_bottom_height = "ANY",
    	layout_column_anno_bottom_height = "ANY",
    	layout_colnames_bottom_height = "ANY",

    	layout_row_hclust_left_width = "ANY",
    	layout_rownames_left_width = "ANY",
    	layout_row_hclust_right_width = "ANY",
    	layout_rownames_right_width = "ANY",

    	layout_index = "matrix",
    	graphic_fun_list = "list"
    	
    )
)

# matrix can be a numeric matrix or a character matrix
Heatmap$methods(initialize = function(matrix, col, name,
	cluster_rows = TRUE, clustering_distance_rows = "euclidean", clustering_method_rows = "complete",
	cluster_columns = TRUE, clustering_distance_columns = "euclidean", clustering_method_columns = "complete",
	title = name, title_side = c("top", "bottom"), draw_title = TRUE, title_gp = gpar(fontsize = 14),
	row_hclust_side = c("left", "right"), row_hclust_width = unit(5, "mm"), show_row_hclust = TRUE,
	column_hclust_side = c("top", "bottom"), column_hclust_height = unit(5, "mm"), show_column_hclust = TRUE,
	rownames_side = c("left", "right"), show_rownames = TRUE, rownames_gp = gpar(fontsize = 12), 
	colnames_side = c("bottom", "top"), show_colnames = TRUE, colnames_gp = gpar(fontsize = 12),
	annotation = NULL, annotation_color = NULL, annotation_side = c("top", "bottom"),
	annotation_height = if(is.null(annotation)) unit(0, "null") else ncol(annotation)*unit(4, "mm")
	) {

	if(is.function(col)) {
		matrix_color_mapping <<- ColorMapping(col_fun = col, name = name)
	} else {
		matrix_color_mapping <<- ColorMapping(colors = col, name = name)
	}
	name <<- name

	get_dist = function(matrix, method) {
		if(method %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) {
			dst = dist(matrix, method = method)
		} else if(method %in% c("pearson", "spearman", "tau")) {
			dst = switch(method,
				         pearson = as.dist(1 - cor(matrix, method = "pearman")),
				         spearman = as.dist(1 - cor(matrix, method = "spearman")),
				         kendall = as.dist(1 - cor(matrix, method = "kendall")))
		} else if(is.function(method)) {
			nargs = length(as.list(args(method)))
			if(nargs == 2) { # a distance function
				dst = method(matrix)
			} else if(nargs == 3) {
				dst = dist2(matrix, method)
			} else {
				stop("Since your distance method is a funciton, it can only accept one or two arguments.")
			}
		}
		return(dst)
	}

	if(cluster_rows) {
		row_hclust <<- hclust(get_dist(matrix, clustering_distance_rows), method = clustering_method_rows)
		row_order = row_hclust$order
	} else {
		row_order = seq_len(nrow(matrix))
	}

	if(cluster_columns) {
		column_hclust <<- hclust(get_dist(t(matrix), clustering_distance_columns), method = clustering_method_columns)
		column_order = column_hclust$order
	} else {
		column_order = seq_len(ncol(matrix))
	}

	matrix <<- matrix[row_order, column_order, drop = FALSE]

	if(is.null(annotation)) {
		# don't need to consider annotation_color
	} else if(is.data.frame(annotation)) {
		# if there is rownames
		if(is.null(rownames(annotation))) {
			column_anno <<- annotation[col_order, , drop = FALSE]
		} else {
			column_anno <<- annotation[colnames(matrix), , drop = FALSE]
		}

		if(is.null(colnames(annotation))) {
			stop("`annotation` should have colnames.")
		}
		if(is.null(names(annotation_color))) {
			stop("`annotation_color` should have names to map to `annotation`.")
		}

		if(!setequal(colnames(annotation), names(annotation_color))) {
			stop()
		} else {
			annotation_color = annotation_color[colnames(annotation)]
			annotation_name = names(annotation_color)
			column_anno_color_mapping = list()
			for(i in seq_along(annotation_color)) {
				if(is.atomic(annotation_color[[i]])) {
					column_anno_color_mapping[[i]] <<- ColorMapping(name = annotation_name[i],
						                                     colors = annotation_color[[i]])
				} else if(is.function(annotation_color[[i]])) {
					column_anno_color_mapping[[i]] <<- ColorMapping(name = annotation_name[i],
						                                          col_fun = annotation_color[[i]])
				}
			}
		}
	} else {
		stop("`annotation` should be a data frame.")
	}

	# settings for positin of each component

	layout_index <<- rbind(c(5, 3))
	graphic_fun_list <<- list(function() .self$draw_heatmap_body())

	title_side = match.arg(title_side)[1]
    if(draw_title) {
    	title <<- title
    	if(title_side == "top") {
    		layout_title_top_height <<- grobHeight(textGrob(title, gp = title_gp))
    		layout_title_bottom_height <<- unit(0, "null")
    		layout_index <<- rbind(layout_index, c(1, 3))
    	} else {
    		layout_title_bottom_height <<- grobHeight(textGrob(title, gp = title_gp))
    		layout_title_top_height <<- unit(0, "null")
    		layout_index <<- rbind(layout_index, c(9, 3))
    	}
    	graphic_fun_list <<- c(graphic_fun_list, function() .self$draw_title())
    } else {
    	title <<- character(0)
    	layout_title_top_height <<- unit(0, "null")
    	layout_title_bottom_height <<- unit(0, "null")
    }

    row_hclust_side = match.arg(row_hclust_side)[1]
    if(show_row_hclust) {
    	if(row_hclust_side == "left") {
    		layout_row_hclust_left_width <<- row_hclust_width
    		layout_row_hclust_right_width <<- unit(0, "null")
    		layout_index <<- rbind(layout_index, c(5, 1))
    	} else {
    		layout_row_hclust_right_width <<- row_hclust_width
    		layout_row_hclust_left_width <<- unit(0, "null")
    		layout_index <<- rbind(layout_index, c(5, 5))
    	}
    	graphic_fun_list <<- c(graphic_fun_list, function() .self$draw_hclust(which = "row", side = row_hclust_side))
    } else {
    	layout_row_hclust_right_width <<- unit(0, "null")
    	layout_row_hclust_left_width <<- unit(0, "null")	
    }

    column_hclust_side = match.arg(column_hclust_side)[1]
    if(show_column_hclust) {
    	if(column_hclust_side == "top") {
			layout_column_hclust_top_height <<- column_hclust_height
    		layout_column_hclust_bottom_height <<- unit(0, "null")
    		layout_index <<- rbind(layout_index, c(2, 3))
    	} else {
    		layout_column_hclust_bottom_height <<- column_hclust_height
    		layout_column_hclust_top_height <<- unit(0, "null")
    		layout_index <<- rbind(layout_index, c(8, 3))
    	}
    	graphic_fun_list <<- c(graphic_fun_list, function() .self$draw_hclust(which = "column", side = column_hclust_side))
    } else {
    	layout_column_hclust_top_height <<- unit(0, "null")
    	layout_column_hclust_bottom_height <<- unit(0, "null")	
    }
    
    rownames_side = match.arg(rownames_side)[1]
    if(is.null(rownames(matrix))) {
    	show_rownames = FALSE
    }
    if(show_rownames) {
    	rownames_width = max(do.call("unit.c", lapply(rownames(matrix), function(x) {
			grobWidth(textGrob(x, gp = rownames_gp))
		})))
		if(rownames_side == "left") {
			layout_rownames_left_width <<- rownames_width
			layout_rownames_right_width <<- unit(0, "null")
			layout_index <<- rbind(layout_index, c(5, 2))
		} else {
			layout_rownames_right_width <<- rownames_width
			layout_rownames_left_width <<- unit(0, "null")
			layout_index <<- rbind(layout_index, c(5, 4))
		}
		graphic_fun_list <<- c(graphic_fun_list, function() .self$draw_dimnames(which = "row", side = rownames_side))
    } else {
    	layout_rownames_left_width <<- unit(0, "null")
    	layout_rownames_right_width <<- unit(0, "null")
    }

    colnames_side = match.arg(colnames_side)[1]
    if(is.null(colnames(matrix))) {
    	show_colnames = FALSE
    }
    if(show_colnames) {
    	colnames_height = max(do.call("unit.c", lapply(colnames(matrix), function(x) {
			grobWidth(textGrob(x, gp = colnames_gp))
		})))
		if(colnames_side == "top") {
			layout_colnames_top_height <<- colnames_height
			layout_colnames_bottom_height <<- unit(0, "null")
			layout_index <<- rbind(layout_index, c(4, 3))
		} else {
			layout_colnames_bottom_height <<- colnames_height
			layout_colnames_top_height <<- unit(0, "null")
			layout_index <<- rbind(layout_index, c(6, 3))
		}
		graphic_fun_list <<- c(graphic_fun_list, function() .self$draw_dimnames(which = "column", side = colnames_side))
    } else {
    	layout_colnames_top_height <<- unit(0, "null")
    	layout_colnames_bottom_height <<- unit(0, "unit")
    }
    
	column_anno_side = match.arg(annotation_side)[1]
	if(is.null(annotation)) {
		layout_column_anno_top_height <<- unit(0, "null")
		layout_column_anno_bottom_height <<- unit(0, "null")
	} else {
		if(column_anno_side == "top") {
			layout_column_anno_top_height <<- annotation_height
			layout_column_anno_bottom_height <<- unit(0, "null")
			layout_index <<- rbind(layout_index, c(3, 3))
		} else {
			layout_column_anno_bottom_height <<- annotation_height
			layout_column_anno_top_height <<- unit(0, "null")
			layout_index <<- rbind(layout_index, c(7, 3))
		}
		graphic_fun_list <<- c(graphic_fun_list, function() .self$draw_annotation())
	}

	return(invisible(.self))
})

# show method is in fact a plot method
Heatmap$methods(show = function(...) {
	ht_list = new("HeatmapList")
	ht_list$add_heatmap(.self)
	ht_list
})

Heatmap$methods(add_heatmap = function(ht) {
	ht_list = new("HeatmapList")
	ht_list$add_heatmap(.self)
	ht_list$add_heatmap(ht)
})

# add the heatmap body
# 100% covered the viewport
Heatmap$methods(draw_heatmap_body = function(...) {
	pushViewport(viewport(name = paste(.self$name, "heatmap_body", sep = "-")))
	col_matrix = .self$matrix_color_mapping$map(.self$matrix)

	nc = ncol(.self$matrix)
	nr = nrow(.self$matrix)
	x = (seq_len(nc) - 0.5) / nc
	y = (rev(seq_len(nr)) - 0.5) / nr
	df = expand.grid(x, y)
	grid.rect(df[[1]], df[[2]], width = 1/nc, height = 1/nr, gp = gpar(fill = col_matrix, ...))
	upViewport()
})

# 100% percent to covert the whole viewport
Heatmap$methods(draw_hclust = function(which = c("row", "column"), 
	side = ifelse(which == "row", "left", "top"), ...) {

	which = match.arg(which)[1]

	side = side[1]
	if(which == "row" && side %in% c("top", "bottom")) {
		stop("`side` can only be set to 'left' or 'right' if `which` is 'row'.")
	}

	if(which == "column" && side %in% c("left", "right")) {
		stop("`side` can only be set to 'top' or 'bottom' if `which` is 'column'.")
	}

	hc = switch(which,
		"row" = .self$row_hclust,
		"column" = .self$column_hclust)

	if(is.null(hc)) {
		return(invisible(NULL))
	}

	h = hc$height / max(hc$height)
	m = hc$merge
	o = hc$order
	n = length(o)

	m[m > 0] = n + m[m > 0] 
	m[m < 0] = abs(m[m < 0])

	dist = matrix(0, nrow = 2 * n - 1, ncol = 2, dimnames = list(NULL, c("x", "y"))) 
	dist[1:n, 1] = 1 / n / 2 + (1 / n) * (match(1:n, o) - 1)

	for(i in 1:nrow(m)){
		dist[n + i, 1] = (dist[m[i, 1], 1] + dist[m[i, 2], 1]) / 2
		dist[n + i, 2] = h[i]
	}
	
	draw_connection = function(x1, x2, y1, y2, y){
		grid.lines(x = c(x1, x1), y = c(y1, y))
		grid.lines(x = c(x2, x2), y = c(y2, y))
		grid.lines(x = c(x1, x2), y = c(y, y))
	}
	
	if( (which == "row" && side == "left") | (which == "column" && side == "bottom") ) {
		dist[, 1] = 1 - dist[, 1]
	}

	if(which == "row") {
		pushViewport(viewport(angle = 90, name = paste(.self$name, "hclust_row", sep = "-") ))
	} else {
		pushViewport(viewport(name = paste(.self$name, "hclust_col", sep = "-") ))
	}
	
	for(i in 1:nrow(m)){
		draw_connection(dist[m[i, 1], 1], dist[m[i, 2], 1], dist[m[i, 1], 2], dist[m[i, 2], 2], h[i])
	}

	upViewport()
})

# width is fixed
Heatmap$methods(draw_dimnames = function(which = c("row", "column"),
	side = ifelse(which == "row", "right", "bottom"), ...) {

	which = match.arg(which)[1]

	side = side[1]
	if(which == "row" && side %in% c("bottom", "top")) {
		stop("`side` can only be set to 'left' or 'right' if `which` is 'row'.")
	}

	if(which == "column" && side %in% c("left", "right")) {
		stop("`side` can only be set to 'top' or 'bottom' if `which` is 'column'.")
	}

	nm = switch(which,
		"row" = rownames(.self$matrix),
		"column" = colnames(.self$matrix))
	
	if(is.null(nm)) {
		return(invisible(NULL))
	}

	just = switch(side,
		       bottom = c("left", "center"),
		       left = c("right", "center"),
		       top = c("left", "center"),
		       right = c("left", "center"))

	n = length(nm)
	x = unit(0, "npc")
	if(which == "row") {
		pushViewport(viewport(name = paste(.self$name, "rownames", sep = "-") ))
		y = (rev(seq_len(n)) - 0.5) / n
	} else {
		pushViewport(viewport(angle = 90, name = paste(.self$name, "colnames", sep = "-") ))
		y = (seq_len(n) - 0.5) / n
	}
	grid.text(nm, x, y, gp = gpar(...))

	upViewport()
})

Heatmap$methods(draw_title = function(title = .self$title, ...) {
	pushViewport(viewport(name = paste(.self$name, "title", sep = "-")))
	grid.text(title, gp = gpar(...))
	upViewport()
})

Heatmap$methods(draw_annotation = function(...) {
	
	# if there is no annotation, draw nothing
	if(is.null(.self$column_anno)) {
		return(invisible(NULL))
	}

	n = ncol(.self$column_anno)
	nc = nrow(.self$column_anno)  # number of columns which correspond to the matrix
	pushViewport(viewport(name = paste(.self$name, "annotation", sep = "-")))
	
	for(i in seq_len(n)) {
		x = (seq_len(nc) - 0.5) / nc
		y = (n - i + 0.5) / n
		cm = .self$column_anno_color_mapping[[i]]
		fill = cm$map(.self$column_anno[[i]])
		grid.rect(x, y, width = 1/nc, height = 1/n, gp = gpar(fill = fill, ...))
	}
	upViewport()
})


"+.heatmap" = function(ht1, ht2) {
	ht1$add_heatmap(ht2)
}

dist2 = function(mat, pairwise_fun = function(x, y) sqrt(sum((x - y)^2))) {

	if(!is.matrix(mat)) {
		stop("`mat` should be a matrix.")
	}

	if(nrow(mat) < 2) {
		stop("`mat` should have at least two rows.")
	}

	nr = nrow(mat)
	mat2 = matrix(NA, nrow = nr, ncol = nr)
	rownames(mat2) = colnames(mat2) = rownames(mat)

	for(i in 2:nr) {
		for(j in 1:(nr-1)) {
			mat2[i, j] = pairwise_fun(mat[i, ], mat[j, ])
		}
	}

	as.dist(mat2)
}


