
###############################
# class for single heatmap
#
# it provides methods for
# add main heatmap, row cluster, column cluster,
#     annotation, rownames, colnames, sub-title
#

setRefClass("heatmap",
    fields = list(
    	"name" = "character",
    	"matrix" = "matrix",  # original order
    	"hclust_row" = "hclust",
    	"hclust_col" = "hclust",
    	"column_anno" = "ANY", # annotation data frame, order of columns are same as matrix
    	"column_anno_color_mapping" = "list", # a list of colorMapping class objects
    	"matrix_color_mapping" = "colorMapping")

heatmap$methods(initialize = function(matrix, order_row_by = "hclust", order_col_by = "hclust",
	annotation = NULL, annotation_color = NULL, ...) {

})

heatmap$methods(show = function(...) {
	print(.self$draw(), ...)
})

heatmap$methods(subset = function(row_index = NULL, col_index = NULL) {

	ht = .self$copy()

	if(!is.null(row_index) && !is.null(col_index)) {
		ht$matrix <<- ht$matrix[row_index, col_index, drop = FALSE]
	} else if(!is.null(row_index)) {
		ht$matrix <<- ht$matrix[row_index, , drop = FALSE]
	} else if(!is.null(col_index)) {
		ht$matrix <<- ht$matrix[, col_index, , drop = FALSE]
	}
	return(ht)
})

heatmap$methods(add_heatmap = function(ht) {
	ht_list = new("heatmapList")
	ht_list$add_heatmap(.self)
	ht_list$add_heatmap(ht)
})

# add the heatmap body
heatmap$methods(draw_heatmap_body = function(...) {
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

heatmap$methods(draw_hclust = function(which = c("row", "column"), 
	side = ifelse(which == "row", 2, 3), ...) {

	which = match.arg(which)[1]

	side = side[1]
	if(which == "row" && side %in% c(1, 3)) {
		stop("`side` can only be set to 2 or 4 if `which` is 'row'.")
	}

	if(which == "column" && side %in% c(2, 4)) {
		stop("`side` can only be set to 1 or 3 if `which` is 'column'.")
	}

	hc = switch(which,
		"row" = .self$hclust_row,
		"column" = .self$hclust_col)

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
	
	if( (which == "row" && side == 2) | (which == "column" && side == 1) ) {
		dist = 1 - dist[, 1]
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


heatmap$methods(draw_dimnames = function(which = c("row", "column"),
	side = ifelse(which == "row", 2, 3), ...) {

	which = match.arg(which)[1]

	side = side[1]
	if(which == "row" && side %in% c(1, 3)) {
		stop("`side` can only be set to 2 or 4 if `which` is 'row'.")
	}

	if(which == "column" && side %in% c(2, 4)) {
		stop("`side` can only be set to 1 or 3 if `which` is 'column'.")
	}

	nm = switch(which,
		"row" = rownames(.self$matrix),
		"column" = colnames(.self$hclust_col))

	if(which == "row") {
		pushViewport(viewport(angle = 90, name = paste(.self$name, "rownames", sep = "-") ))
	} else {
		pushViewport(viewport(name = paste(.self$name, "colnames", sep = "-") ))
	}
	
	if(side == 1) {
		just = c("left", "center")
	} else if(side == 2) {
		just = c("right", "center")
	} else if(side == 3) {
		just = c("left", "center")
	} else if(side == 4) {
		just = c("left", "center")
	}

	n = length(nm)
	x = unit(0, "npc")
	if(which == "row") {
		pushViewport(viewport(angle = 90, name = paste(.self$name, "rownames", sep = "-") ))
		y = (rev(seq_len(n)) - 0.5) / n
	} else {
		pushViewport(viewport(name = paste(.self$name, "colnames", sep = "-") ))
		y = (seq_len(n) - 0.5) / n
	}
	grid.text(nm, x, y, gp = gpar(...))

	upViewport()
})

heatmap$methods(draw_annotation = function(...) {
	
	n = nrow(.self$column_anno)
	nc = nrow(.self$column_anno)  # number of columns which correspond to the matrix
	pushViewport(viewport(name = paste(.self$name, "heatmap_body", sep = "-")))
	
	for(i in seq_len(n)) {
		x = (seq_len(nc) - 0.5) / nc
		y = (n - i + 0.5) / n
		cm = .self$column_anno_color_mapping[[i]]
		fill = cm$map(.self$column_anno[[i]])
		grid.rect(x, y, width = 1/nc, height = 1/nr, gp = gpar(fill = fill, ...))
	}
	upViewport()
})

heatmap$methods(draw = function(hclust_row_side = 2, hclust_col_side = 3, rownames_side = 4, colnames_side = 1,
	annotation_side = 3) {
	# create a viewport with layout
	hclust_row_width
	hclust_col_height
	rownames_width
	colnames_height
	annotation_height

	heatmap_body_row_index = 2
	if(hclust_row_side == 2 && rownames_side == 4) {
		layout_width = unit.c(hclust_row_width, unit(1, "null"), rownames_width)
		add_row_graphics_fun_list = list(function() .self$draw_hclust(),
			                             function() .self$draw_heatmap_body(),
			                             function() .self$draw_dimnames())
	} else if(hclust_row_side == 4 && rownames_side == 2) {
		layout_width = unit.c(rownames_width, unit(1, "null", hclust_row_width))
		add_row_graphics_fun_list = list(function() .self$draw_dimnames(),
			                             function() .self$draw_heatmap_body(),
			                             function() .self$draw_hclust())
	} else {
		stop("")
	}

	
	if(hclust_col_side == 3 && annotation_side == 3 && colnames_side == 1) {
		layout_height = unit.c(hclust_col_height, annotation_height, unit(1, "null"), colnames_height)
		add_col_graphics_fun_list = list(function() .self$draw_hclust(),
		                                 function() .self$draw_annotation(),
		                                 function() .self$draw_heatmap_body(),
		                                 function() .self$draw_dimnames())
		heatmap_body_col_index = 3
	} else if(hclust_col_side == 3 && annotation_side == 1 && colnames_side == 1) {
		layout_height = unit.c(hclust_col_height, unit(1, "null"), annotation_height, colnames_height)
		add_col_graphics_fun_list = list(function() .self$draw_hclust(),
		                                 function() .self$draw_heatmap_body(),
		                                 function() .self$draw_annotation(),
		                                 function() .self$draw_dimnames())
		heatmap_body_col_index = 2
	} else if(hclust_col_side == 1 && annotation_side == 1 && colnames_side == 3) {
		layout_height = unit.c(colnames_height, unit(1, "null"), annotation_height, hclust_col_height)
		add_col_graphics_fun_list = list(function() .self$draw_dimnames(),
		                                 function() .self$draw_heatmap_body(),
		                                 function() .self$draw_annotation(),
			                             function() .self$draw_hclust())
		heatmap_body_col_index = 2
	} else if(hclust_col_side == 1 && annotation_side == 3 && colnames_side == 3) {
		layout_height = unit.c(colnames_height, annotation_height, unit(1, "null"), hclust_col_height)
		add_col_graphics_fun_list = list(function() .self$draw_dimnames(),
		                                 function() .self$draw_annotation(),
		                                 function() .self$draw_heatmap_body(),
			                             function() .self$draw_hclust())
		heatmap_body_col_index = 3
	} else {
		stop("")
	}

	layout = grid.layout(nrow = 4, ncol = 3, widths = layout_width, heights = layout_height)
	pushViewport(viewport(layout = layout))

	for(ri in 1:4) {
		for(ci in 1:3) {

			# add row cluster, heatmap_body and rownames
			if(ri == heatmap_body_row_index) {
				pushViewport(viewport(layout.pos.row = ri, layout.pos.col = ci))
				add_row_graphics_fun_list[[ci]]()
				upViewport()
			} else if(ci == heatmap_body_col_index) {
				pushViewport(viewport(layout.pos.row = ri, layout.pos.col = ci))
				add_col_graphics_fun_list[[ri]]()
				upViewport()
			}
		}
	}

	upViewport()
})


"+.heatmap" = function(ht1, ht2) {
	ht1$add_heatmap(ht2)
}

