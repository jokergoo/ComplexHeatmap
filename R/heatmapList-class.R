
# a list of heatmaps
HeatmapList = setRefClass("HeatmapList",
	fields = list(
		"ht_list" = "list",

		layout = "list",
    	gp_list = "list"
	)
)

# make some re-formatting for each heatmap
HeatmapList$methods(add_heatmap = function(ht) {
	
	# check settings of this new heatmap
	if(inherits(ht, "Heatmap")) {
		ht = list(ht)
		names(ht) = ht$name
	}

	# if ht is a HeatmapList, all settings are already checked
	.self$ht_list = c(.self$ht_list, ht)
	return(.self)
})

# draw everthing
HeatmapList$methods(make_layout = function(
	row_title = character(0), row_title_side = c("left", "right"), row_title_gp = gpar(fontsize = 14),
	column_title = character(0), column_title_side = c("top", "bottom"), column_title_gp = gpar(fontsize = 14),
	heatmap_legend_side = c("right", "left", "bottom", "top"), show_heatmap_legend = TRUE,
	annotation_legend_side = c("right", "left", "bottom", "top"), show_annotation_legend = TRUE
	) {

	.self$layout = list(
		layout_annotation_legend_left_width = NULL,
		layout_heatmap_legend_left_width = NULL,
		layout_row_title_left_width = NULL,
		layout_row_title_right_width = NULL,
		layout_heatmap_legend_right_width = NULL,
		layout_annotation_legend_right_width = NULL,

		layout_annotation_legend_top_height = NULL,
		layout_heatmap_legend_top_height = NULL,
		layout_column_title_top_height = NULL,
		layout_column_title_bottom_height = NULL,
		layout_heatmap_legend_bottom_height = NULL,
		layout_annotation_legend_bottom_height = NULL
	)

	.self$layout$layout_index = rbind(c(4, 4))
	.self$layout$graphic_fun_list = list(function() .self$draw_heatmap_list())

	############################################
	## title on top or bottom
	column_title_side = match.arg(column_title_side)[1]
	if(length(column_title) == 0) {
		column_title = character(0)
	} else if(is.na(column_title)) {
		column_title = character(0)
	} else if(column_title == "") {
		column_title = character(0)
	}
    if(length(column_title) > 0) {
    	column_title = column_title
    	if(column_title_side == "top") {
    		.self$layout$layout_column_title_top_height = grobHeight(textGrob(column_title, gp = column_title_gp))*2
    		.self$layout$layout_column_title_bottom_height = unit(0, "null")
    		.self$layout$layout_index = rbind(layout$layout_index, c(3, 4))
    	} else {
    		.self$layout$layout_column_title_bottom_height = grobHeight(textGrob(column_title, gp = column_title_gp))*2
    		.self$layout$layout_column_title_top_height = unit(0, "null")
    		.self$layout$layout_index = rbind(layout$layout_index, c(5, 4))
    	}
    	.self$layout$graphic_fun_list = c(layout$graphic_fun_list, function() .self$draw_title(column_title, which = "column", side = column_title_side))
    } else {
    	.self$layout$layout_column_title_top_height = unit(0, "null")
    	.self$layout$layout_column_title_bottom_height = unit(0, "null")
    }

    ############################################
	## title on left or right
	row_title_side = match.arg(row_title_side)[1]
	if(length(row_title) == 0) {
		row_title = character(0)
	} else if(is.na(row_title)) {
		row_title = character(0)
	} else if(row_title == "") {
		row_title = character(0)
	}
    if(length(row_title) > 0) {
    	row_title = row_title
    	if(row_title_side == "left") {
    		.self$layout$layout_row_title_left_width = grobHeight(textGrob(row_title, gp = row_title_gp))*2
    		.self$layout$layout_row_title_right_width = unit(0, "null")
    		.self$layout$layout_index = rbind(layout$layout_index, c(4, 3))
    	} else {
    		.self$layout$layout_row_title_right_width = grobHeight(textGrob(row_title, gp = row_title_gp))*2
    		.self$layout$layout_row_title_left_width = unit(0, "null")
    		.self$layout$layout_index = rbind(layout$layout_index, c(4, 5))
    	}
    	.self$layout$graphic_fun_list = c(layout$graphic_fun_list, function() .self$draw_title(row_title, which = "row", side = row_title_side))
    } else {
    	.self$layout$layout_row_title_right_width = unit(0, "null")
    	.self$layout$layout_row_title_left_width = unit(0, "null")
    }

    #################################################
    ## heatmap legend to top, bottom, left and right
    # default values
    .self$layout$layout_heatmap_legend_top_height = unit(0, "null")
    .self$layout$layout_heatmap_legend_bottom_height = unit(0, "null")
    .self$layout$layout_heatmap_legend_left_width = unit(0, "null")
    .self$layout$layout_heatmap_legend_right_width = unit(0, "null")
    if(show_heatmap_legend) {
    	heatmap_legend_side = match.arg(heatmap_legend_side)[1]
    	if(heatmap_legend_side == "top") {
    		.self$layout$layout_heatmap_legend_top_height = .self$heatmap_legend_size(side = "top")[2]
    		.self$layout$layout_index = rbind(layout$layout_index, c(2, 4))
    	} else if(heatmap_legend_side == "bottom") {
    		.self$layout$layout_heatmap_legend_bottom_height = .self$heatmap_legend_size(side = "bottom")[2]
    		.self$layout$layout_index = rbind(layout$layout_index, c(6, 4))
    	} else if(heatmap_legend_side == "left") {
    		.self$layout$layout_heatmap_legend_left_width = .self$heatmap_legend_size(side = "left")[1]
    		.self$layout$layout_index = rbind(layout$layout_index, c(4, 2))
    	} else if(heatmap_legend_side == "right") {
    		.self$layout$layout_heatmap_legend_right_width = .self$heatmap_legend_size(side = "right")[1]
    		.self$layout$layout_index = rbind(layout$layout_index, c(4, 6))
    	}
    	.self$layout$graphic_fun_list = c(layout$graphic_fun_list, function() .self$draw_heatmap_legend(side = heatmap_legend_side))
    }

    #################################################
    ## annotation legend to top, bottom, left and right
    # default values
    .self$layout$layout_annotation_legend_top_height = unit(0, "null")
    .self$layout$layout_annotation_legend_bottom_height = unit(0, "null")
    .self$layout$layout_annotation_legend_left_width = unit(0, "null")
    .self$layout$layout_annotation_legend_right_width = unit(0, "null")
    if(show_annotation_legend) {
    	annotation_legend_side = match.arg(annotation_legend_side)[1]
    	if(annotation_legend_side == "top") {
    		.self$layout$layout_annotation_legend_top_height = .self$annotation_legend_size(side = "top")[2]
    		.self$layout$layout_index = rbind(layout$layout_index, c(1, 4))
    	} else if(annotation_legend_side == "bottom") {
    		.self$.self$layout$layout_annotation_legend_bottom_height = .self$annotation_legend_size(side = "bottom")[2]
    		.self$layout$layout_index = rbind(layout$layout_index, c(7, 4))
    	} else if(heatmap_legend_side == "left") {
    		.self$layout$layout_annotation_legend_left_width = .self$annotation_legend_size(side = "left")[1]
    		.self$layout$layout_index = rbind(layout$layout_index, c(4, 1))
    	} else if(annotation_legend_side == "right") {
    		.self$layout$layout_annotation_legend_right_width = .self$annotation_legend_size(side = "right")[1]
    		.self$layout$layout_index = rbind(layout$layout_index, c(4, 7))
    	}
    	.self$layout$graphic_fun_list = c(layout$graphic_fun_list, function() .self$draw_annotation_legend(side = annotation_legend_side))
    }
})

HeatmapList$methods(draw = function(..., newpage = TRUE) {

	.self$make_layout(...)

	if(newpage) {
		grid.newpage()
	}

	layout = grid.layout(nrow = 7, ncol = 7, widths = .self$component_width(1:7), heights = .self$component_height(1:7))
	pushViewport(viewport(layout = layout, name = "global"))
	ht_layout_index = .self$layout$layout_index
	ht_graphic_fun_list = .self$layout$graphic_fun_list
	
	for(j in seq_len(nrow(ht_layout_index))) {
		pushViewport(viewport(layout.pos.row = ht_layout_index[j, 1], layout.pos.col = ht_layout_index[j, 2]))
		ht_graphic_fun_list[[j]]()
		upViewport()
	}

	upViewport()
})


HeatmapList$methods(component_width = function(k = 1:7) {

	.single_unit = function(k) {
	    if(k == 1) {
	    	.self$layout$layout_annotation_legend_left_width
	    } else if(k == 2) {
	    	.self$layout$layout_heatmap_legend_left_width
	    } else if(k == 3) {
	    	.self$layout$layout_row_title_left_width
	    } else if(k == 4) {
	    	unit(1, "null")
	    } else if(k == 5) {
	    	.self$layout$layout_row_title_right_width
	    } else if(k == 6) {
	    	.self$layout$layout_heatmap_legend_right_width
	    } else if(k == 7) {
	    	.self$layout$layout_annotation_legend_right_width
	    } else {
			stop("wrong 'k'")
		}
	}

	do.call("unit.c", lapply(k, function(i) .single_unit(i)))
})

HeatmapList$methods(component_height = function(k = 1:7) {

	.single_unit = function(k) {
		if(k == 1) {
			.self$layout$layout_annotation_legend_top_height
		} else if(k == 2) {
			.self$layout$layout_heatmap_legend_top_height
		} else if(k == 3) {
			.self$layout$layout_column_title_top_height
		} else if(k == 4) {
			unit(1, "null")
		} else if(k == 5) {
			.self$layout$layout_column_title_bottom_height
		} else if(k == 6) {
			.self$layout$layout_heatmap_legend_bottom_height
		} else if(k == 7) {
			.self$layout$layout_annotation_legend_bottom_height
		} else {
			stop("wrong 'k'")
		}
	}

	do.call("unit.c", lapply(k, function(i) .single_unit(i)))
})


# initialize the layout
HeatmapList$methods(draw_heatmap_list = function() {

	n = length(.self$ht_list)

	# since each heatmap actually has nine rows, calculate the maximum height of corresponding rows in all heatmap 
	max_component_height = unit.c(
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 1)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 2)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 3)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 4)))),
		unit(1, "null"),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 6)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 7)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 8)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 9))))
	)

	# set back to each heatmap
	for(i in seq_len(n)) {
		.self$ht_list[[i]]$set_component_height(k = 1, max_component_height[1])
		.self$ht_list[[i]]$set_component_height(k = 2, max_component_height[2])
		.self$ht_list[[i]]$set_component_height(k = 3, max_component_height[3])
		.self$ht_list[[i]]$set_component_height(k = 4, max_component_height[4])
		.self$ht_list[[i]]$set_component_height(k = 6, max_component_height[6])
		.self$ht_list[[i]]$set_component_height(k = 7, max_component_height[7])
		.self$ht_list[[i]]$set_component_height(k = 8, max_component_height[8])
		.self$ht_list[[i]]$set_component_height(k = 9, max_component_height[9])
	}

	width_without_heatmap_body = do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_width(c(1:3, 5:7))))
	heatmap_ncol = sapply(.self$ht_list, function(ht) ncol(ht$matrix))

	# width for body for each heatmap
	heatmap_body_width = (unit(1, "npc") - sum(width_without_heatmap_body)) * (1/sum(heatmap_ncol)) * heatmap_ncol

	# width of heatmap including body, and other components
	heatmap_width = sum(width_without_heatmap_body[1:3]) + heatmap_body_width[1] + sum(width_without_heatmap_body[5:7-1])

	for(i in seq_len(n - 1) + 1) {
		heatmap_width = unit.c(heatmap_width, sum(width_without_heatmap_body[6*(i-1) + 1:3]) + heatmap_body_width[i] + sum(width_without_heatmap_body[6*(i-1) + 5:7-1]))
	}

	pushViewport(viewport(name = "main_heatmap_list"))
	
	x = unit(0, "npc")
	for(i in seq_len(n)) {
		pushViewport(viewport(x = x, y = unit(0, "npc"), width = heatmap_width[i], just = c("left", "bottom"), name = paste0("heatmap_", .self$ht_list[[i]]$name)))
		ht = .self$ht_list[[i]]
		ht$draw()
		upViewport()

		x = x + sum(heatmap_width[seq_len(i)])
	}

	upViewport()

})

Heatmap$methods(draw_title = function(title, which = c("row", "column"),
	side = ifelse(which == "row", "right", "bottom"), gp = NULL) {
	which = match.arg(which)[1]

	side = side[1]
	if(which == "row" && side %in% c("bottom", "top")) {
		stop("`side` can only be set to 'left' or 'right' if `which` is 'row'.")
	}

	if(which == "column" && side %in% c("left", "right")) {
		stop("`side` can only be set to 'top' or 'bottom' if `which` is 'column'.")
	}

	if(is.null(gp)) {
		gp = switch(which,
			"row" = .self$gp_list$row_title_gp,
			"column" = .self$gp_list$column_title_gp)
	}

	if(which == "row") {
		rot = switch(side,
			"left" = 90,
			"right" = 270)

		pushViewport(viewport(name = "global_row_title", clip = FALSE))
		grid.text(title, rot = rot, gp = gp)
		upViewport()
	} else {
		pushViewport(viewport(name = "global_column_title", clip = FALSE))
		grid.text(title, gp = gp)
		upViewport()
	}
})

HeatmapList$methods(draw_heatmap_legend = function(side = c("right", "left", "top", "bottom")) {

	side = match.arg(side)[1]

	ColorMappingList = lapply(.self$ht_list, function(ht) ht$matrix_color_mapping)
	draw_legend(ColorMappingList, side = side)
})

HeatmapList$methods(draw_annotation_legend = function(side = c("right", "left", "top", "bottom")) {

	side = match.arg(side)[1]

	ColorMappingList = do.call("c", lapply(.self$ht_list, function(ht) ht$column_anno_color_mapping))
	nm = names(ColorMappingList)
	ColorMappingList = ColorMappingList[nm]
	draw_legend(ColorMappingList, side = side)
})

HeatmapList$methods(heatmap_legend_size = function(side = c("right", "left", "top", "bottom")) {

	side = match.arg(side)[1]

	ColorMappingList = lapply(.self$ht_list, function(ht) ht$matrix_color_mapping)
	draw_legend(ColorMappingList, side = side, plot = FALSE)
})

HeatmapList$methods(annotation_legend_size = function(side = c("right", "left", "top", "bottom"), 
	vp_width = unit(1, "npc"), vp_height = unit(1, "npc")) {

	side = match.arg(side)[1]

	ColorMappingList = do.call("c", lapply(.self$ht_list, function(ht) ht$column_anno_color_mapping))
	nm = names(ColorMappingList)
	ColorMappingList = ColorMappingList[nm]
	draw_legend(ColorMappingList, side = side, plot = FALSE, vp_width = vp_width, vp_height = vp_height)
})

draw_legend = function(ColorMappingList, side = c("right", "left", "top", "bottom"), plot = TRUE,
	vp_width = unit(1, "npc"), vp_height = unit(1, "npc")) {

	side = match.arg(side)[1]

	n = length(ColorMappingList)

	if(side %in% c("left", "right")) {
		current_x = unit(0, "npc")
		current_width = unit(0, "null")
		current_y = vp_height
		for(i in seq_len(n)) {
			cm = ColorMappingList[[i]]
			size = cm$legend(plot = FALSE)
			# if this legend is too long that it exceed the bottom of the plotting region
			# it also works for the first legend if it is too long
			#if(compare_unit(current_y - size[2], unit(0, "npc")) < 0) {
			if(0){
				# go to next column
				current_y = unit(1, "npc")
				current_x = current_width
				current_width = current_x + size[1]

				if(plot) cm$legend(x = current_x, y = current_y, just = c("left", "top"), plot = TRUE)
				current_y = current_y - size[2] # move to the bottom
			} else {
				# if this legend is wider
				if(compare_unit(current_width, current_x + size[1]) < 0) {
					current_width = current_x + size[1]
				}

				if(plot) cm$legend(x = current_x, y = current_y, just = c("left", "top"), plot = TRUE)
				current_y = current_y - size[2] # move to the bottom
			}
		}

		return(unit.c(current_width, vp_height))

	} else if(side %in% c("top", "bottom")) {
		current_x = unit(0, "npc")
		current_height = vp_height
		current_y = vp_height
		for(i in seq_len(n)) {
			cm = ColorMappingList[[i]]
			size = cm$legend(plot = FALSE)
			
			# if adding the legend exceeding ...
			#if(compare_unit(current_x + size[1], vp_width) > 0) {
			if(0) {
				# go to next column
				current_y = unit(1, "npc") - current_height
				current_x = unit(0, "npc")
				current_height = current_y - size[2]

				if(plot) cm$legend(x = current_x, y = current_y, just = c("left", "top"), plot = TRUE)
				current_x = current_x + size[1]
			} else {
				# if height of this legend is larger
				if(compare_unit(current_height, current_y - size[2]) > 0) {
					current_height = current_y - size[2]
				}

				if(plot) cm$legend(x = current_x, y = current_y, just = c("left", "top"), plot = TRUE)
				current_x = current_x + size[1]
			}
		}

		return(unit.c(vp_width, unit(1, "npc") - current_height))
		
	}
}

HeatmapList$methods(show = function() {
	cat("A HeatmapList class instance contains:\n\n")
	for(i in seq_along(.self$ht_list)) {
		cat("[", i, "] ", sep = "")
		.self$ht_list[[i]]$show()
		cat("\n")
	}
})

"+.HeatmapList" = function(ht1, ht2) {
	if(inherits(ht2, "Heatmap") || inherits(ht2, "HeatmapList")) {
		ht1$add_heatmap(ht2)
	} else if(inherits(ht2, "HeatmapListConfig")) {
		ht1$add_config(ht2)
	}
}

HeatmapListConfig = function(...) {
	x = as.list(...)
	class(x) = "HeatmapListConfig"
	return(x)
}

compare_unit = function(u1, u2) {
	u1 = convertUnit(u1, "cm", valueOnly = TRUE)
	u2 = convertUnit(u2, "cm", valueOnly = TRUE)
	ifelse(u1 > u2, 1, ifelse(u1 < u2, -1, 0))
}