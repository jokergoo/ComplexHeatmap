
# == title
# Making legend grobs
#
# == param
# -at breaks, can be wither numeric or character
# -labels labels corresponding to ``at``
# -nrow if there are too many legends, they can be positioned in an array, this controls number of rows
# -ncol if there are too many legends, they can be positioned in an array, this controls number of columns.
#       At a same time only one of ``nrow`` and ``ncol`` can be specified.
# -col_fun a color mapping function which is used to make a continuous color bar
# -grid_height height of legend grid
# -grid_width width of legend grid
# -gap when legends are put in multiple columns, this is the gap between neighbouring columns, measured as a `grid::unit` object
# -labels_gp graphic parameters for labels
# -border color of legend borders, also for the ticks in the continuous legend
# -background background colors
# -type type of legends, can be ``grid``, ``points`` and ``lines``
# -legend_gp graphic parameters for the legend
# -pch type of points
# -size size of points
# -legend_height height of the whole legend, used when ``col_fun`` is specified and ``direction`` is set to ``vertical``
# -legend_width width of the whole legend, used when ``col_fun`` is specified  and ``direction`` is set to ``horizontal``
# -direction direction of the continuous legend
# -title title of the legend
# -title_gp graphic parameters of title
# -title_position position of title according to the legend
#
# == value
# A `grid::grob` object
#
Legend = function(at, labels = at, nrow = NULL, ncol = 1, col_fun,
	grid_height = unit(4, "mm"), grid_width = unit(4, "mm"), gap = unit(2, "mm"),
	labels_gp = gpar(fontsize = 10),
	border = NULL, background = "#EEEEEE",
	type = "grid", legend_gp = gpar(), 
	pch = 16, size = unit(2, "mm"),
	legend_height = NULL, legend_width = NULL,
	direction = c("vertical", "horizontal"),
	title = "", title_gp = gpar(fontsize = 10, fontface = "bold"), 
	title_position = c("topleft", "topcenter", "leftcenter", "lefttop")) {

	if(missing(col_fun)) {
		if(is.null(border)) border = "white"
		legend_body = discrete_legend_body(at = at, labels = labels, nrow = nrow, ncol = ncol,
			grid_height = grid_height, grid_width = grid_width, gap = gap, labels_gp = labels_gp,
			border = border, background = background, type = type, legend_gp = legend_gp,
			pch = pch, size = size)
	} else {
		direction = match.arg(direction)[1]
		if(direction == "vertical") {
			legend_body = vertical_continuous_legend_body(at = at, labels = labels, col_fun = col_fun,
				grid_height = grid_height, grid_width = grid_width, legend_height = legend_height,
				labels_gp = labels_gp, border = border)
		} else {
			legend_body = horizontal_continuous_legend_body(at = at, labels = labels, col_fun = col_fun,
				grid_height = grid_height, grid_width = grid_width, legend_width = legend_width,
				labels_gp = labels_gp, border = border)
		}
	}
	if(missing(title)) {
		return(legend_body)
	}
	if(is.null(title)) {
		return(legend_body)
	}
	if(title == "") {
		return(legend_body)
	}

	title_grob = textGrob(title, gp = title_gp)
	title_height = grobHeight(title_grob)
	title_width = grobWidth(title_grob)

	legend_width = grobWidth(legend_body)
	legend_height = grobHeight(legend_body)

	title_position = match.arg(title_position)[1]
	if(title_position %in% c("topleft", "topcenter")) {
		if(convertWidth(title_width, "mm", valueOnly = TRUE) > convertWidth(legend_width, "mm", valueOnly = TRUE) && title_position == "topleft") {
			total_width = title_width
			empty_width = total_width - legend_width
			gf = frameGrob(layout = grid.layout(nrow = 2, ncol = 2, 
				widths = unit.c(legend_width, empty_width),
				heights = unit.c(title_height + unit(1.5, "mm"), legend_height)))

			gf = placeGrob(gf, row = 1, col = 1:2, grob = textGrob(title, unit(0, "npc"), unit(1, "npc"), just = c("left", "top"), gp = title_gp))
			gf = placeGrob(gf, row = 2, col = 1, grob = legend_body)
		} else {
			total_width = max(unit.c(title_width, legend_width))
			gf = frameGrob(layout = grid.layout(nrow = 2, ncol = 1, 
				widths = total_width,
				heights = unit.c(title_height + unit(1.5, "mm"), legend_height)))
			
			if(title_position == "topleft") {
				gf = placeGrob(gf, row = 1, col = 1, grob = textGrob(title, unit(0, "npc"), unit(1, "npc"), just = c("left", "top"), gp = title_gp))
				gf = placeGrob(gf, row = 2, col = 1, grob = legend_body)
			} else {
				gf = placeGrob(gf, row = 1, col = 1, grob = textGrob(title, unit(0.5, "npc"), unit(1, "npc"), just = c("top"), gp = title_gp))
				gf = placeGrob(gf, row = 2, col = 1, grob = legend_body)
			}
		}
	} else if(title_position %in% c("leftcenter", "lefttop")) {
		if(convertWidth(title_height, "mm", valueOnly = TRUE) > convertWidth(legend_height, "mm", valueOnly = TRUE) && title_position == "lefttop") {
			total_height = title_height
			empty_height = total_height - legend_height
			gf = frameGrob(layout = grid.layout(nrow = 2, ncol = 2, 
				widths = unit.c(title_width + unit(1.5, "mm"), legend_width),
				heights = unit.c(legend_height, empty_height)))

			gf = placeGrob(gf, row = 1:2, col = 1, grob = textGrob(title, unit(1, "npc") - unit(1.5, "mm"), unit(1, "npc"), just = c("right", "top"), gp = title_gp))
			gf = placeGrob(gf, row = 1, col = 1, grob = legend_body)
		} else {
			total_height = max(unit.c(title_height, legend_height))
			gf = frameGrob(layout = grid.layout(nrow = 1, ncol = 2, 
				widths = unit.c(title_width + unit(1.5, "mm"), legend_width),
				heights = total_height))
			
			if(title_position == "lefttop") {
				gf = placeGrob(gf, row = 1, col = 1, grob = textGrob(title, unit(1, "npc") - unit(1.5, "mm"), unit(1, "npc"), just = c("right", "top"), gp = title_gp))
				gf = placeGrob(gf, row = 1, col = 2, grob = legend_body)
			} else {
				gf = placeGrob(gf, row = 1, col = 1, grob = textGrob(title, unit(1, "npc") - unit(1.5, "mm"), unit(0.5, "npc"), just = c("right"), gp = title_gp))
				gf = placeGrob(gf, row = 1, col = 2, grob = legend_body)
			}
		}
	}
	return(gf)
}

discrete_legend_body = function(at, labels = at, nrow = NULL, ncol = 1,
	grid_height = unit(4, "mm"), grid_width = unit(4, "mm"), gap = unit(2, "mm"),
	labels_gp = gpar(fontsize = 10),
	border = "white", background = "#EEEEEE",
	type = "grid", legend_gp = gpar(), 
	pch = 16, size = unit(2, "mm")) {

	n_labels = length(labels)
	if(is.null(nrow)) {
		nrow = ceiling(n_labels / ncol)
	} else {
		ncol = ceiling(n_labels / nrow)
	}

	labels_padding_left = unit(1, "mm")
	
	labels_max_width = NULL
	for(i in 1:ncol) {
		index = seq(nrow*(i-1)+1, min(c(nrow*i, n_labels)))
		if(i == 1) {
			labels_max_width = max(do.call("unit.c", lapply(labels[index], function(x) {
					g = grobWidth(textGrob(x, gp = labels_gp))
					if(i < ncol) {
						g = g + gap
					}
					g
				})))
		} else {
			labels_max_width = unit.c(labels_max_width, max(do.call("unit.c", lapply(labels[index], function(x) {
					g = grobWidth(textGrob(x, gp = labels_gp))
					if(i < ncol) {
						g = g + gap
					}
					g
				}))))
		}
	}

	gf = frameGrob(layout = grid.layout(nrow = 1, ncol = 2*ncol, 
		widths = do.call("unit.c", lapply(1:ncol, function(i) {
				unit.c(grid_width + labels_padding_left, labels_max_width[i])
		})),
		heights = nrow*(grid_height)))

	legend_gp = recycle_gp(legend_gp, n_labels)

	# legend grid
	for(i in 1:ncol) {
		index = seq(nrow*(i-1)+1, min(c(nrow*i, n_labels)))
		ni = length(index)
		x = unit(rep(0, ni), "npc")
		y = (0:(ni-1))*(grid_height)
		y = unit(1, "npc") - y

		# labels
		gf = placeGrob(gf, row = 1, col = 2*i, grob = textGrob(labels[index], x, y - grid_height*0.5, 
	 		just = c("left", "center"), gp = labels_gp))

		# grid
		sgd = subset_gp(legend_gp, index)
		sgd2 = gpar()
		if("grid" %in% type) {
			sgd2$fill = sgd$fill
		} else {
			sgd2$fill = background
		}
		sgd2$col = border

		gf = placeGrob(gf, row = 1, col = 2*i-1, grob = rectGrob(x, y, width = grid_width, height = grid_height, just = c("left", "top"),
				gp = sgd2))
		
		if(any(c("points", "p") %in% type)) {
			if(length(pch) == 1) pch = rep(pch, n_labels)
			if(length(size) == 1) size = rep(size, n_labels)
			gf = placeGrob(gf, row = 1, col = 2*i-1, grob = pointsGrob(x+grid_width*0.5, y-grid_height*0.5, pch = pch[index], size = size[index], gp = subset_gp(legend_gp, index)))
		}
		if(any(c("lines", "l") %in% type)) {
			gf = placeGrob(gf, row = 1, col = 2*i-1, grob = segmentsGrob(x+unit(0.5, "mm"), y-grid_height*0.5, x+grid_width - unit(0.5, "mm"), y-grid_height*0.5, gp = subset_gp(legend_gp, index)))
		}
	}
	return(gf)
}

vertical_continuous_legend_body = function(at, labels = at, col_fun,
	grid_height = unit(4, "mm"), grid_width = unit(4, "mm"),
	legend_height = NULL,
	labels_gp = gpar(fontsize = 10),
	border = NULL) {

	od = order(at)
	at = at[od]
	labels = labels[od]

	n_labels = length(labels)
	labels_max_width = max(do.call("unit.c", lapply(labels, function(x) {
			grobWidth(textGrob(x, gp = labels_gp))
		})))

	labels_padding_left = unit(1, "mm")
	
	min_legend_height = length(at)*(grid_height)
	if(is.null(legend_height)) legend_height = min_legend_height
	if(convertHeight(legend_height, "mm", valueOnly = TRUE) < convertHeight(min_legend_height, "mm", valueOnly = TRUE)) {
		legend_height = min_legend_height
	}

	gf = frameGrob(layout = grid.layout(nrow = 1, ncol = 2, 
		widths = unit.c(grid_width + labels_padding_left, labels_max_width),
		heights = legend_height))

	# legend grid
	labels_height = grobHeight(textGrob("foo", gp = labels_gp))
	x = unit(rep(0, n_labels), "npc")
	#y = seq(0, 1, length = n_labels) * (unit(1, "npc") - labels_height) + labels_height*0.5
	offset = labels_height*0.5
	k = length(at)
	ymin = offset
	ymax = unit(1, "npc")-offset
	y = (at - at[1])/(at[k] - at[1])*(ymax - ymin) + ymin
	gf = placeGrob(gf, row = 1, col = 2, grob = textGrob(labels, x, y, just = c("left", "center"), gp = labels_gp))

	at2 = unlist(lapply(seq_len(n_labels - 1), function(i) {
		x = seq(at[i], at[i+1], length = round((at[i+1]-at[i])/(at[k]-at[1])*100))
		x = x[-length(x)]
	}))
	at2 = c(at2, at[length(at)])
	colors = col_fun(at2)
	x2 = unit(rep(0, length(colors)), "npc")
	y2 = seq(0, 1, length = length(colors)+1)
	y2 = y2[-length(y2)] * unit(1, "npc")
	gf = placeGrob(gf, row = 1, col = 1, grob = rectGrob(x2, rev(y2), width = grid_width, height = (unit(1, "npc"))*(1/length(colors)), just = c("left", "center"),
			gp = gpar(col = rev(colors), fill = rev(colors))))
	gf = placeGrob(gf, row = 1, col = 1, grob = segmentsGrob(unit(0, "npc"), y, unit(0.8, "mm"), y, gp = gpar(col = ifelse(is.null(border), "white", border))))
	gf = placeGrob(gf, row = 1, col = 1, grob = segmentsGrob(grid_width, y, grid_width - unit(0.8, "mm"), y, gp = gpar(col = ifelse(is.null(border), "white", border))))
	
	if(!is.null(border)) {
		gf = placeGrob(gf, row = 1, col = 1, grob = rectGrob(width = grid_width, height = legend_height, x = unit(0, "npc"), just = "left", gp = gpar(col = border, fill = NA)))
	}

	return(gf)
}


horizontal_continuous_legend_body = function(at, labels = at, col_fun,
	grid_height = unit(4, "mm"), grid_width = unit(4, "mm"),
	legend_width = NULL,
	labels_gp = gpar(fontsize = 10),
	border = NULL) {

	od = order(at)
	at = at[od]
	labels = labels[od]
	k = length(at)

	n_labels = length(labels)
	labels_width = do.call("unit.c", lapply(labels, function(x) {
			grobWidth(textGrob(x, gp = labels_gp))
		}))
	labels_max_height = max(do.call("unit.c", lapply(labels, function(x) {
			grobHeight(textGrob(x, gp = labels_gp))
		})))

	labels_padding_top = unit(1, "mm")
	
	min_legend_width = sum(labels_width)*1.5
	if(is.null(legend_width)) legend_width = min_legend_width
	# if(convertWidth(legend_width, "mm", valueOnly = TRUE) < convertWidth(min_legend_width, "mm", valueOnly = TRUE)) {
	# 	legend_width = min_legend_width
	# }

	gf = frameGrob(layout = grid.layout(nrow = 2, ncol = 1, 
		widths = legend_width,
		heights = unit.c(grid_height + labels_padding_top, labels_max_height)))

	# legend grid
	offset = max(labels_width[c(1, k)])*0.5
	xmin = offset
	xmax = unit(1, "npc")-offset
	x = (at - at[1])/(at[k] - at[1])*(xmax - xmin)+ xmin
	gf = placeGrob(gf, row = 2, col = 1, grob = textGrob(labels, x, unit(0, "npc"), just = "bottom", gp = labels_gp))

	at2 = unlist(lapply(seq_len(n_labels - 1), function(i) {
		x = seq(at[i], at[i+1], length = round((at[i+1]-at[i])/(at[k]-at[1])*100))
		x = x[-length(x)]
	}))
	at2 = c(at2, at[length(at)])
	colors = col_fun(at2)
	y2 = unit(rep(1, length(colors)), "npc")
	x2 = seq(0, 1, length = length(colors)+1)
	x2 = x2[-length(x2)] * unit(1, "npc")
	gf = placeGrob(gf, row = 1, col = 1, grob = rectGrob(x2, y2, height = grid_height, width = (unit(1, "npc"))*(1/length(colors)), just = "top",
			gp = gpar(col = colors, fill = colors)))
	gf = placeGrob(gf, row = 1, col = 1, grob = segmentsGrob(x, labels_padding_top, x, labels_padding_top + unit(0.8, "mm"), gp = gpar(col = ifelse(is.null(border), "white", border))))
	gf = placeGrob(gf, row = 1, col = 1, grob = segmentsGrob(x, grid_height + labels_padding_top - unit(0.8, "mm"), x, grid_height + labels_padding_top, gp = gpar(col = ifelse(is.null(border), "white", border))))

	if(!is.null(border)) {
		gf = placeGrob(gf, row = 1, col = 1, grob = rectGrob(width = legend_width, height = grid_height, y = unit(1, "npc"), just = "top", gp = gpar(col = border, fill = NA)))
	}

	return(gf)
}