


Legend = function(...) {
	legend_body
}

discrete_legend_body = function(at, labels = at, nrow, ncol = 1, gp = gpar(),
	grid_height = unit(4, "mm"), grid_width = unit(4, "mm"), gap = unit(2, "mm"),
	labels_gp = gpar(fontsize = 10),
	border = "white", background = "#EEEEEE",
	type = "grid", legend_gp = gpar(), 
	pch = 16, size = unit(2, "mm")) {

	n_labels = length(labels)
	if(missing(nrow) && missing(ncol)) {
		nrow = n_labels
		ncol = 1
	} else if(missing(nrow)) {
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
					g = grobWidth(textGrob(x, gp = gp))
					if(i < ncol) {
						g = g + gap
					}
					g
				})))
		} else {
			labels_max_width = unit.c(labels_max_width, max(do.call("unit.c", lapply(labels[index], function(x) {
					g = grobWidth(textGrob(x, gp = gp))
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
		gf = packGrob(gf, row = 1, col = 2*i, grob = textGrob(labels[index], x, y - grid_height*0.5, 
	 		just = c("left", "center"), gp = labels_gp), width = labels_max_width[i], force.width = TRUE)

		# grid
		sgd = subset_gp(legend_gp, index)
		sgd2 = gpar()
		if("grid" %in% type) {
			sgd2$fill = sgd$fill
		} else {
			sgd2$fill = background
		}
		sgd2$col = border

		gf = packGrob(gf, row = 1, col = 2*i-1, grob = rectGrob(x, y, width = grid_width, height = grid_height, just = c("left", "top"),
				gp = sgd2))
		
		if(any(c("points", "p") %in% type)) {
			if(length(pch) == 1) pch = rep(pch, n_labels)
			if(length(size) == 1) size = rep(size, n_labels)
			gf = packGrob(gf, row = 1, col = 2*i-1, grob = pointsGrob(x+grid_width*0.5, y-grid_height*0.5, pch = pch[index], size = size[index], gp = subset_gp(legend_gp, index)))
		}
		if(any(c("lines", "l") %in% type)) {
			gf = packGrob(gf, row = 1, col = 2*i-1, grob = segmentsGrob(x+unit(0.5, "mm"), y-grid_height*0.5, x+grid_width - unit(0.5, "mm"), y-grid_height*0.5, gp = subset_gp(legend_gp, index)))
		}
	}
	return(gf)
}

vertical_continuous_legend_body = function(at, labels = at, col_fun,
	grid_height = unit(4, "mm"), grid_width = unit(4, "mm"),
	legend_height = min_legend_height,
	labels_gp = gpar(fontsize = 10),
	border = "white") {

	od = order(at)
	at = at[od]
	labels = labels[od]

	n_labels = length(labels)
	labels_max_width = max(do.call("unit.c", lapply(labels, function(x) {
			grobWidth(textGrob(x, gp = labels_gp))
		})))

	labels_padding_left = unit(1, "mm")
	
	min_legend_height = length(at)*(grid_height)
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
	gf = packGrob(gf, row = 1, col = 2, grob = textGrob(labels, x, y, just = c("left", "center"), gp = labels_gp), 
		width = labels_max_width, force.width = TRUE, height = legend_height, force.height = TRUE)

	at2 = unlist(lapply(seq_len(n_labels - 1), function(i) {
		x = seq(at[i], at[i+1], length = round((at[i+1]-at[i])/(at[k]-at[1])*100))
		x = x[-length(x)]
	}))
	at2 = c(at2, at[length(at)])
	colors = col_fun(at2)
	x2 = unit(rep(0, length(colors)), "npc")
	y2 = seq(0, 1, length = length(colors)+1)
	y2 = y2[-length(y2)] * unit(1, "npc")
	gf = packGrob(gf, row = 1, col = 1, grob = rectGrob(x2, rev(y2), width = grid_width, height = (unit(1, "npc"))*(1/length(colors)), just = c("left", "center"),
			gp = gpar(col = rev(colors), fill = rev(colors))), height = legend_height, force.height = TRUE)
	gf = packGrob(gf, row = 1, col = 1, grob = segmentsGrob(unit(0, "npc"), y, unit(0.8, "mm"), y, gp = gpar(col = border)), 
		    height = legend_height, force.height = TRUE)
	gf = packGrob(gf, row = 1, col = 1, grob = segmentsGrob(grid_width, y, grid_width - unit(0.8, "mm"), y, gp = gpar(col = border)), 
		    height = legend_height, force.height = TRUE)
	if(!missing(border)) {
		gf = packGrob(gf, row = 1, col = 1, grob = rectGrob(width = grid_width, height = legend_height, x = unit(0, "npc"), just = "left", gp = gpar(col = border, fill = NA)), 
		    height = legend_height, force.height = TRUE)
	}

	return(gf)
}


horizontal_continuous_legend_body = function(at, labels = at, col_fun,
	grid_height = unit(4, "mm"), grid_width = unit(4, "mm"),
	legend_width = min_legend_width,
	labels_gp = gpar(fontsize = 10),
	border = "white") {

	if(missing(at)) {
		at = attr(col_fun, "breaks")
	}

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
	gf = packGrob(gf, row = 2, col = 1, grob = textGrob(labels, x, unit(0, "npc"), just = "bottom", gp = labels_gp), 
		width = legend_width, force.width = TRUE, height = labels_max_height, force.height = TRUE)

	at2 = unlist(lapply(seq_len(n_labels - 1), function(i) {
		x = seq(at[i], at[i+1], length = round((at[i+1]-at[i])/(at[k]-at[1])*100))
		x = x[-length(x)]
	}))
	at2 = c(at2, at[length(at)])
	colors = col_fun(at2)
	y2 = unit(rep(1, length(colors)), "npc")
	x2 = seq(0, 1, length = length(colors)+1)
	x2 = x2[-length(x2)] * unit(1, "npc")
	gf = packGrob(gf, row = 1, col = 1, grob = rectGrob(x2, y2, height = grid_height, width = (unit(1, "npc"))*(1/length(colors)), just = "top",
			gp = gpar(col = colors, fill = colors)), height = grid_height + labels_padding_top, width = legend_width, force.height = TRUE)
	gf = packGrob(gf, row = 1, col = 1, grob = segmentsGrob(x, labels_padding_top, x, labels_padding_top + unit(0.8, "mm"), gp = gpar(col = border)), 
			height = grid_height + labels_padding_top, width = legend_width, force.width = TRUE)
	gf = packGrob(gf, row = 1, col = 1, grob = segmentsGrob(x, grid_height + labels_padding_top - unit(0.8, "mm"), x, grid_height + labels_padding_top, gp = gpar(col = border)), 
		    height = grid_height + labels_padding_top, width = legend_width, force.width = TRUE)

	if(!missing(border)) {
		gf = packGrob(gf, row = 1, col = 1, grob = rectGrob(width = legend_width, height = grid_height, y = unit(1, "npc"), just = "top", gp = gpar(col = border, fill = NA)), 
		    height = grid_height + labels_padding_top, width = legend_width, force.width = TRUE)
	}

	return(gf)
}