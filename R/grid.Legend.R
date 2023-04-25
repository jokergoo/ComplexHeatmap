
# == title
# The Class for Legends
#
# == details
# This is a very simple class for legends that it only has one slot which is the real `grid::grob` of the legends. 
#
# Construct a single legend by `Legend` and a group of legends by `packLegend`.
# 
# == example
# lgd = Legend(at = 1:4)
# lgd
# lgd@grob
Legends = setClass("Legends",
    slots = list(
    	name = "ANY",
        grob = "ANY",
        type = "character",
        n = "numeric",
        multiple = "numeric",
        direction = "character"
    )
)

# == title
# Constructor method for Legends class
#
# == param
# -... arguments.
#
# == details
# There is no public constructor method for the `Legends-class`.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
Legends = function(...) {
    new("Legends", ...)
}

# == title
# Make a Single Legend
#
# == param
# -at Breaks of the legend. The values can be either numeric or character. If it is not specified,
#     the values of ``labels`` are taken as labels.
# -labels Labels corresponding to ``at``. If it is not specified, the values of ``at`` are taken as labels.
# -col_fun A color mapping function which is used to make a continuous legend. Use `circlize::colorRamp2` to
#     generate the color mapping function. If ``at`` is missing, the breaks recorded in the color mapping function
#      are used for ``at``.
# -name Name of the legend, internally used.
# -grob The legend body can be specified by a pre-constructed `grid::grob` object.
# -break_dist A zooming factor to control relative distance of two neighbouring break values.The length
#     of it should be ``length(at) - 1`` or a scalar. 
# -nrow For legend which is represented as grids, ``nrow`` controls number of rows of the grids if the grids
#      are arranged into multiple rows.
# -ncol Similar as ``nrow``, ``ncol`` controls number of columns of the grids if the grids
#      are arranged into multiple columns. Note at a same time only one of ``nrow`` and ``ncol`` can be specified.
# -by_row Are the legend grids arranged by rows or by columns?
# -grid_height The height of legend grid. It can also control the height of the continuous legend if it is horizontal.
# -grid_width The width of legend grid. It can also control the width of the continuous legend if it is vertical.
# -tick_length Length of the ticks on the continuous legends. Value should be a `grid::unit` object.
# -gap If legend grids are put into multiple rows or columns, this controls the gap between neighbouring rows or columns, measured as a `grid::unit` object.
# -column_gap The same as ``gap``.
# -row_gap Space between legend rows.
# -labels_gp Graphic parameters for labels.
# -labels_rot Text rotation for labels. It should only be used for horizontal continuous legend.
# -border Color of legend grid borders. It also works for the ticks in the continuous legend.
# -background Background colors for the grids. It is used when points and lines are the legend graphics.
# -type Type of legends. The value can be one of ``grid``, ``points``, ``lines`` and ``boxplot``.
# -graphics Self-defined graphics for legends. The value should be a list of functions.
#           Each function should accept four argumets: ``x`` and ``y``: positions of the legend grid (center point), ``w`` and ``h``: width and height
#           of the legend grid.
# -legend_gp Graphic parameters for the legend grids. You should control the filled color of the legend grids by ``gpar(fill = ...)``.
# -pch Type of points if points are used as legend. Note you can use single-letter as pch, e.g. ``pch = 'A'``.
#      There are three additional integers that are valid for ``pch``: 26 and 27 for single diagonal lines and 28 for double diagonal lines.
# -size Size of points.
# -legend_height Height of the whole legend body. It is only used for vertical continous legend.
# -legend_width Width of the whole legend body. It is only used for horizontal continous legend.
# -direction Direction of the legend, vertical or horizontal?
# -title Title of the legend.
# -title_gp Graphic parameters of the title.
# -title_position Position of title relative to the legend. ``topleft``, ``topcenter``, ``leftcenter-rot``
#     and ``lefttop-rot`` are only for vertical legend and ``leftcenter``, ``lefttop`` are only for 
#     horizontal legend.
# -title_gap Gap between title and the legend body.
#
# == details
# Most of the argument can also be set in ``heatmap_legend_param`` argument in `Heatmap` or ``annotation_legend_param``
# argument in `HeatmapAnnotation` to configure legend styles for heatmap and annotations.
#
# == seealso
# `packLegend` packs multiple legends into one `Legends-class` object.
#
# See examples of configuring legends: https://jokergoo.github.io/ComplexHeatmap-reference/book/legends.html
#
# == value
# A `Legends-class` object.
#
# == example
# lgd = Legend(labels = month.name[1:6], title = "foo", legend_gp = gpar(fill = 1:6))
# draw(lgd, test = "add labels and title")
#
# require(circlize)
# col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
# lgd = Legend(col_fun = col_fun, title = "foo")
# draw(lgd, test = "only col_fun")
#
# col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
# lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.1, 0.15, 0.5, 0.9, 0.95, 1))
# draw(lgd, test = "unequal interval breaks")
Legend = function(at, labels = at, col_fun, name = NULL, grob = NULL,
	break_dist = NULL, nrow = NULL, ncol = 1, by_row = FALSE,
	grid_height = unit(4, "mm"), 
	grid_width = unit(4, "mm"), tick_length = unit(0.8, "mm"),
	gap = unit(2, "mm"), column_gap = gap, row_gap = unit(0, "mm"),
	labels_gp = gpar(fontsize = 10), labels_rot = 0,
	border = NULL, background = "#EEEEEE",
	type = "grid", graphics = NULL, legend_gp = gpar(),
	pch = 16, size = unit(2, "mm"),
	legend_height = NULL, legend_width = NULL,
	direction = c("vertical", "horizontal"),
	title = "", title_gp = gpar(fontsize = 10, fontface = "bold"),
	title_position = c("topleft", "topcenter", "leftcenter", "lefttop", "leftcenter-rot", "lefttop-rot"),
	title_gap = unit(2, "mm")) {

	dev.null()
	on.exit(dev.off2())

	if(missing(at) && !missing(labels)) {
		at = seq_along(labels)
	}

	if(!"fontsize" %in% names(labels_gp)) {
		labels_gp$fontsize = 10
	}

	labels_gp = check_gp(labels_gp)
	title_gp = check_gp(title_gp)

	# odevlist = dev.list()
	direction = match.arg(direction)[1]
	title_position = match.arg(title_position)[1]
	title_padding = title_gap
	if(!is.null(grob)) {
		legend_body = legend_body_from_grob(grob)
	} else if(missing(col_fun)) {
		if(is.null(border)) border = "white"
		legend_body = discrete_legend_body(at = at, labels = labels, nrow = nrow, ncol = ncol,
			grid_height = grid_height, grid_width = grid_width, gap = gap, row_gap = row_gap, column_gap = column_gap, labels_gp = labels_gp,
			border = border, background = background, type = type, graphics = graphics, legend_gp = legend_gp,
			pch = pch, size = size, by_row = by_row)
	} else {
		if(!missing(col_fun) && missing(at)) {
			breaks = attr(col_fun, "breaks")
			if(is.null(breaks)) {
				stop_wrap("You should provide `at` for color mapping function\n")
			}
			
			if(is.null(break_dist)) {
				le1 = grid.pretty(range(breaks))
				le2 = pretty(breaks, n = 3)
				if(abs(length(le1) - 5) < abs(length(le2) - 5)) {
					at = le1
				} else {
					at = le2
				}
			} else {
				at = breaks
			}
		}

		if(direction == "vertical") {
			legend_body = vertical_continuous_legend_body(at = at, labels = labels, col_fun = col_fun, break_dist = break_dist,
				grid_height = grid_height, grid_width = grid_width, tick_length = tick_length, legend_height = legend_height,
				labels_gp = labels_gp, border = border, legend_gp = legend_gp)
		} else {
			legend_extension = unit(0, "mm")
			if(title_position == "lefttop") {
				title_width = convertWidth(grobWidth(textGrob(title, gp = title_gp)), "mm")
				title_height = convertHeight(grobHeight(textGrob(title, gp = title_gp)), "mm")
				if(unit_to_numeric(title_height[1]) <= unit_to_numeric(grid_height[1])) {
					legend_extension = title_width + title_padding
				}
			}
			legend_body = horizontal_continuous_legend_body(at = at, labels = labels, col_fun = col_fun, break_dist = break_dist,
				grid_height = grid_height, grid_width = grid_width, tick_length = tick_length, legend_width = legend_width,
				labels_gp = labels_gp, labels_rot = labels_rot, border = border, legend_gp = legend_gp, legend_extension = legend_extension)
		}
	}

	if(is.null(title)) {
		object = new("Legends")
		object@grob = legend_body
		object@type = "single_legend_no_title"
		object@n = 1
		object@multiple = 1
		object@direction = "vertical"
		return(object)
	}
	if(!inherits(title, c("expression", "call"))) {
		if(grepl("^\\s+$", title)) title = ""
		if(title == "") {
			object = new("Legends")
			object@grob = legend_body
			object@type = "single_legend_no_title"
			object@n = 1
			object@multiple = 1
			object@direction = "vertical"
			return(object)
		}
	}

	title_grob = textGrob(title, gp = title_gp)
	title_height = convertHeight(grobHeight(title_grob), "mm")
	title_width = convertWidth(grobWidth(title_grob), "mm")

	legend_width = convertWidth(grobWidth(legend_body), "mm")
	legend_height = convertHeight(grobHeight(legend_body), "mm")

	# at the top level, create a global viewport
	if(!missing(col_fun)) {
		if(direction == "vertical") {
			if(title_position %in% c("leftcenter", "lefttop")) {
				stop_wrap("'topleft', 'topcenter', 'leftcenter-rot' and 'lefttop-rot' are only allowd for vertical continuous legend")
			}
		}
		if(direction == "horizontal") {
			if(title_position %in% c('leftcenter-rot', 'lefttop-rot')) {
				stop_wrap("'topleft', 'topcenter', 'lefttop' and 'leftcenter' are only allowd for horizontal continuous legend")
			}
		}
	}

	if(title_position %in% c("topleft", "topcenter")) {
		if(title_width > legend_width && title_position == "topleft") {
			total_width = title_width
			total_height = title_height + title_padding + legend_height
			
			title_x = unit(0, "npc")
			title_just = c("left", "top")
		} else {
			total_width = legend_width
			total_height = title_height + title_padding + legend_height

			if(title_position == "topleft") {
				title_x = unit(0, "npc")
				title_just = c("left", "top")
			} else {
				title_x = unit(0.5, "npc")
				title_just = "top"
			}
		}
		gf = grobTree(
			textGrob(title, x = title_x, y = unit(1, "npc"), just = title_just, gp = title_gp),
			edit_vp_in_legend_grob(legend_body, x = unit(0, "npc"), y = unit(0, "npc"), valid.just = c(0, 0)),
			vp = viewport(width = total_width, height = total_height),
			cl = "legend"
		)
		attr(gf, "width") = total_width
		attr(gf, "height") = total_height
		
	} else if(title_position %in% c("leftcenter", "lefttop")) {
		if(title_height > legend_height && title_position == "lefttop") {
			total_width = title_width + title_padding + legend_width
			total_height = title_height
			
			title_y = unit(1, "npc")
			title_just = c("left", "top")
		} else {
			total_width = title_width + title_padding + legend_width
			total_height = legend_height
			if(title_position == "lefttop") {
				if(missing(col_fun)) {
					title_y = unit(1, "npc") - convertY(grobY(legend_body$children[[1]], 270), "mm")
				} else {
					if(direction == "horizontal") {
						title_y = unit(1, "npc") - convertHeight(grobHeight(legend_body$children[[2]])*0.5 - grobHeight(textGrob(title, gp = title_gp))*0.5, "mm")
					} else {
						title_y = unit(1, "npc")
					}
				}
				title_just = c("left", "top")
			} else {
				title_y = unit(0.5, "npc")
				title_just = "left"
			}
		}
		gf = grobTree(
			textGrob(title, x = unit(0, "npc"), y = title_y, just = title_just, gp = title_gp),
			edit_vp_in_legend_grob(legend_body, x = unit(1, "npc"), y = unit(1, "npc"), 
				valid.just = c(1, 1)),
			vp = viewport(width = total_width, height = total_height),
			cl = "legend"
		)
		attr(gf, "width") = total_width
		attr(gf, "height") = total_height
	} else if(title_position %in% c("leftcenter-rot", "lefttop-rot")) {
		if(title_width > legend_height && title_position == "lefttop-rot") {
			total_width = title_height + title_padding + legend_width
			total_height = title_width
			
			title_y = unit(1, "npc")
			title_just = c("right", "top")
		} else {
			total_width = title_height + title_padding + legend_width
			total_height = legend_height
			if(title_position == "lefttop-rot") {
				title_y = unit(1, "npc")
				title_just = c("right", "top")
			} else {
				title_y = unit(0.5, "npc")
				title_just = "top"
			}
		}
		gf = grobTree(
			textGrob(title, x = unit(0, "npc"), y = title_y, just = title_just, gp = title_gp, rot = 90),
			edit_vp_in_legend_grob(legend_body, x = unit(1, "npc"), y = unit(1, "npc"), 
				valid.just = c(1, 1)),
			vp = viewport(width = total_width, height = total_height, gp = gpar(fontsize = 10, lineheight = 0.8)),
			cl = "legend"
		)
		attr(gf, "width") = total_width
		attr(gf, "height") = total_height
	}

	object = new("Legends")
	object@grob = gf
	object@type = "single_legend"
	object@name = name
	object@n = 1
	object@multiple = 1
	object@direction = "vertical"
	return(object)
}

setMethod("show",
	signature = "Legends",
	definition = function(object) {
	if(object@type == "single_legend") {
		cat("A single legend\n")
	} else if(object@type == "single_legend_no_title") {
		cat("A single legend without title\n")
	} else {
		cat("A pack of", object@n, "legends\n")
	}
})

widthDetails.Legend = function(x) {
	attr(x, "width")
}

heightDetails.Legend = function(x) {
	attr(x, "height")
}

# grids are arranged by rows or columns
discrete_legend_body = function(at, labels = at, nrow = NULL, ncol = 1, by_row = TRUE,
	grid_height = unit(4, "mm"), grid_width = unit(4, "mm"), 
	gap = unit(2, "mm"), column_gap = gap, row_gap = unit(0, "mm"),
	labels_gp = gpar(fontsize = 10),
	border = "white", background = "#EEEEEE",
	type = "grid", graphics = NULL, legend_gp = gpar(),
	pch = 16, size = unit(2, "mm")) {

	n_labels = length(labels)
	if(is.null(nrow)) {
		nrow = ceiling(n_labels / ncol)
	} else {
		ncol = ceiling(n_labels / nrow)
	}
	if(length(at) == 1) {
		nrow = 1
		ncol = 1
	}
	ncol = ifelse(ncol > n_labels, n_labels, ncol)
	if(length(grid_height) == 1) grid_height = rep(grid_height, nrow)
	if(length(grid_width) == 1) grid_width = rep(grid_width, ncol)

	if(!is.null(graphics)) {
		if(length(graphics) != n_labels) {
			stop_wrap("Length of `graphics` should be the same as number of labels.")
		}
		if(!all(sapply(graphics, is.function))) {
			stop_wrap("`graphics` should be a list of functions.")
		}
	}
	if(length(graphics) == 0) graphics = NULL

	if(inherits(labels, "expression")) {
		labels_mat = matrix(c(labels, rep("__NA__", nrow*ncol - n_labels)), nrow = nrow, ncol = ncol, byrow = by_row)
		labels_are_expression = TRUE
	} else {
		labels_mat = matrix(c(labels, rep(NA, nrow*ncol - n_labels)), nrow = nrow, ncol = ncol, byrow = by_row)
		labels_are_expression = FALSE
	}
	index_mat = matrix(1:(nrow*ncol), nrow = nrow, ncol = ncol, byrow = by_row)
	if(labels_are_expression) {
		l_na = sapply(1:ncol(labels_mat), function(i) {
			x = labels_mat[, i]
			all(is.na.expression(x))
		})
	} else {
		l_na = apply(labels_mat, 2, function(x) all(is.na(x)))
	}
	if(any(l_na)) {
		message_wrap(qq("No legend element is put in the last @{sum(l_na)} column@{ifelse(sum(l_na) > 1, 's', '')} under `ncol = @{ncol}`, maybe you should set `by_row = TRUE`? Reset `ncol` to @{sum(!l_na)}."))
		ncol = sum(!l_na)
	}
	if(labels_are_expression) {
		l_na = sapply(1:nrow(labels_mat), function(i) {
			x = labels_mat[i, ]
			all(is.na.expression(x))
		})
	} else {
		l_na = apply(labels_mat, 1, function(x) all(is.na(x)))
	}
	if(any(l_na)) {
		message_wrap(qq("No legend element is put in the last @{sum(l_na)} row@{ifelse(sum(l_na) > 1, 's', '')} under `nrow = @{nrow}`, maybe you should set `by_row = FALSE`? Reset `nrow` to @{sum(!l_na)}."))
		nrow = sum(!l_na)
	}

	labels_padding_left = unit(1, "mm")
	## max width for each column in the legend
	labels_max_width = NULL
	for(i in 1:ncol) {
		if(i == 1) {
			labels_max_width = max(do.call("unit.c", lapply(labels_mat[, i], function(x) {
					if(labels_are_expression) {
						if(identical(as.character(x), "__NA__")) {
							x = ""
						}
					}
					grobWidth(textGrob(x, gp = labels_gp))
				})))
		} else {
			labels_max_width = unit.c(labels_max_width, max(do.call("unit.c", lapply(labels_mat[, i], function(x) {
					if(labels_are_expression) {
						if(identical(as.character(x), "__NA__")) {
							x = ""
						}
					}
					grobWidth(textGrob(x, gp = labels_gp))
				}))))
		}
	}
	labels_max_width = convertWidth(labels_max_width, "mm")

	row_height = NULL
	for(i in 1:nrow) {
		if(i == 1) {
			row_height = max(do.call("unit.c", lapply(labels_mat[i, ], function(x) {
					if(labels_are_expression) {
						if(identical(as.character(x), "__NA__")) {
							x = ""
						}
					}
					grobHeight(textGrob(x, gp = labels_gp))
				})))
		} else {
			row_height = unit.c(row_height, max(do.call("unit.c", lapply(labels_mat[i, ], function(x) {
					if(labels_are_expression) {
						if(identical(as.character(x), "__NA__")) {
							x = ""
						}
					}
					grobHeight(textGrob(x, gp = labels_gp))
				}))))
		}
	}
	row_height = convertWidth(row_height + unit(4, "points"), "mm") # 4pt is the margin of the text
	row_height_no_gap = row_height
	for(i in seq_along(row_height)) {
		row_height_no_gap[i]= row_height[i] = max(row_height[i], grid_height[i])
		if(i < length(row_height)) {
			row_height[i] = row_height[i] + row_gap
		}
	}

	row_height = convertHeight(row_height, "mm")

	legend_gp = recycle_gp(legend_gp, n_labels)

	legend_body_width = sum(grid_width) + labels_padding_left*ncol + sum(labels_max_width) + column_gap*(ncol-1)
	legend_body_height = sum(row_height)
	legend_body_width = convertWidth(legend_body_width, "mm")
	legend_body_height = convertHeight(legend_body_height, "mm")

	# legend grid
	gl = list()
	previous_x = unit(0, "mm")
	for(i in 1:ncol) {
		if(labels_are_expression) {
			index = index_mat[, i][!is.na.expression(labels_mat[, i])]
		} else {
			index = index_mat[, i][!is.na(labels_mat[, i])]
		}
		ni = length(index)
		y = do.call("unit.c", lapply(1:ni, function(ind) {
			if(ind == 1) {
				unit(0, "points")
			} else {
				sum(row_height[1:(ind-1)])
			}
		}))
		y = legend_body_height - y # from top

		labels_x = previous_x + grid_width[i] + labels_padding_left
		labels_y = y - (row_height[1:ni] - row_gap)*0.5
		if(ni == nrow) labels_y[nrow] = y[nrow] - row_height[nrow]*0.5
		labels_x = convertWidth(labels_x, "mm")
		labels_y = convertHeight(labels_y, "mm")
		gl = c(gl, list(
			textGrob(labels[index], x = labels_x, y = labels_y, just = "left", gp = labels_gp)
		))

		######### graphics ############
		# grid
		sgd = subset_gp(legend_gp, index)
		sgd2 = gpar()
		if("grid" %in% type) {
			sgd2$fill = sgd$fill
		} else {
			sgd2$fill = background
		}
		if(identical(legend_gp$col, "asis")) {
			border = "asis"
		}
		if(identical(border, "asis")) {
			sgd2$col = sgd2$fill	
		} else {
			if(is.null(sgd2$col)) {
				sgd2$col = border
			}
		}

		grid_x = previous_x
		grid_y = y - (row_height[1:ni] - row_gap)*0.5
		if(ni == nrow) grid_y[nrow] = y[nrow] - row_height[nrow]*0.5
		grid_x = convertWidth(grid_x, "mm")
		grid_x = rep(grid_x, length(grid_y))
		grid_y = convertHeight(grid_y, "mm")

		if(is.null(graphics)) {

			gl = c(gl, list(
				rectGrob(x = grid_x, y = grid_y, width = grid_width[i], height = row_height_no_gap, gp = sgd2, just = "left")
			))

			if(any(c("points", "p") %in% type)) {
				if(length(pch) == 1) pch = rep(pch, n_labels)
				if(length(size) == 1) size = rep(size, n_labels)

				if(is.character(pch)) {
					gl = c(gl, list(
						textGrob(pch[index], x = grid_x + grid_width[i]*0.5, y = grid_y, gp = subset_gp(legend_gp, index))
					))
				} else {
					gl = c(gl, 
						.pointsGrob_as_a_list(x = grid_x + grid_width[i]*0.5, y = grid_y, pch = pch[index], 
							gp = subset_gp(legend_gp, index), size = size[index], width = grid_width[i], height = row_height_no_gap)
					)
				}
			}
			if(any(c("lines", "l") %in% type)) {
				gl = c(gl, list(
					segmentsGrob(x0 = grid_x, y0 = grid_y, 
						         x1 = grid_x + grid_width[i], y1 = grid_y,
						         gp = subset_gp(legend_gp, index))
				))
			}
			if(any(c("boxplot", "box") %in% type)) {
				gl = c(gl, list(
					segmentsGrob(x0 = grid_x + grid_width[i]*0.5, y0 = grid_y - row_height_no_gap*0.45, 
						         x1 = grid_x + grid_width[i]*0.5, y1 = grid_y + row_height_no_gap*0.45,
						         gp = subset_gp(legend_gp, index)),
					rectGrob(x = grid_x + grid_width[i]*0.5, y = grid_y, width = grid_width[i]*0.9, height = row_height_no_gap*0.5,
						     gp = subset_gp(legend_gp, index)),
					segmentsGrob(x0 = grid_x + grid_width[i]*0.05, y0 = grid_y, 
						         x1 = grid_x + grid_width[i]*0.95, y1 = grid_y,
						         gp = subset_gp(legend_gp, index))
				))
			}
		} else {
			fl = graphics[index]
			gb_lt = list()
			for(k in seq_along(fl)) {
				gb_lt[[k]] = grid.grabExpr({
						fl[[k]](x = grid_x[1] + grid_width[i]*0.5, y = grid_y[k], w = grid_width[i], h = row_height_no_gap[k])
					}, 
					width = convertWidth(grid_width[i], "inch", valueOnly = TRUE), 
					height = convertHeight(row_height_no_gap[k], "inch", valueOnly = TRUE)
				)
					
			}
			gl = c(gl, gb_lt)
		}

		previous_x = previous_x + grid_width[i] + labels_max_width[i] + labels_padding_left + column_gap
	}

	class(gl) = "gList"
	gt = gTree(children = gl, cl = "legend_body", vp = viewport(width = legend_body_width, height = legend_body_height))
	attr(gt, "height") = legend_body_height
	attr(gt, "width") = legend_body_width
	return(gt)
}

vertical_continuous_legend_body = function(at, labels = at, col_fun,
	break_dist = NULL, grid_height = unit(4, "mm"), grid_width = unit(4, "mm"),
	legend_height = NULL, tick_length = unit(0.8, "mm"),
	labels_gp = gpar(fontsize = 10),
	border = NULL, legend_gp = gpar()) {

	n = length(at)
	breaks = attr(col_fun, "breaks")
	if(identical(order(at), seq(n, 1))) {
		breaks = rev(breaks)
		break_dist = rev(break_dist)
	} else {
		od = order(at)
		if(!is.null(break_dist)) {
			if(!identical(od, 1:n)) {
				stop_wrap("`at` should be sorted if `break_dist` is set.")
			}
		}
		at = at[od]
		labels = labels[od]
	}

	if(!is.null(break_dist)) {
		if(is.null(breaks)) {
			stop_wrap("`col_fun` must have a 'breaks' attribute if `break_dist` is set.")
		}
		if(length(break_dist) == 1) {
			break_dist = rep(break_dist, n - 1)
		}
		if(length(break_dist) != n - 1) {
			stop_wrap("Length of `break_dist` should be `length(at) - 1`.")
		}
		# this function is not vectorized
		generate_map_fun = function(breaks) {
			nb = length(breaks)
			y = c(0, break_dist)
			y = cumsum(y)/sum(y)
			function(x) {
				if(x <= y[1]) {
					return(breaks[1])
				} else if(x >= y[nb]) {
					return(breaks[nb])
				} else {
					for(i in 2:nb) {
						if(x == y[i]) {
							return(breaks[i])
						}
						if(x < y[i]) {
							return( (x - y[i-1])/(y[i] - y[i-1])*(breaks[i] - breaks[i-1]) + breaks[i-1] )
						}
					}
				}
			}
		}
		col_fun2 = col_fun
		map_fun = generate_map_fun(sort(at))
		col_fun = function(x) {
			x2 = sapply(x, map_fun)
			col_fun2(x2)
		}
		y = c(0, break_dist)
		y = cumsum(y)/sum(y)

		attr(col_fun, "breaks") = y
		if(at[1] <= at[n]) {
			at = y
		} else {
			at = rev(y)
		}
	}
	
	n_labels = length(labels)
	labels_max_width = max_text_width(labels, gp = labels_gp)

	labels_padding_left = unit(1, "mm")

	labels_gp = recycle_gp(labels_gp, n)
	min_legend_height = unit(sum(sapply(seq_len(n), function(i) {
		convertHeight(grobHeight(textGrob(labels[i], gp = subset_gp(labels_gp, i))), "mm", valueOnly = TRUE)
	})), "mm") + unit(6 * (n-1), "points")

	if(!is.null(legend_height)) {
		if(convertHeight(legend_height, "mm", valueOnly = TRUE) < convertHeight(min_legend_height, "mm", valueOnly = TRUE)) {
			warning_wrap("`legend_height` you specified is too small, use the default minimal height.")
			legend_height = min_legend_height
		}
	} else {
		legend_height = max(min_legend_height, unit(21.1, "mm")) # five-label legend
	}
	legend_height = convertHeight(legend_height, "mm")

	segment_col = border
	if(is_diff_equal(at)) {
		at_diff_is_equal = TRUE
	} else {
		at_diff_is_equal = FALSE
		labels_padding_left = unit(4, "mm")
		# oborder = border
		# if(is.null(border)) segment_col = "black"
	}

	legend_body_width = grid_width + labels_padding_left + labels_max_width
	legend_body_height = legend_height
	legend_body_width = convertWidth(legend_body_width, "mm")
	legend_body_height = convertHeight(legend_body_height, "mm")

	gl = list()

	# labels
	labels_line_height = convertHeight(grobHeight(textGrob("fooy", gp = labels_gp)), "mm")
	x = unit(rep(0, n_labels), "npc")
	offset = unit(2, "points") # space from the first break to the bottom and the last break to the top
	k = length(at)
	ymin = offset
	ymax = legend_height-offset
	y = (at - at[1])/(at[k] - at[1])*(ymax - ymin) + ymin
	y = convertY(y, "mm")
	labels_x = grid_width + labels_padding_left
	labels_y = convertHeight(y, "mm")
	
	if(!at_diff_is_equal) {
		labels_line_height = do.call("unit.c", lapply(labels, 
			function(x) grobHeight(textGrob(x, gp = labels_gp)) + unit(4, "pt")))
		y_top = labels_y + labels_line_height*0.5
		y_bottom = labels_y - labels_line_height*0.5
		y_top = convertY(y_top, "mm", valueOnly = TRUE)
		y_bottom = convertY(y_bottom, "mm", valueOnly = TRUE)
		yrange = convertY(unit.c(offset - labels_line_height[1]*0.5, 
			                     legend_body_height - offset + labels_line_height[length(labels_line_height)]*0.5), "mm", valueOnly = TRUE)
		new_pos = smartAlign(y_bottom, y_top, yrange)
		y2 = (new_pos[, 1] + new_pos[, 2])/2
		y2 = unit(y2, "mm")
		labels_y = y2
	}

	if(all(abs(as.numeric(labels_y) - as.numeric(y)) < 1e-4)) {
		adjust_text_pos = FALSE
		labels_padding_left = unit(1, "mm")
		labels_x = grid_width + labels_padding_left
		legend_body_width = grid_width + labels_padding_left + labels_max_width
		legend_body_width = convertWidth(legend_body_width, "mm")
		# if(is.null(oborder)) segment_col = NULL
	} else {
		adjust_text_pos = TRUE
	}

	gl = c(gl, list(
		textGrob(labels, x = labels_x, y = labels_y, just = "left", gp = labels_gp)
	))
	
	## colors
	if(!at_diff_is_equal) {
		at = seq(at[1], at[length(at)], length.out = n_labels)
	}
	at2 = unlist(lapply(seq_len(n_labels - 1), function(i) {
		x = seq(at[i], at[i+1], length.out = max(10, round((at[i+1]-at[i])/(at[k]-at[1])*100)))
		x = x[-length(x)]
	}))
	at2 = c(at2, at[length(at)])

	colors = col_fun(at2)
	x2 = unit(rep(0, length(colors)), "npc")
	y2 = seq(0, 1, length.out = length(colors)+1); y2 = y2[-length(y2)]
	y2 = y2 * (legend_body_height - 2*offset) + offset
	y2 = y2 + (y2[2] - y2[1])*0.5
	hh = (legend_body_height - 2*offset)*(1/length(colors))
	x2 = unit.c(unit(0, "npc"), x2, unit(0, "npc"))
	y2 = unit.c(offset*0.5, y2, legend_body_height - offset*0.5)
	hh = unit.c(offset, rep(hh, length(y2)-2), offset)
	colors = c(colors[1], colors, colors[length(colors)])

	if(is.null(border)) {
		if(!is.null(legend_gp$col)) {
			border = legend_gp$col
		}
	}

	if(is.null(border)) {
		legend_gp$col = "white"
	} else {
		if(is.null(legend_gp$col)) {
			legend_gp$col = border
		}
	}
	gl = c(gl, list(
		rectGrob(x2, rev(y2), width = grid_width, height = hh, just = c("left", "center"),
			gp = gpar(col = rev(colors), fill = rev(colors))),
		segmentsGrob(unit(0, "npc"), y, tick_length, y, gp = legend_gp),
		segmentsGrob(grid_width, y, grid_width - tick_length, y, gp = legend_gp)
	))

	if(adjust_text_pos) {
		segment_x0 = grid_width
		segment_y0 = y
		segment_x1 = grid_width + labels_padding_left*(1/3)
		segment_y1 = y
		gl = c(gl, list(
			segmentsGrob(segment_x0, segment_y0, segment_x1, segment_y1)
		))
		segment_x0 = grid_width + labels_padding_left - unit(0.5, "mm")
		segment_y0 = labels_y
		segment_x1 = grid_width + labels_padding_left*(2/3)
		segment_y1 = labels_y
		gl = c(gl, list(
			segmentsGrob(segment_x0, segment_y0, segment_x1, segment_y1)
		))
		segment_x0 = grid_width + labels_padding_left*(1/3)
		segment_y0 = y
		segment_x1 = grid_width + labels_padding_left*(2/3)
		segment_y1 = labels_y
		gl = c(gl, list(
			segmentsGrob(segment_x0, segment_y0, segment_x1, segment_y1)
		))
	}

	if(!is.null(border)) {
		legend_gp$fill = "transparent"
		gl = c(gl, list(
			rectGrob(width = grid_width, height = legend_height, x = unit(0, "npc"), just = "left", gp = legend_gp)
		))
	}

	class(gl) = "gList"
	gt = gTree(children = gl, cl = "legend_body", vp = viewport(width = legend_body_width, height = legend_body_height))
	attr(gt, "height") = legend_body_height
	attr(gt, "width") = legend_body_width
	return(gt)
}

horizontal_continuous_legend_body = function(at, labels = at, col_fun,
	break_dist = NULL, grid_height = unit(4, "mm"), grid_width = unit(4, "mm"),
	legend_width = NULL, tick_length = unit(0.8, "mm"),
	labels_gp = gpar(fontsize = 10), labels_rot = 0,
	border = NULL, legend_gp = gpar(), legend_extension = unit(0, "mm")) {
		
	n = k = length(at)
	breaks = attr(col_fun, "breaks")
	if(identical(order(at), seq(k, 1))) {
		breaks = rev(breaks)
		break_dist = rev(break_dist)
	} else {
		od = order(at)
		if(!is.null(break_dist)) {
			if(!identical(od, 1:n)) {
				stop_wrap("`at` should be sorted if `break_dist` is set.")
			}
		}
		at = at[od]
		labels = labels[od]
	}

	if(!is.null(break_dist)) {
		if(is.null(breaks)) {
			stop_wrap("`col_fun` must have a 'breaks' attribute if `break_dist` is set.")
		}
		if(length(break_dist) == 1) {
			break_dist = rep(break_dist, n - 1)
		}
		if(length(break_dist) != n - 1) {
			stop_wrap(qq("Length of `break_dist` should be `length(at) - 1` which is @{length(at) - 1}."))
		}
		# this function is not vectorized
		generate_map_fun = function(breaks) {
			nb = length(breaks)
			y = c(0, break_dist)
			y = cumsum(y)/sum(y)
			function(x) {
				if(x <= y[1]) {
					return(breaks[1])
				} else if(x >= y[nb]) {
					return(breaks[nb])
				} else {
					for(i in 2:nb) {
						if(x == y[i]) {
							return(breaks[i])
						}
						if(x < y[i]) {
							return( (x - y[i-1])/(y[i] - y[i-1])*(breaks[i] - breaks[i-1]) + breaks[i-1] )
						}
					}
				}
			}
		}
		col_fun2 = col_fun
		map_fun = generate_map_fun(sort(at))
		col_fun = function(x) {
			x2 = sapply(x, map_fun)
			col_fun2(x2)
		}
		y = c(0, break_dist)
		y = cumsum(y)/sum(y)

		attr(col_fun, "breaks") = y
		if(at[1] <= at[n]) {
			at = y
		} else {
			at = rev(y)
		}
	}

	labels_rot = labels_rot %% 360

	n_labels = length(labels)
	labels_width = do.call("unit.c", lapply(labels, function(x) {
			grobWidth(textGrob(x, gp = labels_gp, rot = labels_rot))
		}))
	labels_max_height = max(do.call("unit.c", lapply(labels, function(x) {
			grobHeight(textGrob(x, gp = labels_gp, rot = labels_rot))
		})))
	labels_max_height = convertHeight(labels_max_height, "mm")

	labels_padding_top = unit(1, "mm")

	min_legend_width = sum(labels_width) + unit(2.1, "mm")*n_labels
	if(is.null(legend_width)) legend_width = min_legend_width

	segment_col = border
	if(is_diff_equal(at)) {
		at_diff_is_equal = TRUE
	} else {
		at_diff_is_equal = FALSE
	}

	legend_body_width = legend_width
	legend_body_height = grid_height + labels_padding_top + labels_max_height
	legend_body_width = convertWidth(legend_body_width, "mm")
	legend_body_height = convertHeight(legend_body_height, "mm")

	gl = list()

	# legend grid
	offset = unit(2, "pt")
	
	xmin = offset
	xmax = legend_body_width - offset
	x = (at - at[1])/(at[k] - at[1])*(xmax - xmin)+ xmin
	x = convertX(x, "mm")
	labels_x = convertWidth(x, "mm")
	labels_y = legend_body_height - grid_height - labels_padding_top
	if(labels_rot == 0) {
		labels_just = "top"
	} else if(labels_rot > 0 & labels_rot < 180) {
		labels_just = "right"
	} else if(labels_rot > 180 & labels_rot < 360) {
		labels_just = "left"
	}
	# adjust the text position
	labels_width = do.call("unit.c", lapply(labels, 
		function(x) grobWidth(textGrob(x, gp = labels_gp, rot = labels_rot)) + unit(1, "mm")))
	x_right = labels_x + labels_width*0.5
	x_left = labels_x - labels_width*0.5
	x_right = convertX(x_right, "mm", valueOnly = TRUE)
	x_left = convertX(x_left, "mm", valueOnly = TRUE)

	# adjust to the left extension, caused by e.g. title
	ext = max(legend_extension, labels_width[1]*0.5 - offset)
	xrange = convertX(unit.c(-1*ext,
			legend_body_width - offset + labels_width[length(labels_width)]*0.5
	), "mm", valueOnly = TRUE)

	new_pos = smartAlign(x_left, x_right, xrange)
	x2 = (new_pos[, 1] + new_pos[, 2])/2
	x2 = unit(x2, "mm")
	labels_x = x2

	if(all(abs(as.numeric(labels_x) - as.numeric(x)) < 1e-4)) {
		adjust_text_pos = FALSE
	} else {
		
		adjust_text_pos = TRUE
		labels_padding_top = unit(4, "mm")
		legend_body_height = grid_height + labels_padding_top + labels_max_height
		legend_body_height = convertHeight(legend_body_height, "mm")
		labels_y = legend_body_height - grid_height - labels_padding_top
		# if(is.null(segment_col)) segment_col = "black"
	}
	gl = c(gl, list(
		textGrob(labels, x = labels_x, y = labels_y, just = labels_just, gp = labels_gp, rot = labels_rot)
	))

	if(!at_diff_is_equal) {
		at = seq(at[1], at[length(at)], length.out = n_labels)
	}
	at2 = unlist(lapply(seq_len(n_labels - 1), function(i) {
		x = seq(at[i], at[i+1], length.out = round((at[i+1]-at[i])/(at[k]-at[1])*100))
		x = x[-length(x)]
	}))
	at2 = c(at2, at[length(at)])
	colors = col_fun(at2)
	y2 = unit(rep(1, length(colors)), "npc")
	x2 = seq(0, 1, length.out = length(colors)+1); x2 = x2[-length(x2)] # left of rects
	x2 = x2 * (legend_body_width - 2*offset) + offset
	x2 = x2 + (x2[2] - x2[1])*0.5
	ww = (legend_body_width - 2*offset)*(1/length(colors))
	y2 = unit.c(unit(1, "npc"), y2)
	x2 = unit.c(unit(0, "npc"), x2)
	ww = unit.c(offset + (x2[2] - x2[1])*0.5, rep(ww, length(x2) - 2))
	colors = c(colors[1], colors)

	if(is.null(border)) {
		legend_gp$col = "white"
	} else {
		if(is.null(legend_gp$col)) {
			legend_gp$col = border
		}
	}
	gl = c(gl, list(
		rectGrob(x2, y2, height = grid_height, width = ww, just = c("left", "top"),
			gp = gpar(col = colors, fill = colors)),
		segmentsGrob(x, legend_body_height - grid_height, x, legend_body_height - grid_height + tick_length, gp = legend_gp),
		segmentsGrob(x, legend_body_height - tick_length, x, legend_body_height, gp = legend_gp)
	))

	if(adjust_text_pos) {
		segment_x0 = x
		segment_y0 = legend_body_height - grid_height
		segment_x1 = x
		segment_y1 = legend_body_height - grid_height - labels_padding_top*(1/3)
		gl = c(gl, list(
			segmentsGrob(segment_x0, segment_y0, segment_x1, segment_y1)
		))
		segment_x0 = labels_x
		segment_y0 = legend_body_height - grid_height - labels_padding_top*(2/3)
		segment_x1 = labels_x
		segment_y1 = legend_body_height - grid_height - labels_padding_top + unit(0.5, "mm")
		gl = c(gl, list(
			segmentsGrob(segment_x0, segment_y0, segment_x1, segment_y1)
		))
		segment_x0 = x
		segment_y0 = legend_body_height - grid_height - labels_padding_top*(1/3)
		segment_x1 = labels_x
		segment_y1 = legend_body_height - grid_height - labels_padding_top*(2/3)
		gl = c(gl, list(
			segmentsGrob(segment_x0, segment_y0, segment_x1, segment_y1)
		))
	}

	if(!is.null(border)) {
		legend_gp$fill = "transparent"
		gl = c(gl, list(
			rectGrob(width = legend_width, height = grid_height, y = unit(1, "npc"), just = "top", gp = legend_gp)
		))
	}

	class(gl) = "gList"
	gt = gTree(children = gl, cl = "legend_body", vp = viewport(width = legend_body_width, height = legend_body_height))
	attr(gt, "height") = legend_body_height
	attr(gt, "width") = legend_body_width
	return(gt)
}

legend_body_from_grob = function(grob) {
	legend_body_width = grobWidth(grob)
	legend_body_height = grobHeight(grob)
	grob$x = unit(0, "npc")
	grob$y = unit(0, "npc")
	grob$just =
	gl = list(grob)
	class(gl) = "gList"
	gt = gTree(children = gl, cl = "legend_body", vp = viewport(width = legend_body_width, height = legend_body_height))
	attr(gt, "height") = legend_body_height
	attr(gt, "width") = legend_body_width
	return(gt)
}

# == title
# Pack Legends
#
# == param
# -... A list of objects returned by `Legend`.
# -gap Gap between two neighbouring legends. The value is a `grid::unit` object with length of one.
#      It is the same as ``row_gap`` if the direction if vertial and the same as ``column_gap`` if
#      the direction is horizontal.
# -row_gap Horizontal gaps between legends.
# -column_gap Vertical gaps between legends.
# -direction The direction to arrange legends.
# -max_width The maximal width of the total packed legends. It only works for horizontal arrangement.
#           If the total width of the legends exceeds it, the legends will be arranged into multiple rows.
# -max_height Similar as ``max_width``, but for the vertical arrangment of legends.
# -list The list of legends can be specified as a list.
#
# == value
# A `Legends-class` object.
#
# == seealso
# https://jokergoo.github.io/ComplexHeatmap-reference/book/legends.html#a-list-of-legends
#
# == example
# require(circlize)
# col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
# lgd1 = Legend(at = 1:6, legend_gp = gpar(fill = 1:6), title = "legend1")
# lgd2 = Legend(col_fun = col_fun, title = "legend2", at = c(0, 0.25, 0.5, 0.75, 1))
# pd = packLegend(lgd1, lgd2)
# draw(pd, test = "two legends")
# pd = packLegend(lgd1, lgd2, direction = "horizontal")
# draw(pd, test = "two legends packed horizontally")
packLegend = function(..., gap = unit(4, "mm"), row_gap = unit(4, "mm"), column_gap = unit(4, "mm"),
	direction = c("vertical", "horizontal"),
	max_width = NULL, max_height = NULL, list = NULL) {

	dev.null()
	on.exit(dev.off2())

	if(!is.null(list)) {
		legend_list = list
	} else {
		legend_list = list(...)
	}
	if(length(legend_list) == 1) {
		return(legend_list[[1]])
	}

	legend_list = lapply(legend_list, function(x) {
		lgd = x@grob
		lgd$name = legend_grob_name()
		lgd
	})
	direction = match.arg(direction)
	if(direction == "vertical") {
		if(missing(row_gap)) {
			row_gap = gap
		}
	}
	if(direction == "horizontal") {
		if(missing(column_gap)) {
			column_gap = gap
		}
	}
	if(length(row_gap) != 1) {
		stop_wrap("Length of `row_gap` must be one.")
	}
	if(length(column_gap) != 1) {
		stop_wrap("Length of `column_gap` must be one.")
	}
    n_lgd = length(legend_list)
    nr = 1
    nc = 1
    if(direction == "vertical") {
	    lgd_height = do.call("unit.c", lapply(legend_list, grobHeight))

	    if(is.null(max_height)) {
	    	ind_list = list(1:n_lgd)
	    	nc = 1
	    } else {
	    	lgd_height_num = convertHeight(lgd_height, "mm", valueOnly = TRUE)
	    	max_height_num = convertHeight(max_height, "mm", valueOnly = TRUE)
	    	gap_num = convertHeight(column_gap, "mm", valueOnly = TRUE)

	    	if(n_lgd == 1 && max_height_num < lgd_height_num) {
	    		ind_list = list(1)
	    		nc = 1
	    	} else {
		    	ind_list = split_by_max(lgd_height_num, max_height_num, gap_num)
		    	nc = length(ind_list)
		    }
	    }

    	pack_width = NULL
    	pack_height = NULL
    	for(i in 1:nc) {
    		ind = ind_list[[i]]
    		pack_width = unit.c(pack_width, max(do.call("unit.c", lapply(legend_list[ ind_list[[i]] ], grobWidth))) + column_gap)

    		hu = do.call("unit.c", lapply(legend_list[ind], function(x) unit.c(grobHeight(x), row_gap)))
    		hu = hu[-length(hu)]
    		ph = sum(hu)
    		pack_height[i] = convertHeight(ph, "mm", valueOnly = TRUE)
    	}
    	pack_width[length(pack_width)] = pack_width[length(pack_width)] - column_gap
    	pack_width = convertWidth(pack_width, "mm")
    	pack_height = unit(max(pack_height), "mm")

    	## pack_width is the width for each column
    	## pack_height is the total height of the packed legends
    	gl = list()
    	for(i in 1:nc) {
    		ind = ind_list[[i]]
    		ni = length(ind)
    		legend_x = sum(pack_width[1:i]) - pack_width[i]  # most left side
    		legend_x = convertX(legend_x, "mm")
    		for(j in 1:ni) {
    			# the legend height in current column
    			current_legend_height = do.call("unit.c", lapply(legend_list[ind], function(x) grobHeight(x) + row_gap))
    			current_legend_height = convertHeight(current_legend_height, "mm")
    			legend_y = unit(1, "npc") - sum(current_legend_height[1:j]) + row_gap
	    		gl = c(gl, list(
	    			edit_vp_in_legend_grob(legend_list[[ ind[j] ]], x = legend_x, y = legend_y, valid.just = c(0, 0))
	    		))
	    	}
    	}
    } else {
    	lgd_width = do.call("unit.c", lapply(legend_list, grobWidth))

	    if(is.null(max_width)) {
	    	ind_list = list(1:n_lgd)
	    	nr = 1
	    } else {
	    	lgd_width_num = convertWidth(lgd_width, "mm", valueOnly = TRUE)
	    	max_width_num = convertWidth(max_width, "mm", valueOnly = TRUE)
	    	gap_num = convertWidth(column_gap, "mm", valueOnly = TRUE)

	    	if(n_lgd == 1 && max_width_num < lgd_width_num) {
	    		ind_list = list(1)
	    		nr = 1
	    	} else {
		    	ind_list = split_by_max(lgd_width_num, max_width_num, gap_num)
		    	nr = length(ind_list)
		    }
	    }

    	pack_width = NULL
    	pack_height = NULL
    	for(i in 1:nr) {
    		ind = ind_list[[i]]
    		pack_height = unit.c(pack_height, max(do.call("unit.c", lapply(legend_list[ind], function(x) grobHeight(x) + row_gap))))

    		hu = do.call("unit.c", lapply(legend_list[ind], function(x) unit.c(grobWidth(x), column_gap)))
    		hu = hu[-length(hu)]
    		ph = sum(hu)
    		pack_width[i] = convertWidth(ph, "mm", valueOnly = TRUE)
    	}
    	pack_height[length(pack_height)] = pack_height[length(pack_height)] - row_gap
    	pack_height = convertWidth(pack_height, "mm")
    	pack_width = unit(max(pack_width), "mm")

    	## pack_height is the height for each row
    	## pack_width is the total width of the packed legends

    	gl = list()
    	for(i in 1:nr) {
    		ind = ind_list[[i]]
    		ni = length(ind)
    		legend_y = unit(1, "npc") - sum(pack_height[1:i]) + pack_height[i]  # most bottom side
    		for(j in 1:ni) {
    			# the legend width in current row
    			current_legend_width = do.call("unit.c", lapply(legend_list[ind], function(x) grobWidth(x) + column_gap))
    			current_legend_width = convertWidth(current_legend_width, "mm")
    			legend_x = sum(current_legend_width[1:j]) - column_gap  # most right side
    			legend_x = convertX(legend_x, "mm")
	    		gl = c(gl, list(
	    			edit_vp_in_legend_grob(legend_list[[ ind[j] ]], x = legend_x, y = legend_y, valid.just = c(1, 1))
	    		))
	    	}
	    }
	    	
    }

    pack_legends_width = sum(pack_width)
    pack_legends_height = sum(pack_height)
    pack_legends_width = convertWidth(pack_legends_width, "mm")
    pack_legends_height = convertHeight(pack_legends_height, "mm")

    class(gl) = "gList"
	gt = gTree(children = gl, cl = "packed_legends", vp = viewport(width = pack_legends_width, height = pack_legends_height))
	attr(gt, "width") = pack_legends_width
	attr(gt, "height") = pack_legends_height

	object = new("Legends")
	object@grob = gt
	object@type = "packed_legends"
	object@direction = direction
	object@n = n_lgd
	if(direction == "vertical") {
		object@multiple = nc
	} else {
		object@multiple = nr
	}
	return(object)
}

# 
split_by_max = function(x, max, gap = 0) {
	x = x + gap
	ind = seq_along(x)
	ind_list = list()
	while(length(x)) {
		i2 = which(cumsum(x) < max)
		if(length(i2)) {
			i = max(i2)
		} else {
			i = 1
		}
		ind_list = c(ind_list, list(ind[seq_len(i)]))
		x = x[-seq_len(i)]
		ind = ind[-seq_len(i)]
	}
	ind_list
}

legend_grob_name = (function() {
	i = 1
	function() {
		txt = paste0("legend_grob_", i)
		i <<- i + 1
		return(txt)
	}
})()

edit_vp_in_legend_grob = function(gtree, ...) {
	if(is.null(gtree$vp)) {
		vp_param = list(...)
		nm = names(vp_param)
		if("valid.just" %in% nm) {
			valid.just = vp_param$valid.just
		} else {
			valid.just = NULL
		}
		vp_param = vp_param[names(vp_param) != "valid.just"]
		gtree$vp = do.call(viewport, vp_param)
		if(!is.null(valid.just)) {
			gtree$vp$valid.just = valid.just
			gtree$vp$justification = valid.just
		}
	} else {
		vp_param = list(...)
		nm = names(vp_param)
		if("just" %in% nm) {
			if(is.numeric(vp_param$just)) {
				vp_param$valid.just = vp_param$just
			} else {
				vp_param$valid.just = valid_just(vp_param$just)
			}
		}
		if("justification" %in% nm) {
			if(is.numeric(vp_param$justification)) {
				vp_param$valid.just = vp_param$justification
			} else {
				vp_param$valid.just = valid_just(vp_param$justification)
			}
		} else {
			vp_param$justification = vp_param$valid.just
		}
		
		for(nm in names(vp_param)) {
			gtree$vp[[nm]] = vp_param[[nm]]
		}
	}

	# gtree$vp$name = legend_vp_name()

	return(gtree)
}

valid_just = function(just) {
	if(is.numeric(just)) return(just)
	if(length(just) == 1) {
		just = switch(just,
			"centre" = c("center", "center"),
			"center" = c("center", "center"),
			"left" = c("left", "center"),
			"right" = c("right", "center"),
			"top" = c("center", "top"),
			"bottom" = c("center", "bottom"),
			"top" = c("center", "top"),
			c("center", "center"))
	}
	if(length(just) != 2) {
		stop_wrap("`just` should be a single character or a vector of length 2.")
	}
	j = c("center" = 0.5, "left" = 0, "right" = 1, "top" = 1, "bottom" = 0)
	if(is.character(just)) {
		just = j[just]
	} else if(!is.numeric(just)) {
		stop_wrap("`just` can only be character or numeric.")
	}
	return(unname(just))
}

# == title
# Draw the Legends
#
# == param
# -object The `grid::grob` object returned by `Legend` or `packLegend`.
# -x The x position of the legends, measured in current viewport.
# -y The y position of the legends, measured in current viewport.
# -just Justification of the legends.
# -test Only used for testing.
#
# == details
# In the legend grob, there should always be a viewport attached which is like a wrapper of 
# all the graphic elements in a legend.
# If in the ``object``, there is already a viewport attached, it will modify the ``x``, ``y``
# and ``valid.just`` of the viewport. If there is not viewport attached, a viewport
# with specified ``x``, ``y`` and ``valid.just`` is created and attached.
#
# You can also directly use `grid::grid.draw` to draw the legend object, but you can
# only control the position of the legends by first creating a parent viewport and adjusting
# the position of the parent viewport.
#
# == example
# lgd = Legend(at = 1:4, title = "foo")
# draw(lgd, x = unit(0, "npc"), y = unit(0, "npc"), just = c("left", "bottom"))
#
# # and a similar version of grid.draw
# pushViewport(viewport(x = unit(0, "npc"), y = unit(0, "npc"), just = c("left", "bottom")))
# grid.draw(lgd)
# popViewport()
setMethod(f = "draw",
	signature = "Legends",
	definition = function(object, x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = "centre", test = FALSE) {

	legend = object@grob
	legend = edit_vp_in_legend_grob(legend, x = x, y = y, valid.just = valid_just(just))

	if(is.character(test)) {
		test2 = TRUE
	} else {
		test2 = test
		test = ""
	}
	if(test2) {
        grid.newpage()
        # rect_grob = rectGrob(gp = gpar(col = "red", lty = 2, fill = "transparent"))
        # legend$children[[rect_grob$name]] = rect_grob
        # legend$childrenOrder = c(legend$childrenOrder, rect_grob$name)
    }
	grid.draw(legend)
	if(test2) {
		# grid.rect(width = grobWidth(legend), height = grobHeight(legend))
		grid.text(test, x = 0.5, y = unit(1, "npc") - unit(1, "cm"))
	}
})

# == title
# Draw the Legends
#
# == param
# -x The `grid::grob` object returned by `Legend` or `packLegend`.
# -recording Pass to `grid::grid.draw`.
#
# == details
# This function is actually an S3 method of the ``Legends`` class for the `grid::grid.draw`
# general method. It applies `grid::grid.draw` on the ``grob`` slot of the object.
#
# == example
# lgd = Legend(at = 1:4, title = "foo")
# pushViewport(viewport(x = unit(0, "npc"), y = unit(0, "npc"), just = c("left", "bottom")))
# grid.draw(lgd)
# popViewport()
grid.draw.Legends = function(x, recording = TRUE) {
	grid.draw(x@grob, recording =  recording)
}

# == title
# Grob width for legend_body
#
# == param
# -x A legend_body object.
#
widthDetails.legend_body = function(x) {
	attr(x, "width")
}

# == title
# Grob height for legend_body
#
# == param
# -x A legend_body object.
#
heightDetails.legend_body = function(x) {
	attr(x, "height")
}

# == title
# Grob width for packed_legends
#
# == param
# -x A legend object.
#
widthDetails.legend = function(x) {
	attr(x, "width")
}

# == title
# Grob height for packed_legends
#
# == param
# -x A legend object.
#
heightDetails.legend = function(x) {
	attr(x, "height")
}

# == title
# Grob width for packed_legends
#
# == param
# -x A packed_legends object.
#
widthDetails.packed_legends = function(x) {
	attr(x, "width")
}

# == title
# Grob height for packed_legends
#
# == param
# -x A packed_legends object.
#
heightDetails.packed_legends = function(x) {
	attr(x, "height")
}


# assume x is ordered
is_diff_equal = function(x) {
	all(abs(diff(diff(x)))/mean(diff(x)) < 1e-4)
}

# x, y, pch, size, gp are all vectorized
# width and height are single values
# pch are numeric
.pointsGrob_as_a_list = function(x, y, pch, gp, size, width, height) {
	n = length(x)
	pch2 = pch; pch2[is.na(pch2)] = -1
	if(any(pch2 %in% 26:31)) {
		gl = list()
		for(i in 1:n) {
			if(pch2[i] == 26) {
				gb = segmentsGrob(x[i] - width*0.4, y[i] - height*0.4, x[i] + width*0.4, y[i] + height*0.4, gp = subset_gp(gp[i]))
			} else if(pch2[i] == 27) {
				gb = segmentsGrob(x[i] + width*0.4, y[i] - height*0.4, x[i] - width*0.4, y[i] + height*0.4, gp = subset_gp(gp[i]))
			} else if(pch2[i] == 28) {
				gb = segmentsGrob(unit.c(x[i] - width*0.4, x[i] + width*0.4), 
					              unit.c(y[i] - height*0.4, y[i] - height*0.4),
					              unit.c(x[i] + width*0.4, x[i] - width*0.4),
					              unit.c(y[i] + height*0.4, y[i] + height*0.4), 
					              gp = subset_gp(gp[i]))
			} else if(pch2[i] %in% 29:31) {
				stop_wrap("pch in 29:31 is not implemented in Legend().")
			} else {
				gb = pointsGrob(x = x[i], y = y[i], pch = pch[i], gp = subset_gp(gp, i), size = size[i])
			}
			gl = c(gl, list(gb))
		}
		return(gl)

	} else {
		gb = pointsGrob(x = x, y = y, pch = pch, gp = gp, size = size)
		return(list(gb))
	}
}
