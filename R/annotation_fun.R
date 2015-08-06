
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
# -axis whether add axis.
# -axis_side if it is placed as column annotation, value can only be "left" or "right".
#            If it is placed as row annotation, value can only be "bottom" or "top".
# -axis_gp graphic parameters for axis
# -... for future use.
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_points = function(x, which = c("column", "row"), border = TRUE, gp = gpar(), pch = 16, 
	size = unit(2, "mm"), axis = FALSE, axis_side = NULL, 
	axis_gp = gpar(fontsize = 8), ...) {

	x = x
	which = match.arg(which)[1]
	data_scale = range(x)
	data_scale = data_scale + c(-0.05, 0.05)*(data_scale[2] - data_scale[1])
	gp = check_gp(gp)

	if(which == "column") {
		if(is.null(axis_side)) axis_side = "left"
		if(axis_side == "top" || axis_side == "bottom") {
			stop("`axis_side` can only be 'left' and 'right' for column annotations")
		}
	}
	if(which == "row") {
		if(is.null(axis_side)) axis_side = "bottom"
		if(axis_side == "left" || axis_side == "right") {
			stop("`axis_side` can only be 'top' and 'bottom' for row annotations")
		}
	}

	switch(which,
		row = function(index, k = NULL, N = NULL) {
			n = length(index)
			
			if(is.null(k)) {
				gp = subset_gp(gp, index)
			} else {
				gp = subset_gp(gp, k)
			}

			pushViewport(viewport(xscale = data_scale, yscale = c(0.5, n+0.5)))
			if(border) grid.rect()
			grid.points(x[index], n - seq_along(index) + 1, gp = gp, default.units = "native", pch = pch, size = size)
			if(axis) {
				if(k == 1 && axis_side == "top") {
					grid.xaxis(main = FALSE, gp = axis_gp)
				} else if(k == N && axis_side == "bottom") {
					grid.xaxis(gp = axis_gp)
				}
			}
			upViewport()
		},
		column = function(index) {
			n = length(index)
			gp = subset_gp(gp, index)

			pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = data_scale))
			if(border) grid.rect()
			grid.points(seq_along(index), x[index], gp = gp, default.units = "native", pch = pch, size = size)
			if(axis) {
				if(axis_side == "left") {
					grid.yaxis(gp = axis_gp)
				} else {
					grid.yaxis(main = FALSE, gp = axis_gp)
				}
			}
			upViewport()
		})
}

# == title
# Using barplot as annotation
#
# == param
# -x a vector of numeric values.
# -which is the annotation a column annotation or a row annotation?
# -border whether show border of the annotation compoment
# -bar_width relative width of the bars, should less than one
# -gp graphic parameters.
# -axis whether add axis
# -axis_side if it is placed as column annotation, value can only be "left" or "right".
#            If it is placed as row annotation, value can only be "bottom" or "top".
# -axis_gp graphic parameters for axis
# -... for future use.
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_barplot = function(x, which = c("column", "row"), border = TRUE, bar_width = 0.6,
	gp = gpar(fill = "#CCCCCC"), axis = FALSE, axis_side = NULL, 
	axis_gp = gpar(fontsize = 8), ...) {

	x = x
	which = match.arg(which)[1]

	factor = bar_width
	data_scale = range(x)
	data_scale = data_scale + c(-0.05, 0.05)*(data_scale[2] - data_scale[1])
	gp = check_gp(gp)

	if(which == "column") {
		if(is.null(axis_side)) axis_side = "left"
		if(axis_side == "top" || axis_side == "bottom") {
			stop("`axis_side` can only be 'left' and 'right' for column annotations")
		}
	}
	if(which == "row") {
		if(is.null(axis_side)) axis_side = "bottom"
		if(axis_side == "left" || axis_side == "right") {
			stop("`axis_side` can only be 'top' and 'bottom' for row annotations")
		}
	}

	switch(which,
		row = function(index, k = NULL, N = NULL) {
			n = length(index)
			
			if(is.null(k)) {
				gp = subset_gp(gp, index)
			} else {
				gp = subset_gp(gp, k)
			}

			pushViewport(viewport(xscale = data_scale, yscale = c(0.5, n+0.5)))
			if(border) grid.rect()
			grid.rect(x = data_scale[1], y = n - seq_along(index) + 1, width = x[index] - data_scale[1], height = 1*factor, just = "left", default.units = "native", gp = gp)
			if(axis) {
				if(k == 1 && axis_side == "top") {
					grid.xaxis(main = FALSE, gp = axis_gp)
				} else if(k == N && axis_side == "bottom") {
					grid.xaxis(gp = axis_gp)
				}
			}
			upViewport()
		},
		column = function(index) {
			n = length(index)

			gp = subset_gp(gp, index)
			
			pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = data_scale))
			if(border) grid.rect()
			grid.rect(x = seq_along(index), y = data_scale[1], height = x[index] - data_scale[1], width = 1*factor, just = "bottom", default.units = "native", gp = gp)
			if(axis) {
				if(axis_side == "left") {
					grid.yaxis(gp = axis_gp)
				} else {
					grid.yaxis(main = FALSE, gp = axis_gp)
				}
			}
			upViewport()
		})
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
# -pch point type
# -size point size
# -axis whether add axis
# -axis_side if it is placed as column annotation, value can only be "left" or "right".
#            If it is placed as row annotation, value can only be "bottom" or "top".
# -axis_gp graphic parameters for axis
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_boxplot = function(x, which = c("column", "row"), border = TRUE,
	gp = gpar(fill = "#CCCCCC"), 
	pch = 16, size = unit(2, "mm"), axis = FALSE, axis_side = NULL, 
	axis_gp = gpar(fontsize = 8)) {

	x = x
	which = match.arg(which)[1]

	factor = 0.6
	data_scale = range(x)
	data_scale = data_scale + c(-0.05, 0.05)*(data_scale[2] - data_scale[1])
	gp = check_gp(gp)

	if(which == "column") {
		if(is.null(axis_side)) axis_side = "left"
		if(axis_side == "top" || axis_side == "bottom") {
			stop("`axis_side` can only be 'left' and 'right' for column annotations")
		}
	}
	if(which == "row") {
		if(is.null(axis_side)) axis_side = "bottom"
		if(axis_side == "left" || axis_side == "right") {
			stop("`axis_side` can only be 'top' and 'bottom' for row annotations")
		}
	}

	switch(which,
		row = function(index, k = NULL, N = NULL) {
			if(is.matrix(x)) {
				x = x[index, , drop = FALSE]
				boxplot_stats = boxplot(t(x), plot = FALSE)$stats
			} else {
				x = x[index]
				boxplot_stats = boxplot(x, plot = FALSE)$stats
			}

			n = length(index)
			if(is.null(k)) {
				gp = subset_gp(gp, index)
			} else {
				gp = subset_gp(gp, k)
			}
			if(n != ncol(boxplot_stats)) {
				stop(paste0("Length of index should be ", ncol(boxplot_stats)))
			}
			pushViewport(viewport(xscale = data_scale, yscale = c(0.5, n+0.5)))
			if(border) grid.rect()
			grid.segments(boxplot_stats[5, ], n - seq_along(index) + 1 - 0.5*factor, 
				          boxplot_stats[5, ], n - seq_along(index) + 1 + 0.5*factor, default.units = "native", gp = gp)
			grid.segments(boxplot_stats[5, ], n - seq_along(index) + 1,
				          boxplot_stats[4, ], n - seq_along(index) + 1, default.units = "native", gp = gp)
			grid.segments(boxplot_stats[1, ], n - seq_along(index) + 1, 
				          boxplot_stats[2, ], n - seq_along(index) + 1, default.units = "native", gp = gp)
			grid.segments(boxplot_stats[1, ], n - seq_along(index) + 1 - 0.5*factor, 
				          boxplot_stats[1, ], n - seq_along(index) + 1 + 0.5*factor, default.units = "native", gp = gp)
			grid.rect(x = boxplot_stats[2, ], y = n - seq_along(index) + 1,  
				height = 1*factor, width = boxplot_stats[4, ] - boxplot_stats[2, ], just = "left", 
				default.units = "native", gp = gp)
			grid.points(x = boxplot_stats[3, ], y = n - seq_along(index) + 1, default.units = "native", gp = gp, pch = pch, size = size)
			if(axis) {
				if(k == 1 && axis_side == "top") {
					grid.xaxis(main = FALSE, gp = axis_gp)
				} else if(k == N && axis_side == "bottom") {
					grid.xaxis(gp = axis_gp)
				}
			}
			upViewport()
		},
		column = function(index) {
			if(is.matrix(x)) {
				x = x[, index, drop = FALSE]
				boxplot_stats = boxplot(x, plot = FALSE)$stats
			} else {
				x = x[index]
				boxplot_stats = boxplot(x, plot = FALSE)$stats
			}

			n = length(index)
			gp = subset_gp(gp, index)
			if(n != ncol(boxplot_stats)) {
				stop(paste0("Length of index should be ", ncol(boxplot_stats)))
			}
			pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = data_scale))
			if(border) grid.rect()
			grid.segments(seq_along(index) - 0.5*factor, boxplot_stats[5, ],
				          seq_along(index) + 0.5*factor, boxplot_stats[5, ], default.units = "native", gp = gp)
			grid.segments(seq_along(index), boxplot_stats[5, ],
				          seq_along(index), boxplot_stats[4, ], default.units = "native", gp = gp)
			grid.segments(seq_along(index), boxplot_stats[1, ],
				          seq_along(index), boxplot_stats[2, ], default.units = "native", gp = gp)
			grid.segments(seq_along(index) - 0.5*factor, boxplot_stats[1, ],
				          seq_along(index) + 0.5*factor, boxplot_stats[1, ], default.units = "native", gp = gp)
			grid.rect(x = seq_along(index), y = boxplot_stats[2, ], 
				height = boxplot_stats[4, ] - boxplot_stats[2, ], width = 1*factor, just = "bottom", 
				default.units = "native", gp = gp)
			grid.points(x = seq_along(index), y = boxplot_stats[3, ], default.units = "native", gp = gp, pch = pch, size = size)
			if(axis) {
				if(axis_side == "left") {
					grid.yaxis(gp = axis_gp)
				} else {
					grid.yaxis(main = FALSE, gp = axis_gp)
				}
			}
			upViewport()
		})
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
anno_histogram = function(x, which = c("column", "row"), gp = gpar(fill = "#CCCCCC"), ...) {
	x = x
	which = match.arg(which)[1]

	gp = check_gp(gp)

	switch(which,
		row = function(index, k = NULL, N = NULL) {
			if(is.matrix(x)) {
				x = x[index, , drop = FALSE]
				x_range =range(x)
				histogram_stats = apply(x, 1, hist, plot = FALSE, breaks = seq(x_range[1], x_range[2], length = 11), ...)
				histogram_breaks = lapply(histogram_stats, function(x) x$breaks)
				histogram_counts = lapply(histogram_stats, function(x) x$counts)
			} else {
				x = x[index]
				x_range =range(unlist(x))
				histogram_stats = lapply(x, hist, plot = FALSE, breaks = seq(x_range[1], x_range[2], length = 11), ...)
				histogram_breaks = lapply(histogram_stats, function(x) x$breaks)
				histogram_counts = lapply(histogram_stats, function(x) x$counts)
			}

			xscale = range(unlist(histogram_breaks))
			xscale = xscale + c(-0.05, 0.05)*(xscale[2] - xscale[1])
			yscale = c(0, max(unlist(histogram_counts)))
			yscale[2] = yscale[2]*1.05
			
			n = length(index)
			if(is.null(k)) {
				gp = subset_gp(gp, index)
			} else {
				gp = subset_gp(gp, k)
			}
			if(n != length(histogram_counts)) {
				stop(paste0("Length of index should be ", length(histogram_counts)))
			}
			for(i in seq_len(n)) {
				n_breaks = length(histogram_breaks[[i]])
				pushViewport(viewport(x = unit(0, "npc"), y = unit((n-i)/n, "npc"), height = unit(1/n, "npc"), just = c("left", "bottom"), xscale = xscale, yscale = yscale))
				grid.rect(x = histogram_breaks[[i]][-1], y = 0, width = histogram_breaks[[i]][-1] - histogram_breaks[[i]][-n_breaks], height = histogram_counts[[i]], just = c("right", "bottom"), default.units = "native", gp = subset_gp(gp, index[i]))	
				upViewport()
			}
		},
		column = function(index) {
			if(is.matrix(x)) {
				x = x[, index, drop = FALSE]
				x_range = range(x)
				histogram_stats = apply(x, 2, hist, plot = FALSE, breaks = seq(x_range[1], x_range[2], length = 11), ...)
				histogram_breaks = lapply(histogram_stats, function(x) x$breaks)
				histogram_counts = lapply(histogram_stats, function(x) x$counts)
			} else {
				x = x[index]
				x_range = range(unlist(x))
				histogram_stats = lapply(x, hist, plot = FALSE, breaks = seq(x_range[1], x_range[2], length =11), ...)
				histogram_breaks = lapply(histogram_stats, function(x) x$breaks)
				histogram_counts = lapply(histogram_stats, function(x) x$counts)
			}

			yscale = range(unlist(histogram_breaks))
			yscale = yscale + c(-0.05, 0.05)*(yscale[2] - yscale[1])
			xscale = c(0, max(unlist(histogram_counts)))
			xscale[2] = xscale[2]*1.05

			n = length(index)
			gp = subset_gp(gp, index)
			if(n != length(histogram_counts)) {
				stop(paste0("Length of index should be ", length(histogram_counts)))
			}
			for(i in seq_len(n)) {
				n_breaks = length(histogram_breaks[[i]])
				pushViewport(viewport(y = unit(0, "npc"), x = unit(i/n, "npc"), width = unit(1/n, "npc"), just = c("right", "bottom"), xscale = xscale, yscale = yscale))
				grid.rect(y = histogram_breaks[[i]][-1], x = 0, height = histogram_breaks[[i]][-1] - histogram_breaks[[i]][-n_breaks], width = histogram_counts[[i]], just = c("left", "top"), default.units = "native", gp = subset_gp(gp, index[i]))	
				upViewport()
			}
		})
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
	type = c("lines", "violin", "heatmap"), ...) {
	x = x
	which = match.arg(which)[1]
	type = match.arg(type)[1]

	gp = check_gp(gp)

	switch(which,
		row = function(index, k = NULL, N = NULL) {
			if(is.matrix(x)) {
				x = x[index, , drop = FALSE]
				density_stats = apply(x, 1, density, ...)
				density_x = lapply(density_stats, function(x) x$x)
				density_y = lapply(density_stats, function(x) x$y)
			} else {
				x = x[index]
				density_stats = lapply(x, density, ...)
				density_x = lapply(density_stats, function(x) x$x)
				density_y = lapply(density_stats, function(x) x$y)
			}
			min_density_x = min(unlist(density_x))
			max_density_x = max(unlist(density_x))
			
			xscale = range(unlist(density_x))
			xscale = xscale + c(-0.05, 0.05)*(xscale[2] - xscale[1])
			if(type == "lines") {
				yscale = c(0, max(unlist(density_y)))
				yscale[2] = yscale[2]*1.05
			} else if(type == "violin") {
				yscale = max(unlist(density_y))
				yscale = c(-yscale*1.05, yscale*1.05)
			} else if(type == "heatmap") {
				xscale = range(unlist(density_x))
				yscale = c(0, 1)
				min_y = min(unlist(density_y))
				max_y = max(unlist(density_y))
				col_fun = colorRamp2(seq(min_y, max_y, length = 11), rev(brewer.pal(name = "RdYlBu", n = 11)))
			}
			n = length(index)
			if(is.null(k)) {
				gp = subset_gp(gp, index)
			} else {
				gp = subset_gp(gp, k)
			}
			if(n != length(density_x)) {
				stop(paste0("Length of index should be ", length(density_x)))
			}
			for(i in seq_len(n)) {
				pushViewport(viewport(x = unit(0, "npc"), y = unit((n-i)/n, "npc"), just = c("left", "bottom"), height = unit(1/n, "npc"), xscale = xscale, yscale = yscale))
				if(type == "lines") {
					grid.polygon(x = density_x[[i]], y = density_y[[i]], default.units = "native", gp = subset_gp(gp, index[i]))
				} else if(type == "violin") {
					grid.polygon(x = c(density_x[[i]], rev(density_x[[i]])), y = c(density_y[[i]], -rev(density_y[[i]])), default.units = "native", gp = subset_gp(gp, index[i]))
				} else if(type == "heatmap") {
					n_breaks = length(density_x[[i]])
					grid.rect(x = density_x[[i]][-1], y = 0, width = density_x[[i]][-1] - density_x[[i]][-n_breaks], height = 1, just = c("right", "bottom"), default.units = "native", gp = gpar(fill = col_fun((density_y[[i]][-1] + density_y[[i]][-n_breaks])/2), col = NA))
					grid.rect(x = density_x[[i]][1], y = 0, width = density_x[[i]][1] - min_density_x, height = 1, just = c("right", "bottom"), default.units = "native", gp = gpar(fill = col_fun(0), col = NA))
					grid.rect(x = density_x[[i]][n_breaks], y = 0, width = max_density_x - density_x[[i]][n_breaks], height = 1, just = c("left", "bottom"), default.units = "native", gp = gpar(fill = col_fun(0), col = NA))
				}
				upViewport()
			}
		},
		column = function(index) {
			if(is.matrix(x)) {
				x = x[, index, drop = FALSE]
				density_stats = apply(x, 2, density, ...)
				density_x = lapply(density_stats, function(x) x$x)
				density_y = lapply(density_stats, function(x) x$y)
			} else {
				x = x[index]
				density_stats = lapply(x, density, ...)
				density_x = lapply(density_stats, function(x) x$x)
				density_y = lapply(density_stats, function(x) x$y)
			}
			min_density_x = min(unlist(density_x))
			max_density_x = max(unlist(density_x))
			
			yscale = range(unlist(density_x))
			yscale = yscale + c(-0.05, 0.05)*(yscale[2] - yscale[1])
			if(type == "lines") {
				xscale = c(0, max(unlist(density_y)))
				xscale[2] = xscale[2]*1.05
			} else if(type == "violin") {
				xscale = max(unlist(density_y))
				xscale = c(-xscale*1.05, xscale*1.05)
			} else if(type == "heatmap") {
				yscale = range(unlist(density_x))
				xscale = c(0, 1)
				min_y = min(unlist(density_y))
				max_y = max(unlist(density_y))
				col_fun = colorRamp2(seq(min_y, max_y, length = 11), rev(brewer.pal(name = "RdYlBu", n = 11)))
			}

			n = length(index)
			gp = subset_gp(gp, index)
			if(n != length(density_x)) {
				stop(paste0("Length of index should be ", length(density_x)))
			}
			for(i in seq_len(n)) {
				pushViewport(viewport(y = unit(0, "npc"), x = unit(i/n, "npc"), width = unit(1/n, "npc"), just = c("right", "bottom"), xscale = xscale, yscale = yscale))
				if(type == "lines") {
					grid.polygon(y = density_x[[i]], x = density_y[[i]], default.units = "native", gp = subset_gp(gp, index[i]))
				} else if(type == "violin") {
					grid.polygon(y = c(density_x[[i]], rev(density_x[[i]])), x = c(density_y[[i]], -rev(density_y[[i]])), default.units = "native", gp = subset_gp(gp, index[i]))
				} else if(type == "heatmap") {
					n_breaks = length(density_x[[i]])
					grid.rect(y = density_x[[i]][-1], x = 0, height = density_x[[i]][-1] - density_x[[i]][-n_breaks], width = 1, just = c("left", "top"), default.units = "native", gp = gpar(fill = col_fun((density_y[[i]][-1] + density_y[[i]][-n_breaks])/2), col = NA))
					grid.rect(y = density_x[[i]][1], x = 0, height = density_x[[i]][1] - min_density_x, width = 1, just = c("left", "top"), default.units = "native", gp = gpar(fill = col_fun(0), col = NA))
					grid.rect(y = density_x[[i]][n_breaks], x = 0, height = max_density_x - density_x[[i]][n_breaks], width = 1, just = c("left", "bottom"), default.units = "native", gp = gpar(fill = col_fun(0), col = NA))
				}
				upViewport()
			}
		})
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
anno_text = function(x, which = c("column", "row"), gp = gpar(), rot = 0, 
	just = NULL, offset = unit(0.5, "npc")) {

	x = x
	which = match.arg(which)[1]
	gp = check_gp(gp)

	switch(which,
		row = function(index, k = NULL, N = NULL) {
			n = length(index)
			
			if(is.null(k)) {
				gp = subset_gp(gp, index)
			} else {
				gp = subset_gp(gp, k)
			}
			pushViewport(viewport(xscale = c(0, 1), yscale = c(0.5, n+0.5)))
			grid.text(x[index], offset, unit(n - seq_along(index) + 1, "native"), gp = gp, just = just, rot = rot)
			upViewport()
		},
		column = function(index) {
			n = length(index)
			gp = subset_gp(gp, index)
			pushViewport(viewport(yscale = c(0, 1), xscale = c(0.5, n+0.5)))
			grid.text(x[index], unit(seq_along(index), "native"), offset, gp = gp, just = just, rot = rot)
			upViewport()
		})
}
