 
# == title
# Function to add simple graphics as annotations
#
# == param
# -x a vector of values.
# -type ``p`` for points, ``histogram`` for histogram.
# -which is the annotation a column annotation or a row annotation?
# -gp graphic parameters.
# -pch point type.
# -size point size.
#
# == details
# It is a shortcut function for `anno_points` and `anno_histogram`.
# It is designed to support more graphics in future versions.
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == see also
# `anno_points`, `anno_histogram`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_simple = function(x, type = c("p", "histogram"), which = c("column", "row"), 
	gp = gpar(), pch = 16, size = unit(2, "mm")) {
	x = x
	which = match.arg(which)[1]
	switch(type,
		p = anno_points(x, which = which, gp = gp, pch = pch, size = size),
		histogram = anno_histogram(x, which = which, gp = gp))
}

# == title
# Using points as annotation
#
# == param
# -x a vector of values.
# -which is the annotation a column annotation or a row annotation?
# -gp graphic parameters.
# -pch point type.
# -size point size.
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_points = function(x, which = c("column", "row"), gp = gpar(), pch = 16, 
	size = unit(2, "mm")) {

	x = x
	which = match.arg(which)[1]
	data_scale = range(x)
	data_scale = data_scale + c(-0.05, 0.05)*(data_scale[2] - data_scale[1])

	switch(which,
		row = function(index) {
			n = length(index)
			pushViewport(viewport(xscale = data_scale, yscale = c(0.5, n+0.5)))
			grid.rect()
			grid.points(x[index], seq_along(index), gp = gp, default.units = "native", pch = pch, size = size)
			upViewport()
		},
		column = function(index) {
			n = length(index)
			pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = data_scale))
			grid.rect()
			grid.points(seq_along(index), x[index], gp = gp, default.units = "native", pch = pch, size = size)
			upViewport()
		})
}

# == title
# Using histogram as annotation
#
# == param
# -x a vector of values.
# -which is the annotation a column annotation or a row annotation?
# -gp graphic parameters.
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_histogram = function(x, which = c("column", "row"), gp = gpar()) {
	x = x
	which = match.arg(which)[1]

	factor = 0.6
	data_scale = range(x)
	data_scale = data_scale + c(-0.05, 0.05)*(data_scale[2] - data_scale[1])

	switch(which,
		row = function(index) {
			n = length(index)
			pushViewport(viewport(xscale = data_scale, yscale = c(0.5, n+0.5)))
			grid.rect()
			grid.rect(x = min(x), y = seq_along(index), width = x[index], height = 1*factor, just = "left", default.units = "native", gp = gp)
			upViewport()
		},
		column = function(index) {
			n = length(index)
			pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = data_scale))
			grid.rect()
			grid.rect(x = seq_along(index), y = min(x), height = x[index], width = 1*factor, just = "bottom", default.units = "native", gp = gp)
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
# -gp graphic parameters
# -pch point type
# -size point size
#
# == value
# A graphic function which can be set in `HeatmapAnnotation` constructor method.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_boxplot = function(x, which = c("column", "row"), gp = gpar(fill = "#CCCCCC"), 
	pch = 16, size = unit(2, "mm")) {
	x = x
	which = match.arg(which)[1]

	factor = 0.6
	data_scale = range(x)
	data_scale = data_scale + c(-0.05, 0.05)*(data_scale[2] - data_scale[1])

	switch(which,
		row = function(index) {
			if(is.matrix(x)) {
				x = x[sort(index), , drop = FALSE]
				boxplot_stats = boxplot(t(x), plot = FALSE)$stats
			} else {
				x = x[sort(index)]
				boxplot_stats = boxplot(x, plot = FALSE)$stats
			}

			n = length(index)
			pushViewport(viewport(xscale = data_scale, yscale = c(0.5, n+0.5)))
			grid.rect()
			grid.segments(boxplot_stats[5, ], seq_along(index) - 0.5*factor, 
				          boxplot_stats[5, ], seq_along(index) + 0.5*factor, default.units = "native", gp = gp)
			grid.segments(boxplot_stats[5, ], seq_along(index),
				          boxplot_stats[4, ], seq_along(index), default.units = "native", gp = gp)
			grid.segments(boxplot_stats[1, ], seq_along(index), 
				          boxplot_stats[2, ], seq_along(index), default.units = "native", gp = gp)
			grid.segments(boxplot_stats[1, ], seq_along(index) - 0.5*factor, 
				          boxplot_stats[1, ], seq_along(index) + 0.5*factor, default.units = "native", gp = gp)
			grid.rect(x = boxplot_stats[2, ], y = seq_along(index),  
				height = 1*factor, width = boxplot_stats[4, ] - boxplot_stats[2, ], just = "left", 
				default.units = "native", gp = gp)
			grid.points(x = boxplot_stats[3, ], y = seq_along(index), default.units = "native", gp = gp, pch = pch, size = size)
			upViewport()
		},
		column = function(index) {
			if(is.matrix(x)) {
				x = x[, sort(index), drop = FALSE]
				boxplot_stats = boxplot(x, plot = FALSE)$stats
			} else {
				x = x[sort(x)]
				boxplot_stats = boxplot(x, plot = FALSE)$stats
			}

			n = length(index)
			pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = data_scale))
			grid.rect()
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
			upViewport()
		})
}
