 
# == title
# Function to add simple graphics as annotations
#
# == param
# -x a vector
# -type ``p`` for points.
# -which column annotation or row annotation
# -gp graphic parameters
# -pch point type
# -size point size
#
# == details
# Short cut function.
#
# == value
# A function
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
# -x a vector
# -which column annotation or row annotation
# -gp graphic parameters
# -pch point type
# -size point size
#
# == value
# A function
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_points = function(x, which = c("column", "row"), gp = gpar(), pch = 16, 
	size = unit(2, "mm")) {

	x = x
	which = match.arg(which)[1]
	switch(which,
		row = function(index) {
			n = length(index)
			pushViewport(viewport(xscale = range(x), yscale = c(0.5, n+0.5)))
			grid.points(x[index], seq_along(index), gp = gp, default.units = "native", pch = pch, size = size)
			upViewport()
		},
		column = function(index) {
			n = length(index)
			pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = range(x)))
			grid.points(seq_along(index), x[index], gp = gp, default.units = "native", pch = pch, size = size)
			upViewport()
		})
}

# == title
# Using histogram as annotation
#
# == param
# -x a vector
# -which column annotation or row annotation
# -gp graphic parameters
#
# == value
# A function
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_histogram = function(x, which = c("column", "row"), gp = gpar()) {
	x = x
	which = match.arg(which)[1]

	factor = 0.6

	switch(which,
		row = function(index) {
			n = length(index)
			pushViewport(viewport(xscale = range(x), yscale = c(0.5, n+0.5)))
			grid.rect(x = min(x), y = seq_along(index), width = x[index], height = 1*factor, just = "left", default.units = "native", gp = gp)
			upViewport()
		},
		column = function(index) {
			n = length(index)
			pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = range(x)))
			grid.rect(x = seq_along(index), y = min(x), height = x[index], width = 1*factor, just = "bottom", default.units = "native", gp = gp)
			upViewport()
		})
}

# == title
# Using boxplot as annotation
#
# == param
# -x a matrix or a list
# -which column annotation or row annotation
# -gp graphic parameters
# -pch point type
# -size point size
#
# == value
# A function
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
anno_boxplot = function(x, which = c("column", "row"), gp = gpar(), 
	pch = 16, size = unit(2, "mm")) {
	x = x
	which = match.arg(which)[1]

	factor = 0.6

	switch(which,
		row = function(index) {
			if(is.matrix(x)) {
				x = x[index, , drop = FALSE]
				boxplot_stats = boxplot(t(x), plot = FALSE)$stats
			} else {
				boxplot_stats = boxplot(x, plot = FALSE)$stats
			}

			n = length(index)
			pushViewport(viewport(xscale = range(x), yscale = c(0.5, n+0.5)))
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
			grid.points(x = seq_along(index), y = boxplot_stats[3, ], default.units = "native", gp = gp, pch = pch, size = size)
			upViewport()
		},
		column = function(index) {
			if(is.matrix(x)) {
				x = x[, index, drop = FALSE]
				boxplot_stats = boxplot(x, plot = FALSE)$stats
			} else {
				boxplot_stats = boxplot(x, plot = FALSE)$stats
			}

			n = length(index)
			pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = range(x)))
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
