
# == title
# Use colors to represent density distribution
#
# == param
# -data  a matrix or a list. If it is a matrix, density will be calculated by columns.
# -col a list of colors that density values are mapped to.
# -density_param parameters send to `stats::density`, ``na.rm`` is enforced to ``TRUE``.
# -color_space the color space in which colors are interpolated. Pass to `circlize::colorRamp2`.
# -anno annotation for the matrix columns or the list. The value should be a vector or a data frame 
#       and colors for annotations are randomly assigned. If you want to customize the annotation colors,
#       use a `HeatmapAnnotation-class` object directly.
# -ylab label on y-axis in the plot
# -title title of the plot
# -range ranges on the y-axis. By default the range is between 1th quantile and 99th quantile of the data.
# -cluster_columns whether cluster columns (here cluster by density distributions)
# -clustering_distance_columns pass to `Heatmap`
# -clustering_method_columns pass to `Heatmap`
# -column_dend_side pass to `Heatmap`
# -column_dend_height pass to `Heatmap`
# -show_column_dend pass to `Heatmap`
# -column_dend_gp pass to `Heatmap`
# -column_dend_reorder pass to `Heatmap`
# -column_names_side pass to `Heatmap`
# -show_column_names pass to `Heatmap`
# -column_names_max_height pass to `Heatmap`
# -column_names_gp pass to `Heatmap`
# -column_order order of columns
# -... pass to `draw,HeatmapList-method`
#
# == details
# To visualize data distribution in a matrix or in a list, sometimes we use boxplot or beanplot.
# Here we use colors to map the density values and visualize distribution of values
# in each column (or each vector in the list) through a heatmap. It is useful if you have huge number 
# of columns in ``data`` to visualize.
#
# The density matrix is generated with 500 rows ranging between the maximun and minimal values in all densities.
# The density values in each row are linearly intepolated between the two density values at the two nearest bounds.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# matrix = matrix(rnorm(100), 10); colnames(matrix) = letters[1:10]
# densityHeatmap(matrix)
# densityHeatmap(matrix, anno = rep(c("A", "B"), each = 5))
# densityHeatmap(matrix, col = c("white", "red"), anno = rep(c("A", "B"), each = 5))
#
# ha = HeatmapAnnotation(points = anno_points(runif(10)),
#     anno = rep(c("A", "B"), each = 5), col = list(anno = c("A" = "red", "B" = "blue")))
# densityHeatmap(matrix, anno = ha)
#
# lt = list(rnorm(10), rnorm(10))
# densityHeatmap(lt)
#
densityHeatmap = function(data, 
	density_param = list(na.rm = TRUE),
	
	col = rev(brewer.pal(11, "Spectral")),
	color_space = "LAB", 
	top_annotation = NULL, 
	bottom_annotation = NULL,
	ylab = deparse(substitute(data)),
	column_title = paste0("Density heatmap of ", deparse(substitute(data))),
	title = column_title,
	range = c(-Inf, Inf),

	cluster_columns = FALSE,
	clustering_distance_columns = "euclidean",
	clustering_method_columns = "complete",
	column_dend_side = "top",
	column_dend_height = unit(10, "mm"),
	show_column_dend = FALSE,
	column_dend_gp = gpar(),
	column_dend_reorder = TRUE,
	column_names_side = c("bottom", "top"),
	show_column_names = TRUE,
	column_names_max_height = unit(6, "cm"),
	column_names_gp = gpar(fontsize = 12),
	column_order = NULL,
	...) {

	density_param$na.rm = TRUE

	if(is.matrix(data)) {
		density_list = apply(data, 2, function(x) do.call(density, c(list(x = x), density_param)))
		quantile_list = apply(data, 2, quantile, na.rm = TRUE)
		mean_value = apply(data, 2, mean, na.rm = TRUE)
	} else if(is.data.frame(data) || is.list(data)) {
		density_list = lapply(data, function(x) do.call(density, c(list(x = x), density_param)))
		quantile_list = sapply(data, quantile, na.rm = TRUE)
		mean_value = sapply(data, mean, na.rm = TRUE)
	} else {
		stop("only matrix and list are allowed.")
	}

	n = length(density_list)
	nm = names(density_list)

	max_x = quantile(unlist(lapply(density_list, function(x) x$x)), 0.99)
	min_x = quantile(unlist(lapply(density_list, function(x) x$x)), 0.01)

	max_x = min(max_x, range[2])
	min_x = max(min_x, range[1])
	
	x = seq(min_x, max_x, length = 500)

	mat = lapply(density_list, function(r) {
			f = approxfun(r$x, r$y)
			res = f(x)
			res[is.na(res)] = 0
			rev(res)
		})
	mat = as.matrix(as.data.frame(mat))
	colnames(mat) = nm

	col = colorRamp2(seq(0, max(mat, na.rm = TRUE), length = length(col)), col, space = color_space)

	bb = grid.pretty(c(min_x, max_x))
	ht = Heatmap(mat, col = col, name = "density", cluster_rows = FALSE, 
		cluster_columns = cluster_columns,
		clustering_distance_columns = clustering_distance_columns,
		clustering_method_columns = clustering_method_columns,
		column_dend_side = column_dend_side,
		column_dend_height = column_dend_height,
		show_column_dend = show_column_dend,
		column_dend_gp = column_dend_gp,
		column_dend_reorder = column_dend_reorder,
		column_names_side = column_names_side,
		show_column_names = show_column_names,
		column_names_max_height = column_names_max_height,
		column_names_gp = column_names_gp,
		column_order = column_order,
		left_annotation = rowAnnotation(axis = anno_empty(border = FALSE, 
				width = grobHeight(textGrob(ylab))*2 + max_text_width(bb) + unit(6, "mm")),
			show_annotation_name = FALSE), 
		right_annotation = rowAnnotation(quantile = anno_empty(border = FALSE, 
				width = grobWidth(textGrob("100%")) + unit(6, "mm")),
			show_annotation_name = FALSE)
	)

	ht = draw(ht, column_title = title, ...)
	column_order = column_order(ht)$density

	decorate_annotation("axis", {
		grid.text(ylab, x = grobHeight(textGrob(ylab)), rot = 90)
	})

	decorate_heatmap_body("density", {
		pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(min_x, max_x), clip = TRUE))
		for(i in seq_len(5)) {
			grid.lines(1:n, quantile_list[i, column_order], default.units = "native", gp = gpar(lty = 2))
		}
		grid.lines(1:n, mean_value[column_order], default.units = "native", gp = gpar(lty = 2, col = "darkred"))
		upViewport()
	})
	decorate_heatmap_body("density", {
		pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(min_x, max_x), clip = FALSE))
		grid.rect(gp = gpar(fill = NA))
		grid.yaxis()

		labels = c(rownames(quantile_list), "mean")
		y = c(quantile_list[, column_order[n]], mean_value[column_order[n]])
		od = order(y)
		y = y[od]
		labels = labels[od]
		text_height = convertHeight(grobHeight(textGrob(labels[1])) * (1 + 0.2), "native", valueOnly = TRUE)
        h1 = y - text_height*0.5
        h2 = y + text_height*0.5
        pos = rev(smartAlign(h1, h2, c(min_x, max_x)))
        h = (pos[, 1] + pos[, 2])/2
        link_width = unit(6, "mm")
        n2 = length(labels)
        grid.text(labels, unit(1, "npc") + rep(link_width, n2), h, default.units = "native", just = "left")
        link_width = link_width - unit(1, "mm")
        grid.segments(unit(rep(1, n2), "npc"), y, unit(1, "npc") + rep(link_width * (1/3), n2), y, default.units = "native")
        grid.segments(unit(1, "npc") + rep(link_width * (1/3), n2), y, unit(1, "npc") + rep(link_width * (2/3), n2), h, default.units = "native")
        grid.segments(unit(1, "npc") + rep(link_width * (2/3), n2), h, unit(1, "npc") + rep(link_width, n2), h, default.units = "native")

		upViewport()
	})

	return(invisible(NULL))
}
