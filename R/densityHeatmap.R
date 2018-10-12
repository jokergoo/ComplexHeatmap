
# == title
# Visualize Density Distribution by Heatmap
#
# == param
# -data A matrix or a list. If it is a matrix, density is calculated by columns.
# -density_param Parameters send to `stats::density`, ``na.rm`` is enforced to be ``TRUE``.
# -col A vector of colors that density values are mapped to.
# -color_space The color space in which colors are interpolated. Pass to `circlize::colorRamp2`.
# -ylab Label on y-axis.
# -column_title Title of the heatmap.
# -title Same as ``column_title``.
# -ylim Ranges on the y-axis. By default the range is between 1th quantile and 99th quantile of the data.
# -range Same as ``ylim``.
# -title_gp = gpar(fontsize = 14),
# -ylab_gp = gpar(fontsize = 12),
# -tick_label_gp = gpar(fontsize = 10),
# -quantile_gp = gpar(fontsize = 10),
# -column_order column_order
# -column_names_side Pass to `Heatmap`.
# -show_column_names Pass to `Heatmap`.
# -column_names_max_height Pass to `Heatmap`.
# -column_names_gp Pass to `Heatmap`.
# -column_names_rot Pass to `Heatmap`.
# -cluster_columns Whether cluster columns (here clustered by density values)? Normally we don't cluster columns.
# -... pass to `Heatmap`.
#
# == details
# To visualize data distribution in a matrix or in a list, we normally use
# boxplot or violinplot. We can also use colors to map the density values and
# visualize distribution of values through a heatmap. It is useful if you have
# huge number of columns in ``data`` to visualize.
#
# The density matrix is generated with 500 rows ranging between the maximun
# and minimal values in all densities. The density values in each row are
# linearly intepolated between the two density values at the two nearest
# bounds.
#
# == value
# A `HeatmapList-class` object with only one heatmap, but it can only add other heatmaps/annotations vertically.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# matrix = matrix(rnorm(100), 10); colnames(matrix) = letters[1:10]
# densityHeatmap(matrix)
#
# lt = list(rnorm(10), rnorm(10))
# densityHeatmap(lt)
#
# ha = HeatmapAnnotation(points = anno_points(runif(10)),
#     anno = rep(c("A", "B"), each = 5), col = list(anno = c("A" = "red", "B" = "blue")))
# densityHeatmap(matrix, top_annotation = ha)
# densityHeatmap(matrix, top_annotation = ha) %v% Heatmap(matrix, height = unit(6, "cm"))
densityHeatmap = function(data, 
	density_param = list(na.rm = TRUE),
	
	col = rev(brewer.pal(11, "Spectral")),
	color_space = "LAB", 
	ylab = deparse(substitute(data)),
	column_title = paste0("Density heatmap of ", deparse(substitute(data))),
	title = column_title,
	ylim = c(-Inf, Inf),
	range = ylim,

	title_gp = gpar(fontsize = 14),
	ylab_gp = gpar(fontsize = 12),
	tick_label_gp = gpar(fontsize = 10),
	quantile_gp = gpar(fontsize = 10),

	column_order = NULL,
	column_names_side = "bottom",
	show_column_names = TRUE,
	column_names_max_height = unit(6, "cm"),
	column_names_gp = gpar(fontsize = 12),
	column_names_rot = 90,

	cluster_columns = FALSE,
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
	ht = Heatmap(mat, col = col, name = "density", 
		column_title = column_title,
		column_title_gp = title_gp,
		cluster_rows = FALSE, 
		cluster_columns = cluster_columns,
		column_names_side = column_names_side,
		show_column_names = show_column_names,
		column_names_max_height = column_names_max_height,
		column_names_gp = column_names_gp,
		column_names_rot = column_names_rot,
		column_order = column_order,
		left_annotation = rowAnnotation(axis = anno_empty(border = FALSE, 
				width = grobHeight(textGrob(ylab, gp = ylab_gp))*2 + max_text_width(bb, gp = tick_label_gp) + unit(4, "mm")),
			show_annotation_name = FALSE), 
		right_annotation = rowAnnotation(quantile = anno_empty(border = FALSE, 
				width = grobWidth(textGrob("100%", gp = quantile_gp)) + unit(6, "mm")),
			show_annotation_name = FALSE),
		...
	)

	random_str = paste(sample(c(letters, LETTERS, 0:9), 8), collapse = "")
	ht@name = paste0(ht@name, "_", random_str)
	names(ht@left_annotation) = paste0(names(ht@left_annotation), "_", random_str)
	names(ht@right_annotation) = paste0(names(ht@right_annotation), "_", random_str)

	post_fun = function(ht) {
		column_order = column_order(ht)

		decorate_annotation(paste0("axis_", random_str), {
			grid.text(ylab, x = grobHeight(textGrob(ylab, gp = ylab_gp)), rot = 90)
		})

		decorate_heatmap_body(paste0("density_", random_str), {
			pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(min_x, max_x), clip = TRUE))
			for(i in seq_len(5)) {
				grid.lines(1:n, quantile_list[i, column_order], default.units = "native", gp = gpar(lty = 2))
			}
			grid.lines(1:n, mean_value[column_order], default.units = "native", gp = gpar(lty = 2, col = "darkred"))
			upViewport()
		})
		decorate_heatmap_body(paste0("density_", random_str), {
			pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(min_x, max_x), clip = FALSE))
			grid.rect(gp = gpar(fill = NA))
			grid.yaxis(gp = tick_label_gp)

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
	        grid.text(labels, unit(1, "npc") + rep(link_width, n2), h, default.units = "native", just = "left", gp = quantile_gp)
	        link_width = link_width - unit(1, "mm")
	        grid.segments(unit(rep(1, n2), "npc"), y, unit(1, "npc") + rep(link_width * (1/3), n2), y, default.units = "native")
	        grid.segments(unit(1, "npc") + rep(link_width * (1/3), n2), y, unit(1, "npc") + rep(link_width * (2/3), n2), h, default.units = "native")
	        grid.segments(unit(1, "npc") + rep(link_width * (2/3), n2), h, unit(1, "npc") + rep(link_width, n2), h, default.units = "native")

			upViewport()
		})
	}

	ht@heatmap_param$post_fun = post_fun

	ht_list = ht %v% NULL
	return(ht_list)
}
