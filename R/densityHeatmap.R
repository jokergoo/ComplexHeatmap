
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
# -ylim Ranges on the y-axis.
# -range Same as ``ylim``.
# -title_gp Graphic parameters for title.
# -ylab_gp Graphic parameters for y-labels.
# -tick_label_gp Graphic parameters for y-ticks.
# -quantile_gp Graphic parameters for the quantiles.
# -column_order Order of columns.
# -column_names_side Pass to `Heatmap`.
# -show_column_names Pass to `Heatmap`.
# -column_names_max_height Pass to `Heatmap`.
# -column_names_gp Pass to `Heatmap`.
# -column_names_rot Pass to `Heatmap`.
# -cluster_columns Whether cluster columns?
# -clustering_distance_columns There is a specific distance method ``ks`` which is the Kolmogorov-Smirnov statistic between two distributions.
#          For other methods, the distance is calculated on the density matrix.
# -clustering_method_columns Pass to `Heatmap`.
# -... Pass to `Heatmap`.
#
# == details
# To visualize data distribution in a matrix or in a list, we normally use
# boxplot or violinplot. We can also use colors to map the density values and
# visualize distribution of values through a heatmap. It is useful if you have
# huge number of columns in ``data`` to visualize.
#
# The density matrix is generated with 500 rows ranging between the maximun
# and minimal values in all densities. 
#
# == value
# A `Heatmap-class` object. It can oly add other heatmaps/annotations vertically.
#
# == seealso
# https://jokergoo.github.io/ComplexHeatmap-reference/book/other-high-level-plots.html#density-heatmap
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
# densityHeatmap(matrix, top_annotation = ha) \%v\% Heatmap(matrix, height = unit(6, "cm"))
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
	clustering_distance_columns = "ks",
	clustering_method_columns = "complete",

	...) {

	arg_list = list(...)
	if(length(arg_list)) {
		if(any(c("row_km", "row_split", "split", "km") %in% names(arg_list))) {
			stop_wrap("density heatmaps do not allow row splitting.")
		}
		if(any(grepl("row", names(arg_list)))) {
			stop_wrap("density heatmaps do not allow to set rows.")
		}
	}

	ylab = ylab
	column_title = column_title

	density_param$na.rm = TRUE

	if(!is.matrix(data) && !is.data.frame(data) && !is.list(data)) {
		stop_wrap("only matrix and list are allowed.")
	}
	if(is.matrix(data)) {
		data2 = as.list(as.data.frame(data))
		names(data2) = colnames(data)
		data = data2
	}
	density_list = lapply(data, function(x) do.call(density, c(list(x = x), density_param)))
	quantile_list = sapply(data, quantile, na.rm = TRUE)
	mean_value = sapply(data, mean, na.rm = TRUE)

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

	if(cluster_columns) {
		if(clustering_distance_columns == "ks") {
			nc = length(data)
		    d = matrix(NA, nrow = nc, ncol = nc)
		    rownames(d) = colnames(d) = rownames(d)

		    for(i in 2:nc) {
		        for(j in 1:(nc-1)) {
		            suppressWarnings(d[i, j] <- ks.test(data[[i]], data[[j]])$stat)
		        }
		    }

		    d = as.dist(d)

			hc = hclust(d, clustering_method_columns)
			cluster_columns = hc
		}
	}

	col = colorRamp2(seq(0, max(mat, na.rm = TRUE), length = length(col)), col, space = color_space)

	bb = grid.pretty(c(min_x, max_x))
	ht = Heatmap(mat, col = col, name = "density", 
		column_title = title,
		column_title_gp = title_gp,
		cluster_rows = FALSE, 
		cluster_columns = cluster_columns,
		clustering_distance_columns = clustering_distance_columns,
		clustering_method_columns = clustering_method_columns,
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
		if(!is.list(column_order)) {
			column_order = list(column_order)
		}
		n_slice = length(column_order)

		decorate_annotation(paste0("axis_", random_str), {
			grid.text(ylab, x = grobHeight(textGrob(ylab, gp = ylab_gp)), rot = 90)
		}, slice = 1)

		for(i_slice in 1:n_slice) {
			decorate_heatmap_body(paste0("density_", random_str), {
				n = length(column_order[[i_slice]])
				pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(min_x, max_x), clip = TRUE))
				for(i in seq_len(5)) {
					grid.lines(1:n, quantile_list[i, column_order[[i_slice]] ], default.units = "native", gp = gpar(lty = 2))
				}
				grid.lines(1:n, mean_value[ column_order[[i_slice]] ], default.units = "native", gp = gpar(lty = 2, col = "darkred"))
				upViewport()
			}, column_slice = i_slice)
		}

		decorate_heatmap_body(paste0("density_", random_str), {
			pushViewport(viewport(yscale = c(min_x, max_x), clip = FALSE))
			grid.rect(gp = gpar(fill = NA))
			grid.yaxis(gp = tick_label_gp)
			upViewport()
		}, column_slice = 1)

		decorate_heatmap_body(paste0("density_", random_str), {
			n = length(column_order[[n_slice]])
			
			lq = !apply(quantile_list, 1, function(x) all(x > max_x) || all(x < min_x))
			lq = c(lq, !(all(mean_value > max_x) || all(mean_value < min_x)))
			if(sum(lq) == 0) {
				return(NULL)
			}

			labels = c(rownames(quantile_list), "mean")
			y = c(quantile_list[, column_order[[n_slice]][n] ], mean_value[ column_order[[n_slice]][n] ])
			labels = labels[lq]
			y = y[lq]
			od = order(y)
			y = y[od]
			labels = labels[od]
			
			pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(min_x, max_x), clip = FALSE))
			text_height = convertHeight(grobHeight(textGrob(labels[1])) * (1 + 0.2), "native", valueOnly = TRUE)
	        h1 = y - text_height*0.5
	        h2 = y + text_height*0.5
	        pos = rev(smartAlign(h1, h2, c(min_x, max_x)))
	        h = (pos[, 1] + pos[, 2])/2
	        link_width = unit(6, "mm")
	        n2 = length(labels)
	        grid.text(labels, unit(1, "npc") + rep(link_width, n2), h, default.units = "native", just = "left", gp = quantile_gp)
	        link_width = link_width - unit(1, "mm")
	        ly = y <= max_x & y >= min_x
	        if(sum(ly)) {
		        grid.segments(unit(rep(1, n2), "npc")[ly], y[ly], unit(1, "npc") + rep(link_width * (1/3), n2)[ly], y[ly], default.units = "native")
		        grid.segments(unit(1, "npc") + rep(link_width * (1/3), n2)[ly], y[ly], unit(1, "npc") + rep(link_width * (2/3), n2)[ly], h[ly], default.units = "native")
		        grid.segments(unit(1, "npc") + rep(link_width * (2/3), n2)[ly], h[ly], unit(1, "npc") + rep(link_width, n2)[ly], h[ly], default.units = "native")
		    }
			upViewport()
		}, column_slice = n_slice)
	}

	ht@heatmap_param$post_fun = post_fun

	ht_list = ht %v% NULL
	return(ht_list)
}
