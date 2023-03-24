
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
# -show_quantiles Whether show quantile lines.
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
# -mc.cores Multiple cores for calculating ks distance. This argument will be removed in future versions.
# -cores Multiple cores for calculating ks distance.
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
	ylim = NULL,
	range = ylim,

	title_gp = gpar(fontsize = 14),
	ylab_gp = gpar(fontsize = 12),
	tick_label_gp = gpar(fontsize = 10),
	quantile_gp = gpar(fontsize = 10),
	show_quantiles = TRUE,

	column_order = NULL,
	column_names_side = "bottom",
	show_column_names = TRUE,
	column_names_max_height = unit(6, "cm"),
	column_names_gp = gpar(fontsize = 12),
	column_names_rot = 90,

	cluster_columns = FALSE,
	clustering_distance_columns = "ks",
	clustering_method_columns = "complete",
	mc.cores = 1, cores = mc.cores,

	...) {

	arg_list = list(...)
	if(length(arg_list)) {
		if(any(c("row_km", "row_split", "split", "km") %in% names(arg_list))) {
			stop_wrap("density heatmaps do not allow row splitting.")
		}
		if(any(grepl("row", names(arg_list)))) {
			stop_wrap("density heatmaps do not allow to set rows.")
		}
		if("anno" %in% names(arg_list)) {
			stop_wrap("`anno` is removed from the argument. Please directly construct a `HeatmapAnnotation` object and set to `top_annotation` or `bottom_annotation`.")
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

	if(!is.null(range)) {
		max_x = range[2]
		min_x = range[1]
	}
	
	x = seq(min_x, max_x, length.out = 500)

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
			d = ks_dist(mat, cores = cores)

			dend = as.dendrogram(hclust(d, clustering_method_columns))
			dend = reorder(dend, colMeans(mat))
			cluster_columns = dend
		}
	}

	if(inherits(col, "function")) {
		col = col(mat)
	} else {
		col = colorRamp2(seq(0, quantile(mat, 0.99, na.rm = TRUE), length.out = length(col)), col, space = color_space)
	}

	bb = grid.pretty(c(min_x, max_x))
	ht = Heatmap(mat, col = col, name = "density", 
		column_title = title,
		column_title_gp = title_gp,
		cluster_rows = FALSE, 
		cluster_columns = cluster_columns,
		clustering_distance_columns = clustering_distance_columns,
		clustering_method_columns = clustering_method_columns,
		column_dend_reorder = mean_value,
		column_names_side = column_names_side,
		show_column_names = show_column_names,
		column_names_max_height = column_names_max_height,
		column_names_gp = column_names_gp,
		column_names_rot = column_names_rot,
		column_order = column_order,
		left_annotation = rowAnnotation(axis = anno_empty(border = FALSE, 
				width = grobHeight(textGrob(ylab, gp = ylab_gp))*2 + max_text_width(bb, gp = tick_label_gp) + unit(4, "mm")),
			show_annotation_name = FALSE), 
		right_annotation = {if(show_quantiles) {rowAnnotation(quantile = anno_empty(border = FALSE, 
				width = grobWidth(textGrob("100%", gp = quantile_gp)) + unit(6, "mm")),
			show_annotation_name = FALSE)} else NULL},
		...
	)

	random_str = paste(sample(c(letters, LETTERS, 0:9), 8), collapse = "")
	ht@name = paste0(ht@name, "_", random_str)
	names(ht@left_annotation) = paste0(names(ht@left_annotation), "_", random_str)
	if(show_quantiles) {
		names(ht@right_annotation) = paste0(names(ht@right_annotation), "_", random_str)
	}

	post_fun = function(ht) {
		column_order = column_order(ht)
		if(!is.list(column_order)) {
			column_order = list(column_order)
		}
		n_slice = length(column_order)

		decorate_annotation(paste0("axis_", random_str), {
			grid.text(ylab, x = grobHeight(textGrob(ylab, gp = ylab_gp)), rot = 90)
		}, slice = 1)

		if(!is.null(ht@right_annotation)) {
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
		}

		decorate_heatmap_body(paste0("density_", random_str), {
			pushViewport(viewport(yscale = c(min_x, max_x), clip = FALSE))
			grid.rect(gp = gpar(fill = NA))
			grid.yaxis(gp = tick_label_gp)
			upViewport()
		}, column_slice = 1)

		if(!is.null(ht@right_annotation)) {
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
				text_height = convertHeight(grobHeight(textGrob(labels[1])) * 2, "native", valueOnly = TRUE)
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
	}

	ht@heatmap_param$post_fun = post_fun
	ht@heatmap_param$type = "densityHeatmap"

	ht_list = ht
	return(ht_list)
}

# https://stackoverflow.com/a/29853834/3425904
ks_dist_pair = function(x, y) {
	# if(length(x) > 5000) x = sample(x, 5000)
	# if(length(y) > 5000) y = sample(y, 5000)
	n <- length(x)
    n.x <- as.double(n)
    n.y <- length(y)
    n <- n.x * n.y/(n.x + n.y)
    w <- c(x, y)
    z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))
    max(abs(z))
}

# data: a list or a matrix
ks_dist = function(data, cores = 1) {
	has_names = TRUE
	if(is.matrix(data)) {
		has_names = !is.null(colnames(data))
		data = as.data.frame(data)
	}

    nc = length(data)

	ind_mat = expand.grid(seq_len(nc), seq_len(nc))
	ind_mat = ind_mat[  ind_mat[, 1] > ind_mat[, 2], , drop = FALSE]
	
	# Ensures that .libPaths() in each cluster is the same as the main node
	# Refer to: https://www.r-bloggers.com/2020/12/how-to-set-library-path-on-a-parallel-r-cluster/
	cl <- makeCluster(cores)

	lp = .libPaths()
	clusterExport(cl, "lp", envir = environment())
	clusterEvalQ(cl, .libPaths(lp))

	registerDoParallel(cl)
	v <- foreach (ind = seq_len(nrow(ind_mat))) %dopar% {
		i = ind_mat[ind, 1]
		j = ind_mat[ind, 2]
		suppressWarnings(d <- ks_dist_pair(data[[i]], data[[j]]))
		return(d)
	}
	stopCluster(cl)

	v = unlist(v)

	i = ind_mat[, 1]
	j = ind_mat[, 2]

	
    ind = (j - 1) * nc + i
    d = matrix(0, nrow = nc, ncol = nc)
    if(has_names) rownames(d) = colnames(d) = names(data)

    d[ind] = v
    as.dist(d)
}

# m = matrix(rnorm(200), nc = 10)
# ks_dist(m, mc.cores = 1)
# ks_dist(m, mc.cores = 2)
# ks_dist_1(m)
# lt = lapply(1:10, function(i) rnorm(runif(1, min = 10, max = 20)))
# ks_dist(lt, mc.cores = 1)
# ks_dist(lt, mc.cores = 2)
# ks_dist_1(lt)
ks_dist_1 = function(data) {
	has_names = TRUE
	if(is.matrix(data)) {
		has_names = !is.null(colnames(data))
		data = as.data.frame(data)
	}
	nc = length(data)
    d = matrix(NA, nrow = nc, ncol = nc)
    if(has_names) rownames(d) = colnames(d) = names(data)

    for(i in 2:nc) {
        for(j in 1:(nc-1)) {
            suppressWarnings(d[i, j] <- ks_dist_pair(data[[i]], data[[j]]))
        }
    }

    as.dist(d)
}

# == title
# Visualize Frequency Distribution by Heatmap
#
# == param
# -data A matrix or a list. If it is a matrix, density is calculated by columns.
# -breaks Pass to `graphics::hist`. Please only set equal bin size.
# -stat Statistic to use.
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
# -column_order Order of columns.
# -column_names_side Pass to `Heatmap`.
# -show_column_names Pass to `Heatmap`.
# -column_names_max_height Pass to `Heatmap`.
# -column_names_gp Pass to `Heatmap`.
# -column_names_rot Pass to `Heatmap`.
# -cluster_columns Whether cluster columns?
# -use_3d Whether to visualize the frequencies as a 3D heatmap with `Heatmap3D`?
# -... Pass to `Heatmap` or `Heatmap3D` (if ``use_3d = TRUE``).
#
# == value
# A `Heatmap-class` object. It can oly add other heatmaps/annotations vertically.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# matrix = matrix(rnorm(100), 10); colnames(matrix) = letters[1:10]
# frequencyHeatmap(matrix)
# frequencyHeatmap(matrix, use_3d = TRUE)
frequencyHeatmap = function(data, 
	breaks = "Sturges",
	stat = c("count", "density", "proportion"),
	
	col = brewer.pal(9, "Blues"),
	color_space = "LAB", 
	ylab = deparse(substitute(data)),
	column_title = paste0("Frequency heatmap of ", deparse(substitute(data))),
	title = column_title,
	ylim = NULL,
	range = ylim,

	title_gp = gpar(fontsize = 14),
	ylab_gp = gpar(fontsize = 12),
	tick_label_gp = gpar(fontsize = 10),

	column_order = NULL,
	column_names_side = "bottom",
	show_column_names = TRUE,
	column_names_max_height = unit(6, "cm"),
	column_names_gp = gpar(fontsize = 12),
	column_names_rot = 90,
	cluster_columns = FALSE,

	use_3d = FALSE,
	...) {

	arg_list = list(...)
	if(length(arg_list)) {
		if(any(c("row_km", "row_split", "split", "km") %in% names(arg_list))) {
			stop_wrap("frequency heatmaps do not allow row splitting.")
		}
		if(any(grepl("row", names(arg_list)))) {
			stop_wrap("frequency heatmaps do not allow to set rows.")
		}
		if("anno" %in% names(arg_list)) {
			stop_wrap("`anno` is removed from the argument. Please directly construct a `HeatmapAnnotation` object and set to `top_annotation` or `bottom_annotation`.")
		}
	}

	ylab = ylab
	column_title = column_title

	if(!is.matrix(data) && !is.data.frame(data) && !is.list(data)) {
		stop_wrap("only matrix and list are allowed.")
	}
	if(is.matrix(data)) {
		data2 = as.list(as.data.frame(data))
		names(data2) = colnames(data)
		data = data2
	}

	h = hist(unlist(data), breaks = breaks, plot = FALSE)
	breaks = h$breaks

	min_x = min(breaks)
	max_x = max(breaks)

	freq_list = lapply(data, function(x) hist(x, plot = FALSE, breaks = breaks))

	n = length(freq_list)
	nm = names(freq_list)

	stat = match.arg(stat)[1]
	if(stat == "count") {
		mat = lapply(freq_list, function(x) {
			rev(x$count)
		})
	} else if(stat == "proportion") {
		mat = lapply(freq_list, function(x) {
			rev(x$count)/sum(x$count)
		})
	} else if(stat == "density") {
		mat = lapply(freq_list, function(x) {
			rev(x$density)
		})	
	}
	mat = as.matrix(as.data.frame(mat))
	colnames(mat) = nm

	if(inherits(col, "function")) {
		col = col(mat)
	} else {
		col = colorRamp2(seq(0, quantile(mat, 0.99, na.rm = TRUE), length.out = length(col)), col, space = color_space)
	}

	bb = grid.pretty(c(min_x, max_x))

		
	if(use_3d) {
		ht = Heatmap3D(mat, col = col, name = stat, 
			column_title = title,
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
			...
		)
	} else {
		ht = Heatmap(mat, col = col, name = stat, 
			column_title = title,
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
			...
		)
	}

	random_str = paste(sample(c(letters, LETTERS, 0:9), 8), collapse = "")
	ht@name = paste0(ht@name, "_", random_str)
	names(ht@left_annotation) = paste0(names(ht@left_annotation), "_", random_str)

	post_fun = function(ht) {
		column_order = column_order(ht)
		if(!is.list(column_order)) {
			column_order = list(column_order)
		}
		n_slice = length(column_order)

		decorate_annotation(paste0("axis_", random_str), {
			grid.text(ylab, x = grobHeight(textGrob(ylab, gp = ylab_gp)), rot = 90)
		}, slice = 1)

		decorate_heatmap_body(paste0(stat, "_", random_str), {
			pushViewport(viewport(yscale = c(min_x, max_x), clip = FALSE))
			grid.segments(0, 0, 0, 1)
			grid.yaxis(gp = tick_label_gp)
			upViewport()
		}, column_slice = 1)

	}

	ht@heatmap_param$post_fun = post_fun
	ht@heatmap_param$type = "frequencyHeatmap"

	ht_list = ht
	return(ht_list)
}
