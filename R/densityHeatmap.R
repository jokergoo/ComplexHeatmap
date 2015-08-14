
# == title
# Use colors to represent density distribution
#
# == param
# -data  a matrix or a list. If it is a matrix, density will be calculated by columns.
# -col a list of colors that density values are scaled to.
# -anno annotation for matrix columns or list. The value should be a vector or a data frame. 
#       It can also be a `HeatmapAnnotation-class` object.
# -ylab label on y-axis in the plot
# -title title of the plot
#
# == details
# To visualize distribution of columns in a matrix or in a list, sometimes we use boxplot or beanplot.
# Here we use colors to map to the density values and visualize distribution of values
# in each column (or each element in the list) through a heatmap. It is useful if you have huge number of columns in ``data`` to visualize.
#
# == value
# No value is returned.
#
# == example
# matrix = matrix(rnorm(100), 10); colnames(matrix) = letters[1:10]
# densityHeatmap(matrix)
# densityHeatmap(matrix, anno = rep(c("A", "B"), each = 5))
# densityHeatmap(matrix, col = c("white", "red"), anno = rep(c("A", "B"), each = 5))
#
# ha = HeatmapAnnotation(points = anno_points(runif(10)))
# densityHeatmap(matrix, anno = ha)
#
# lt = list(rnorm(10), rnorm(10))
# densityHeatmap(lt)
#
densityHeatmap = function(data, col = rev(brewer.pal(11, "Spectral")),
	anno = NULL, ylab = deparse(substitute(data)), 
	title = paste0("Density heatmap of ", deparse(substitute(data)))) {

	if(is.matrix(data)) {
		density_list = apply(data, 2, density)
		quantile_list = apply(data, 2, quantile)
	} else if(is.data.frame(data) || is.list(data)) {
		density_list = lapply(data, density)
		quantile_list = sapply(data, quantile)
	} else {
		stop("only matrix and list are allowed.")
	}

	n = length(density_list)
	nm = names(density_list)

	max_x = max(unlist(lapply(density_list, function(x) x$x)))
	min_x = min(unlist(lapply(density_list, function(x) x$x)))
	
	x = seq(min_x, max_x, length = 500)

	mat = lapply(density_list, function(r) {
			f = approxfun(r$x, r$y)
			res = f(x)
			res[is.na(res)] = 0
			rev(res)
		})
	mat = as.matrix(as.data.frame(mat))
	colnames(mat) = nm

	if(is.null(anno)) {
		ht = Heatmap(mat, col = col, name = "density", cluster_rows = FALSE, cluster_columns = FALSE)
	} else if(inherits(anno, "HeatmapAnnotation")) {
		ht = Heatmap(mat, col = col, top_annotation = anno, name = "density", cluster_rows = FALSE, cluster_columns = FALSE)
	} else {
		if(!is.data.frame(anno)) anno = data.frame(anno = anno)
		ha = HeatmapAnnotation(df = anno)
		ht = Heatmap(mat, col = col, top_annotation = ha, name = "density", cluster_rows = FALSE, cluster_columns = FALSE)
	}

	bb = grid.pretty(c(min_x, max_x))
	ht_list = rowAnnotation(axis = function(index) NULL, width = grobHeight(textGrob(ylab))*2 + max(grobWidth(textGrob(bb))) + unit(6, "mm")) + 
		ht + rowAnnotation(quantile = function(index) NULL, width = grobWidth(textGrob("100%")))

	draw(ht_list, column_title = title)

	decorate_annotation("axis", {
		grid.text(ylab, x = grobHeight(textGrob(ylab)), rot = 90)
	}, slice = 1)

	decorate_heatmap_body("density", {
		pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(min_x, max_x)))
		grid.rect(gp = gpar(fill = NA))
		grid.yaxis()
		for(i in seq_len(5)) {
			grid.lines(1:n, quantile_list[i, ], default.units = "native", gp = gpar(lty = 2))
			grid.text(rownames(quantile_list)[i], unit(1, "npc")+unit(2, "mm"), quantile_list[i, n], default.units = "native", just = "left")
		}
		upViewport()
	})
}
