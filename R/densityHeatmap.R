
# == title
# use colors to represent density distribution
#
# == param
# -data  a matrix or a list. If it is a matrix, density will be calculated by columns
# -col a list of colors that density values are to be mapped to
# -anno annotation for matrix columns or list, a vector or a data frame
# -ylab label on y-axis in the plot
# -title title of the plot
#
# == details
# To visualize distribution of columns in a matrix or in a list, sometimes we use boxplot or beanplot.
# Here we use colors to map to the density values and visualize distribution of values
# in each column (or each list element) through a heatmap.
#
# == example
# matrix = matrix(rnorm(100), 10); colnames(matrix) = letters[1:10]
# densityHeatmap(matrix)
# densityHeatmap(matrix, anno = rep(c("A", "B"), each = 5))
# densityHeatmap(matrix, col = c("white", "red"), anno = rep(c("A", "B"), each = 5))
#
# lt = list(rnorm(10), rnorm(10))
# densityHeatmap(lt)
#
densityHeatmap = function(data, col = rev(brewer.pal(11, "Spectral")),
	anno = NULL, ylab = deparse(substitute(data)), 
	title = paste0("density heatmap of ", deparse(substitute(data)))) {

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
	} else {
		if(!is.data.frame(anno)) anno = data.frame(anno = anno)
		ha = HeatmapAnnotation(df = anno)
		ht = Heatmap(mat, col = col, top_annotation = ha, name = "density", cluster_rows = FALSE, cluster_columns = FALSE)
	}

	bb = grid.pretty(c(min_x, max_x))
	ht_list = rowAnnotation(axis = function(index) NULL, width = grobHeight(textGrob(ylab))*2 + max(grobWidth(textGrob(bb))) + unit(6, "mm")) + 
		ht + rowAnnotation(quantile = function(index) NULL, width = grobWidth(textGrob("100%")))

	draw(ht_list, column_title = title)

	seekViewport("annotation_axis")
	grid.text(ylab, x = grobHeight(textGrob(ylab)), rot = 90)

	seekViewport("density_heatmap_body_1")
	pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(min_x, max_x)))
	grid.rect(gp = gpar(fill = NA))
	grid.yaxis()
	for(i in seq_len(5)) {
		grid.lines(1:n, quantile_list[i, ], default.units = "native", gp = gpar(lty = 2))
		grid.text(rownames(quantile_list)[i], unit(1, "npc")+unit(2, "mm"), quantile_list[i, n], default.units = "native", just = "left")
	}
	upViewport()
}
