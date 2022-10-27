library(circlize)
library(ComplexHeatmap)
library(GetoptLong)

if(!exists("normalize_graphic_param_to_mat")) {
	normalize_graphic_param_to_mat = ComplexHeatmap:::normalize_graphic_param_to_mat
}

if(!exists("height")) {
	height = ComplexHeatmap:::height
}

if(!exists("width")) {
	width = ComplexHeatmap:::width
}

normalize_graphic_param_to_mat(1, nc = 2, nr = 4, "foo")
normalize_graphic_param_to_mat(1:2, nc = 2, nr = 4, "foo")
normalize_graphic_param_to_mat(1:4, nc = 2, nr = 4, "foo")

### AnnotationFunction constructor #####
fun = function(index) {
	x = runif(10)
	pushViewport(viewport(xscale = c(0.5, 10.5), yscale = c(0, 1)))
	grid.points(index, x[index])
	popViewport()
}
anno = AnnotationFunction(fun = fun)

x = runif(10)
fun = function(index) {
	pushViewport(viewport(xscale = c(0.5, 10.5), yscale = c(0, 1)))
	grid.points(index, x[index])
	popViewport()
}
anno = AnnotationFunction(fun = fun, var_import = "x")
anno = AnnotationFunction(fun = fun, var_import = list(x))


x = runif(10)
cell_fun = function(i) {
	pushViewport(viewport(yscale = c(0, 1)))
	grid.points(unit(0.5, "npc"), x[i])
	popViewport()
}
anno = AnnotationFunction(cell_fun = cell_fun, var_import = "x")
ha = HeatmapAnnotation(foo = anno)
draw(ha, 1:10, test = T)

cell_fun = function(i) {
	pushViewport(viewport(xscale = c(0, 1)))
	grid.points(x[i], unit(0.5, "npc"))
	popViewport()
}
anno = AnnotationFunction(cell_fun = cell_fun, var_import = "x", which = "row")
ha = rowAnnotation(foo = anno)
draw(ha, 1:10, test = T)

# devAskNewPage(ask = dev.interactive())

########### testing anno_simple ############
anno = anno_simple(1:10)
draw(anno, test = "as a simple vector")
draw(anno[1:5], test = "subset of column annotation")
anno = anno_simple(1:10, which = "row")
draw(anno, test = "as row annotation")
draw(anno[1:5], test = "subste of row annotation")

anno = anno_simple(1:10, col = structure(rand_color(10), names = 1:10))
draw(anno, test = "self-define colors")

anno = anno_simple(1:10, border = TRUE)
draw(anno, test = "border")
anno = anno_simple(1:10, gp = gpar(col = "red"))
draw(anno, test = "gp for the grids")

anno = anno_simple(c(1:9, NA))
draw(anno, test = "vector has NA values")

anno = anno_simple(cbind(1:10, 10:1))
draw(anno, test = "a matrix")
draw(anno[1:5], test = "subste of a matrix")

anno = anno_simple(1:10, pch = 1, pt_gp = gpar(col = "red"), pt_size = unit(seq(1, 10), "mm"))
draw(anno, test = "with symbols + pt_gp + pt_size")
anno = anno_simple(1:10, pch = 1:10)
draw(anno, test = "pch is a vector")
anno = anno_simple(1:10, pch = c(1:4, NA, 6:8, NA, 10, 11))
draw(anno, test = "pch has NA values")

anno = anno_simple(cbind(1:10, 10:1), pch = 1, pt_gp = gpar(col = "blue"))
draw(anno, test = "matrix with symbols")
anno = anno_simple(cbind(1:10, 10:1), pch = 1:2)
draw(anno, test = "matrix, length of pch is number of annotations")
anno = anno_simple(cbind(1:10, 10:1), pch = 1:10)
draw(anno, test = "matrix, length of pch is length of samples")
anno = anno_simple(cbind(1:10, 10:1), pch = matrix(1:20, nc = 2))
draw(anno, test = "matrix, pch is a matrix")
pch = matrix(1:20, nc = 2)
pch[sample(length(pch), 10)] = NA
anno = anno_simple(cbind(1:10, 10:1), pch = pch)
draw(anno, test = "matrix, pch is a matrix with NA values")


####### test anno_empty ######
anno = anno_empty()
draw(anno, test = "anno_empty")
anno = anno_empty(border = FALSE)
draw(anno, test = "anno_empty without border")

if(0) {
###### test anno_image #####
image1 = sample(dir("~/Downloads/IcoMoon-Free-master/PNG/64px", full.names = TRUE), 10)
anno = anno_image(image1)
draw(anno, test = "png")
draw(anno[1:5], test = "subset of png")
anno = anno_image(image1, which = "row")
draw(anno, test = "png on rows")
image2 = sample(dir("~/Downloads/IcoMoon-Free-master/SVG/", full.names = TRUE), 10)
anno = anno_image(image2)
draw(anno, test = "svg")
image3 = sample(dir("~/Downloads/IcoMoon-Free-master/EPS/", full.names = TRUE), 10)
anno = anno_image(image3)
draw(anno, test = "eps")
image4 = sample(dir("~/Downloads/IcoMoon-Free-master/PDF/", full.names = TRUE), 10)
anno = anno_image(image4)
draw(anno, test = "pdf")

anno = anno_image(c(image1[1:3], image2[1:3], image3[1:3], image4[1:3]))
draw(anno, test = "png+svg+eps+pdf")

anno = anno_image(image1, gp = gpar(fill = 1:10, col = "black"))
draw(anno, test = "png + gp")
draw(anno[1:5], test = "png + gp")

anno = anno_image(image1, space = unit(3, "mm"))
draw(anno, test = "space")

image1[1] = ""
anno = anno_image(image1)
draw(anno, test = "png")
}

######## test anno_points #####
anno = anno_points(runif(10))
draw(anno, test = "anno_points")
anno = anno_points(matrix(runif(20), nc = 2), pch = 1:2)
draw(anno, test = "matrix")
anno = anno_points(c(1:5, 1:5))
draw(anno, test = "anno_points")
anno = anno_points(cbind(c(1:5, 1:5), c(5:1, 5:1)), gp = gpar(col = 2:3))
draw(anno, test = "matrix")

anno = anno_points(1:10, gp = gpar(col = rep(2:3, each = 5)), pch = rep(2:3, each = 5))
draw(anno, test = "anno_points")
draw(anno, index = c(1, 3, 5, 7, 9, 2, 4, 6, 8, 10), test = "anno_points")

anno = anno_points(c(1:5, NA, 7:10))
draw(anno, test = "anno_points")


anno = anno_points(runif(10), axis_param = list(direction = "reverse"), ylim = c(0, 1))
draw(anno, test = "anno_points")

anno = anno_points(runif(10), axis_param = list(direction = "reverse"), ylim = c(0, 1), which = "row")
draw(anno, test = "anno_points")

# pch as image
if(0) {
image1 = sample(dir("/desktop-home/guz/Downloads/IcoMoon-Free-master/PNG/64px", full.names = TRUE), 10)
x = runif(10)
anno1 = anno_points(x, pch = image1, pch_as_image = TRUE, size = unit(5, "mm"), height = unit(4, "cm"))
anno2 = anno_points(x, height = unit(4, "cm"))
draw(anno1, test = "anno_points")
draw(anno2, test = "anno_points")
}

##### test anno_lines ###
anno = anno_lines(runif(10))
draw(anno, test = "anno_lines")
anno = anno_lines(cbind(c(1:5, 1:5), c(5:1, 5:1)), gp = gpar(col = 2:3))
draw(anno, test = "matrix")
anno = anno_lines(cbind(c(1:5, 1:5), c(5:1, 5:1)), gp = gpar(col = 2:3),
	add_points = TRUE, pt_gp = gpar(col = 5:6), pch = c(1, 16))
draw(anno, test = "matrix")
anno = anno_lines(sort(rnorm(10)), height = unit(2, "cm"), smooth = TRUE, add_points = TRUE)
draw(anno, test = "anno_lines, smooth")
anno = anno_lines(cbind(sort(rnorm(10)), sort(rnorm(10), decreasing = TRUE)), 
	height = unit(2, "cm"), smooth = TRUE, add_points = TRUE, gp = gpar(col = 2:3))
draw(anno, test = "anno_lines, smooth, matrix")

anno = anno_lines(sort(rnorm(10)), width = unit(2, "cm"), smooth = TRUE, add_points = TRUE, which = "row")
draw(anno, test = "anno_lines, smooth, by row")
anno = anno_lines(cbind(sort(rnorm(10)), sort(rnorm(10), decreasing = TRUE)), 
	width = unit(2, "cm"), smooth = TRUE, add_points = TRUE, gp = gpar(col = 2:3), which = "row")
draw(anno, test = "anno_lines, smooth, matrix, by row")

anno = anno_lines(c(1:5, NA, 7:10))
draw(anno, test = "anno_lines")

anno = anno_lines(runif(10), axis_param = list(direction = "reverse"))
draw(anno, test = "anno_lines")

###### test anno_text #######
anno = anno_text(month.name)
draw(anno, test = "month names")
anno = anno_text(month.name, gp = gpar(fontsize = 16))
draw(anno, test = "month names with fontsize")
anno = anno_text(month.name, gp = gpar(fontsize = 1:12+4))
draw(anno, test = "month names with changing fontsize")
anno = anno_text(month.name, which = "row")
draw(anno, test = "month names on rows")
anno = anno_text(month.name, location = 0, rot = 45, just = "left", gp = gpar(col = 1:12))
draw(anno, test = "with rotations")
anno = anno_text(month.name, location = 1, rot = 45, just = "right", gp = gpar(fontsize = 1:12+4))
draw(anno, test = "with rotations")


for(rot in seq(0, 360, by = 45)) {
	anno = anno_text(month.name, which = "row", location = 0, rot = rot, 
		just = "left")
	draw(anno, test = paste0("rot =", rot))
}


##### test anno_barplot #####
anno = anno_barplot(1:10)
draw(anno, test = "a vector")
draw(anno[1:5], test = "a vector, subset")
anno = anno_barplot(1:10, which = "row")
draw(anno, test = "a vector")
anno = anno_barplot(1:10, bar_width = 1)
draw(anno, test = "bar_width")
anno = anno_barplot(1:10, gp = gpar(fill = 1:10))
draw(anno, test = "fill colors")

anno = anno_barplot(matrix(nc = 2, c(1:10, 10:1)))
draw(anno, test = "a matrix")
draw(anno[1:5], test = "a matrix, subset")
anno = anno_barplot(matrix(nc = 2, c(1:10, 10:1)), which = "row")
draw(anno, test = "a matrix, on rows")
anno = anno_barplot(matrix(nc = 2, c(1:10, 10:1)), gp = gpar(fill = 2:3, col = 2:3))
draw(anno, test = "a matrix with fill")

m = matrix(runif(4*10), nc = 4)
m = t(apply(m, 1, function(x) x/sum(x)))
anno = anno_barplot(m)
draw(anno, test = "proportion matrix")
anno = anno_barplot(m, gp = gpar(fill = 2:5), bar_width = 1, height = unit(6, "cm"))
draw(anno, test = "proportion matrix")

anno = anno_barplot(c(1:5, NA, 7:10))
draw(anno, test = "a vector")

anno = anno_barplot(1:10, which = "row", axis_param = list(direction = "reverse"))
draw(anno, test = "a vector")

anno = anno_barplot(1:10, baseline = 5, which = "row", axis_param = list(direction = "reverse"))
draw(anno, test = "a vector")

anno = anno_barplot(matrix(nc = 2, c(1:10, 10:1)), which = "row", axis_param = list(direction = "reverse"))
draw(anno, test = "a vector")


anno = anno_barplot(matrix(nc = 2, c(1:10, 10:1)), beside = TRUE)
draw(anno, test = "a matrix")
draw(anno[1:5], test = "a matrix, subset")
anno = anno_barplot(matrix(nc = 2, c(1:10, 10:1)), beside = TRUE, which = "row")
draw(anno, test = "a matrix, on rows")
anno = anno_barplot(matrix(nc = 2, c(1:10, 10:1)), beside = TRUE, gp = gpar(fill = 2:3, col = 2:3))
draw(anno, test = "a matrix with fill")


##### test anno_boxplot #####
set.seed(123)
m = matrix(rnorm(100), 10)
anno = anno_boxplot(m, height = unit(4, "cm"))
draw(anno, test = "anno_boxplot")
draw(anno[1:5], test = "subset")
anno = anno_boxplot(m, height = unit(4, "cm"), gp = gpar(fill = 1:10))
draw(anno, test = "anno_boxplot with gp")
anno = anno_boxplot(m, height = unit(4, "cm"), box_width = 0.9)
draw(anno, test = "anno_boxplot with box_width")

m = matrix(rnorm(100), 10)
m[1, ] = NA
anno = anno_boxplot(m, height = unit(4, "cm"))
draw(anno, test = "anno_boxplot")


####### test anno_joyplot ####
m = matrix(rnorm(1000), nc = 10)
lt = apply(m, 2, function(x) data.frame(density(x)[c("x", "y")]))
anno = anno_joyplot(lt, width = unit(4, "cm"), which = "row")
draw(anno, test = "joyplot")
anno = anno_joyplot(lt, width = unit(4, "cm"), which = "row", gp = gpar(fill = 1:10))
draw(anno, test = "joyplot + col")
anno = anno_joyplot(lt, width = unit(4, "cm"), which = "row", scale = 1)
draw(anno, test = "joyplot + scale")

m = matrix(rnorm(5000), nc = 50)
lt = apply(m, 2, function(x) data.frame(density(x)[c("x", "y")]))
anno = anno_joyplot(lt, width = unit(4, "cm"), which = "row", gp = gpar(fill = NA), scale = 4)
draw(anno, test = "joyplot")

######## test anno_horizon ######
lt = lapply(1:20, function(x) cumprod(1 + runif(1000, -x/100, x/100)) - 1)
anno = anno_horizon(lt, which = "row")
draw(anno, test = "horizon chart")
anno = anno_horizon(lt, which = "row", gp = gpar(pos_fill = "orange", neg_fill = "darkgreen"))
draw(anno, test = "horizon chart, col")
anno = anno_horizon(lt, which = "row", negative_from_top = TRUE)
draw(anno, test = "horizon chart + negative_from_top")
anno = anno_horizon(lt, which = "row", gap = unit(1, "mm"))
draw(anno, test = "horizon chart + gap")
anno = anno_horizon(lt, which = "row", gp = gpar(pos_fill = rep(c("orange", "red"), each = 10),
	neg_fill = rep(c("darkgreen", "blue"), each = 10)))
draw(anno, test = "horizon chart, col")

####### test anno_histogram ####
m = matrix(rnorm(1000), nc = 10)
anno = anno_histogram(t(m), which = "row")
draw(anno, test = "row histogram")
draw(anno[1:5], test = "subset row histogram")
anno = anno_histogram(t(m), which = "row", gp = gpar(fill = 1:10))
draw(anno, test = "row histogram with color")
anno = anno_histogram(t(m), which = "row", n_breaks = 20)
draw(anno, test = "row histogram with color")
m[1, ] = NA
anno = anno_histogram(t(m), which = "row")
draw(anno, test = "row histogram")


####### test anno_density ######
anno = anno_density(t(m), which = "row")
draw(anno, test = "normal density")
draw(anno[1:5], test = "normal density, subset")
anno = anno_density(t(m), which = "row", type = "violin")
draw(anno, test = "violin")
anno = anno_density(t(m), which = "row", type = "heatmap")
draw(anno, test = "heatmap")
anno = anno_density(t(m), which = "row", type = "heatmap", heatmap_colors = c("white", "orange"))
draw(anno, test = "heatmap, colors")

anno = anno_density(t(m), which = "row", xlim = c(-2, 2))
draw(anno, test = "normal density")
anno = anno_density(t(m), which = "row", type = "violin", xlim = c(-2, 2))
draw(anno, test = "violin")
anno = anno_density(t(m), which = "row", type = "heatmap", xlim = c(-2, 2))
draw(anno, test = "heatmap")

###### anno_mark ###
if(0) {
library(gridtext)
grid.text = function(text, x = 0.5, y = 0.5, gp = gpar(), rot = 0, default.units = "npc", just = "center") {
	if(length(just) == 1) {
		if(just == "center") {
			just = c("center", "center")
		} else if(just == "bottom") {
			just = c("center", "bottom")
		} else if (just == "top") {
			just = c("center", "top")
		} else if(just == "left") {
			just = c("left", "center")
		} else if(just == "right") {
			just = c("right", "center")
		}
	}
	just2 = c(0.5, 0.5)
	if(is.character(just)) {
		just2[1] = switch(just[1], "center" = 0.5, "left" = 0, "right" = 1)
		just2[2] = switch(just[2], "center" = 0.5, "bottom" = 0, "top" = 1)
	}
	gb = richtext_grob(text, x = x, y = y, gp = gpar(fontsize = 10), box_gp = gpar(col = "black"),
		default.units = default.units, hjust = just2[1], vjust = just2[2], rot = rot)
	grid.draw(gb)
}
}
anno = anno_mark(at = c(1:4, 20, 60, 97:100), labels = month.name[1:10], which = "row")
draw(anno, index = 1:100, test = "anno_mark")

anno = anno_mark(at = c(1:4, 20, 60, 97:100), labels = month.name[1:10], labels_rot = 30, which = "column")
draw(anno, index = 1:100, test = "anno_mark")

m = matrix(1:1000, byrow = TRUE, nr = 100)
anno = anno_mark(at = c(1:4, 20, 60, 97:100), labels = month.name[1:10], which = "row", labels_rot = 30)
Heatmap(m, cluster_rows = F, cluster_columns = F) + rowAnnotation(mark = anno)
Heatmap(m) + rowAnnotation(mark = anno)

ht_list = Heatmap(m, cluster_rows = F, cluster_columns = F) + rowAnnotation(mark = anno)
draw(ht_list, row_split = c(rep("a", 95), rep("b", 5)))


grid.newpage()
pushViewport(viewport(x = 0.45, w = 0.7, h = 0.95))
h = unit(0, "mm")
for(rot in seq(0, 360, by = 30)[-13]) {
	anno = anno_mark(at = c(1:4, 20, 60, 97:100), labels = strrep(letters[1:10], 4), labels_rot = rot, which = "column", side = "bottom")
	h = h + height(anno)
	pushViewport(viewport(y = h, height = height(anno), just = "top"))
	grid.rect()
	draw(anno, index = 1:100)
	grid::grid.text(qq("labels_rot = @{rot}"), unit(1, "npc") + unit(2, "mm"), just = "left")
	popViewport()
}


grid.newpage()
pushViewport(viewport(w = 0.9, h = 0.9))
w = unit(0, "mm")
for(rot in seq(0, 360, by = 30)) {
	anno = anno_mark(at = c(1:4, 20, 60, 97:100), labels = strrep(letters[1:10], 4), labels_rot = rot, which = "row", side = "left")
	w = w + width(anno)
	pushViewport(viewport(x = w, width = width(anno), just = "right"))
	grid.rect()
	draw(anno, index = 1:100)
	popViewport()
}



### graphic parameters after reordering
index = c(1, 3, 5, 7, 9, 2, 4, 6, 8, 10)
anno = anno_simple(1:10, pch = 1:10, pt_gp = gpar(col = rep(c(1, 2), each = 5)),
	pt_size = unit(1:10, "mm"))
draw(anno, index, test = "a numeric vector")
anno = anno_simple(1:10, pch = 1:10, pt_gp = gpar(col = rep(c(1, 2), each = 5)),
	pt_size = unit(1:10, "mm"), which = "row")
draw(anno, index, test = "a numeric vector")


anno = anno_points(1:10, pch = 1:10, gp = gpar(col = rep(c(1, 2), each = 5)),
	size = unit(1:10, "mm"))
draw(anno, index, test = "a numeric vector")
anno = anno_points(1:10, pch = 1:10, gp = gpar(col = rep(c(1, 2), each = 5)),
	size = unit(1:10, "mm"), which = "row")
draw(anno, index, test = "a numeric vector")


anno = anno_lines(sort(runif(10)), pch = 1:10, pt_gp = gpar(col = rep(c(1, 2), each = 5)),
	size = unit(1:10, "mm"), add_points = TRUE)
draw(anno, index, test = "a numeric vector")
anno = anno_lines(sort(runif(10)), pch = 1:10, pt_gp = gpar(col = rep(c(1, 2), each = 5)),
	size = unit(1:10, "mm"), add_points = TRUE, which = "row")
draw(anno, index, test = "a numeric vector")


anno = anno_barplot(1:10, gp = gpar(fill = rep(c(1, 2), each = 5)))
draw(anno, index, test = "a numeric vector")
anno = anno_barplot(1:10, gp = gpar(fill = rep(c(1, 2), each = 5)), which = "row")
draw(anno, index, test = "a numeric vector")

anno = anno_barplot(cbind(1:10, 10:1), gp = gpar(fill = 1:2))
draw(anno, index, test = "a numeric vector")
anno = anno_barplot(cbind(1:10, 10:1), gp = gpar(fill = 1:2), which = "row")
draw(anno, index, test = "a numeric vector")


m = matrix(rnorm(100), 10)
m = m[, order(apply(m, 2, median))]
anno = anno_boxplot(m, pch = 1:10, gp = gpar(fill = rep(c(1, 2), each = 5)),
	size = unit(1:10, "mm"), height = unit(4, "cm"))
draw(anno, index, test = "a numeric vector")
anno = anno_boxplot(t(m), pch = 1:10, gp = gpar(fill = rep(c(1, 2), each = 5)),
	size = unit(1:10, "mm"), which = "row", width = unit(4, "cm"))
draw(anno, index, test = "a numeric vector")

anno = anno_histogram(m, gp = gpar(fill = rep(c(1, 2), each = 5)))
draw(anno, index, test = "a numeric vector")
anno = anno_histogram(t(m), gp = gpar(fill = rep(c(1, 2), each = 5)), which = "row")
draw(anno, index, test = "a numeric vector")

anno = anno_density(m, gp = gpar(fill = rep(c(1, 2), each = 5)))
draw(anno, index, test = "a numeric vector")
anno = anno_density(t(m), gp = gpar(fill = rep(c(1, 2), each = 5)), which = "row")
draw(anno, index, test = "a numeric vector")


anno = anno_density(m, type = "violin", gp = gpar(fill = rep(c(1, 2), each = 5)))
draw(anno, index, test = "a numeric vector")
anno = anno_density(t(m), type = "violin", gp = gpar(fill = rep(c(1, 2), each = 5)), which = "row")
draw(anno, index, test = "a numeric vector")


anno = anno_text(month.name, gp = gpar(col = rep(c(1, 2), each = 5)))
draw(anno, index, test = "a numeric vector")
anno = anno_text(month.name, gp = gpar(col = rep(c(1, 2), each = 5)), which= "row")
draw(anno, index, test = "a numeric vector")

lt = lapply(1:10, function(x) cumprod(1 + runif(1000, -x/100, x/100)) - 1)
anno = anno_horizon(lt, gp = gpar(pos_fill = rep(c(1, 2), each = 5), neg_fill = rep(c(3, 4), each = 5)), which = "row")
draw(anno, index, test = "a numeric vector")

m = matrix(rnorm(1000), nc = 10)
lt = apply(m, 2, function(x) data.frame(density(x)[c("x", "y")]))
anno = anno_joyplot(lt, gp = gpar(fill = rep(c(1, 2), each = 5)), 
	width = unit(4, "cm"), which = "row")
draw(anno, index, test = "joyplot")


anno = anno_block(gp = gpar(fill = 1:4))
draw(anno, index = 1:10, k = 1, n = 4, test = "anno_block")
draw(anno, index = 1:10, k = 2, n = 4, test = "anno_block")

anno = anno_block(gp = gpar(fill = 1:4), labels = letters[1:4], labels_gp = gpar(col = "white"))
draw(anno, index = 1:10, k = 2, n = 4, test = "anno_block")
draw(anno, index = 1:10, k = 4, n = 4, test = "anno_block")
# draw(anno, index = 1:10, k = 2, n = 2, test = "anno_block")

anno = anno_block(gp = gpar(fill = 1:4), labels = letters[1:4], labels_gp = gpar(col = "white"), which = "row")
draw(anno, index = 1:10, k = 2, n = 4, test = "anno_block")


### anno_zoom
fa = sort(sample(letters[1:3], 100, replace = TRUE, prob = c(1, 2, 3)))
panel_fun = function(index, nm) {
	grid.rect()
	grid.text(nm)
}
anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun)
draw(anno, index = 1:100, test = "anno_zoom")

anno = anno_zoom(align_to = list(a = which(fa == "a")), which = "row", panel_fun = panel_fun)
draw(anno, index = 1:100, test = "anno_zoom")


panel_fun = function(index, nm) {
	grid.rect(gp = gpar(fill = "grey", col = NA))
	grid.text(nm)
}

anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun, link_gp = gpar(fill = "grey", col = "black"), internal_line = FALSE)
draw(anno, index = 1:100, test = "anno_zoom")


anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun,
	gap = unit(1, "cm"))
draw(anno, index = 1:100, test = "anno_zoom, set gap")

anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun,
	size = 1:3)
draw(anno, index = 1:100, test = "anno_zoom, size set as relative values")

anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun,
	size = 1:3, extend = unit(1, "cm"))
draw(anno, index = 1:100, test = "anno_zoom, extend")

anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun,
	size = unit(1:3, "cm"))
draw(anno, index = 1:100, test = "anno_zoom, size set as absolute values")

anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun,
	size = unit(c(2, 20, 40), "cm"))
draw(anno, index = 1:100, test = "anno_zoom, big size")

anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun,
	size = 1:3, gap = unit(1, "cm"))
draw(anno, index = 1:100, test = "anno_zoom, size set as relative values, gap")

anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun,
	size = unit(1:3, "cm"), gap = unit(1, "cm"))
draw(anno, index = 1:100, test = "anno_zoom, size set as absolute values, gap")


anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun,
	size = unit(1:3, "cm"), side = "left")
draw(anno, index = 1:100, test = "anno_zoom, side")


anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun,
	size = unit(1:3, "cm"), link_gp = gpar(fill = 1:3))
draw(anno, index = 1:100, test = "anno_zoom, link_gp")

anno = anno_zoom(align_to = fa, which = "row", panel_fun = panel_fun,
	size = unit(1:3, "cm"), link_gp = gpar(fill = 1:3),
	link_width = unit(2, "cm"), width = unit(4, "cm"))
draw(anno, index = 1:100, test = "anno_zoom, width")

anno = anno_zoom(align_to = list(a = 1:10, b = 30:45, c = 70:90), 
	which = "row", panel_fun = panel_fun, size = unit(1:3, "cm"))
draw(anno, index = 1:100, test = "anno_zoom, a list of indices")

anno = anno_zoom(align_to = fa, which = "column", panel_fun = panel_fun,
	size = unit(1:3, "cm"))
draw(anno, index = 1:100, test = "anno_zoom, column annotation")


m = matrix(rnorm(100*10), nrow = 100)
hc = hclust(dist(m))
fa2 = cutree(hc, k = 4)
anno = anno_zoom(align_to = fa2, which = "row", panel_fun = panel_fun)
draw(anno, index = hc$order, test = "anno_zoom, column annotation")

anno = anno_zoom(align_to = fa2, which = "column", panel_fun = panel_fun)
draw(anno, index = hc$order, test = "anno_zoom, column annotation")


anno = anno_zoom(align_to = fa2, which = "row", panel_fun = panel_fun)
draw(Heatmap(m, cluster_rows = hc, right_annotation = rowAnnotation(foo = anno)))
draw(Heatmap(m, cluster_rows = hc, right_annotation = rowAnnotation(foo = anno), row_split = 2))


anno = anno_zoom(align_to = fa2, which = "row", panel_fun = panel_fun, size = unit(1:4, "cm"))
draw(Heatmap(m, cluster_rows = hc, right_annotation = rowAnnotation(foo = anno)))

set.seed(123)
m = matrix(rnorm(100*10), nrow = 100)
subgroup = sample(letters[1:3], 100, replace = TRUE, prob = c(1, 5, 10))
rg = range(m)
panel_fun = function(index, nm) {
	pushViewport(viewport(xscale = rg, yscale = c(0, 2)))
	grid.rect()
	grid.xaxis(gp = gpar(fontsize = 8))
	grid.boxplot(m[index, ], pos = 1, direction = "horizontal")
	grid.text(paste("distribution of group", nm), mean(rg), y = 1.9, 
		just = "top", default.units = "native", gp = gpar(fontsize = 10))
	popViewport()
}
anno = anno_zoom(align_to = subgroup, which = "row", panel_fun = panel_fun, 
	size = unit(2, "cm"), gap = unit(1, "cm"), width = unit(4, "cm"))
draw(Heatmap(m, right_annotation = rowAnnotation(foo = anno), row_split = subgroup))

panel_fun2 = function(index, nm) {
	pushViewport(viewport())
	grid.rect()
	n = floor(length(index)/4)
	txt = paste("gene function", 1:n, collapse = "\n")
	grid.text(txt, 0.95, 0.5, default.units = "npc", just = "right", gp = gpar(fontsize = 8))
	popViewport()
}
anno2 = anno_zoom(align_to = subgroup, which = "row", panel_fun = panel_fun2, 
	gap = unit(1, "cm"), width = unit(3, "cm"), side = "left")

draw(Heatmap(m, right_annotation = rowAnnotation(subgroup = subgroup, foo = anno,
	show_annotation_name = FALSE), 
	left_annotation = rowAnnotation(bar = anno2, subgroup = subgroup, show_annotation_name = FALSE),
	show_row_dend = FALSE,
	row_split = subgroup))

draw(Heatmap(m, right_annotation = rowAnnotation(foo = anno), 
	left_annotation = rowAnnotation(bar = anno2),
	show_row_dend = FALSE,
	row_split = subgroup))

set.seed(12345)
mat = matrix(rnorm(30*10), nr = 30)
row_split = c(rep("a", 10), rep("b", 5), rep("c", 2), rep("d", 3), 
	          rep("e", 2), letters[10:17])
row_split = factor(row_split)

panel_fun = function(index, name) {
	pushViewport(viewport())
	grid.rect()
	grid.text(name)
	popViewport()
}

anno = anno_zoom(align_to = row_split, which = "row", panel_fun = panel_fun, 
	size = unit(0.5, "cm"), width = unit(4, "cm"))

# > dev.size()
# [1] 3.938326 4.502203
dev.new(width = 3.938326, height = 4.502203)
draw(Heatmap(mat, right_annotation = rowAnnotation(foo = anno), 
	row_split = row_split))



#### anno_customize ###
x = sort(sample(letters[1:3], 10, replace = TRUE))
graphics = list(
	"a" = function(x, y, w, h) grid.points(x, y, pch = 16),
	"b" = function(x, y, w, h) grid.rect(x, y, w*0.8, h*0.8, gp = gpar(fill = "red")),
	"c" = function(x, y, w, h) grid.segments(x - 0.5*w, y - 0.5*h, x + 0.5*w, y + 0.5*h, gp = gpar(lty = 2))
)

anno = anno_customize(x, graphics = graphics)
draw(anno, index = 1:10, test = "")

anno = anno_customize(c(x, "d"), graphics = graphics)

### anno_numeric ##
x = runif(10)
anno = anno_numeric(x)
draw(anno, 1:10, test = TRUE)
anno = anno_numeric(x, align_to = "right")
draw(anno, 1:10, test = TRUE)


x = 10^(-runif(10, 1, 6))
anno = anno_numeric(x, x_convert = function(x) -log10(x), labels_format = function(x) sprintf("%.2e", x))
draw(anno, 1:10, test = TRUE)

x = runif(10, -1, 1)
anno = anno_numeric(x)
draw(anno, 1:10, test = TRUE)
anno = anno_numeric(x, labels_gp = gpar(col = c("green", "red")))
draw(anno, 1:10, test = TRUE)

anno = anno_numeric(x, bg_gp = gpar(col = c("green", "red")))
draw(anno, 1:10, test = TRUE)


x = runif(10, 0.5, 1.5)
anno = anno_numeric(x, align_to = 0)
draw(anno, 1:10, test = TRUE)




