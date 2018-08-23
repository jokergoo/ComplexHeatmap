
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
anno = AnnotationFunction(fun = fun, var_imported = "x")
anno = AnnotationFunction(fun = fun, var_imported = list(x))


devAskNewPage(ask = TRUE)

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

######## test anno_points #####
anno = anno_points(runif(10))
draw(anno, test = "anno_points")
anno = anno_points(matrix(runif(20), nc = 2), pch = 1:2)
draw(anno, test = "matrix")
anno = anno_points(c(1:5, 1:5))
draw(anno, test = "anno_points")
anno = anno_points(cbind(c(1:5, 1:5), c(5:1, 5:1)), gp = gpar(col = 2:3))
draw(anno, test = "matrix")


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

devAskNewPage(ask = TRUE)
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


###### anno_mark ###
anno = anno_mark(at = c(1:4, 20, 60, 97:100), labels = month.name[1:10], which = "row")
draw(anno, index = 1:100, test = "anno_mark")

m = matrix(1:1000, byrow = TRUE, nr = 100)
anno = anno_mark(at = c(1:4, 20, 60, 97:100), labels = month.name[1:10], which = "row")
Heatmap(m, cluster_rows = F, cluster_columns = F) + rowAnnotation(mark = anno)
Heatmap(m) + rowAnnotation(mark = anno)


