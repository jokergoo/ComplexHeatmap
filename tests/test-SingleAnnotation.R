library(circlize)
library(ComplexHeatmap)
library(GetoptLong)

ha = SingleAnnotation(value = 1:10)
draw(ha, test = "single column annotation")
ha = SingleAnnotation(value = 1:10, which = "row")
draw(ha, test = "single row annotation")
ha = SingleAnnotation(value = 1:10)
draw(ha, index = 6:10, test = "single column annotation, subset")
draw(ha, index = 6:10, k = 1, n = 2, test = "single column annotation, subset, k=1 n=2")
draw(ha, index = 6:10, k = 2, n = 2, test = "single column annotation, subset, k=1 n=2")

x = 1:10
ha = SingleAnnotation(value = x)
draw(ha, test = "single column annotation")

m = cbind(1:10, 10:1)
colnames(m) = c("a", "b")
ha = SingleAnnotation(value = m)
draw(ha, test = "matrix as column annotation")

ha = SingleAnnotation(value = 1:10, col = colorRamp2(c(1, 10), c("blue", "red")))
draw(ha, test = "color mapping function")

ha = SingleAnnotation(value = c(rep(c("a", "b"), 5)))
draw(ha, test = "discrete annotation")
ha = SingleAnnotation(value = c(rep(c("a", "b"), 5)), col = c("a" = "red", "b" = "blue"))
draw(ha, test = "discrete annotation with defined colors")

anno = anno_simple(1:10)
ha = SingleAnnotation(fun = anno)
draw(ha, test = "AnnotationFunction as input")

anno = anno_barplot(matrix(nc = 2, c(1:10, 10:1)))
ha = SingleAnnotation(fun = anno)
draw(ha, test = "anno_barplot as input")
draw(ha, index = 1:5, test = "anno_barplot as input, 1:5")
draw(ha, index = 1:5, k = 1, n = 2, test = "anno_barplot as input, 1:5, k = 1, n = 2")
draw(ha, index = 1:5, k = 2, n = 2, test = "anno_barplot as input, 1:5, k = 2, n = 2")

lt = lapply(1:20, function(x) cumprod(1 + runif(1000, -x/100, x/100)) - 1)
anno = anno_horizon(lt, which = "row")
ha = SingleAnnotation(fun = anno, which = "row")
draw(ha, test = "anno_horizon as input")

fun = local({
	value = 1:10
	function(index, k = 1, n = 1) {
		pushViewport(viewport(xscale = c(0.5, length(index) + 0.5), yscale = range(value)))
		grid.points(seq_along(index), value[index])
		grid.rect()
		if(k == 1) grid.yaxis()
		popViewport()
	}
})
ha = SingleAnnotation(fun = fun, height = unit(4, "cm"))
# ha[1:5]
draw(ha, index = c(1, 4, 2, 6), test = "self-defined function")
draw(ha, index = c(1, 4, 2, 6), k = 1, n = 2, test = "self-defined function, k = 1, n = 2")
draw(ha, index = c(1, 4, 2, 6), k = 2, n = 2, test = "self-defined function, k = 2, n = 2")


# test gridtext
ha = SingleAnnotation(value = 1:10, label = gt_render("foo", r = unit(2, "pt")), name_gp = gpar(box_fill = "red"))
draw(ha, test = "single column annotation")


