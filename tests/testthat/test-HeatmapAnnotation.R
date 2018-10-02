

ha = HeatmapAnnotation(foo = 1:10)
ha


ha = HeatmapAnnotation(foo = cbind(1:10, 10:1))
ha
draw(ha, test = "matrix as column annotation")

ha = HeatmapAnnotation(foo = 1:10, bar = sample(c("a", "b"), 10, replace = TRUE),
	pt = anno_points(1:10), annotation_name_side = "left")
draw(ha, test = "complex annotations")

ha = HeatmapAnnotation(foo = 1:10, bar = sample(c("a", "b"), 10, replace = TRUE),
	pt = anno_points(1:10), annotation_name_side = "left", height = unit(8, "cm"))
draw(ha, test = "complex annotations")


ha = HeatmapAnnotation(foo = 1:10, bar = sample(c("a", "b"), 10, replace = TRUE))

ha = HeatmapAnnotation(foo = 1:10, 
	bar = cbind(1:10, 10:1),
	pt = anno_points(1:10),
	gap = unit(2, "mm"))
draw(ha, test = "complex annotations")

ha2 = resize(ha, annotation_height = unit(1:3, "cm"))
draw(ha2, test = "complex annotations")
ha2 = resize(ha, annotation_height = 1, height = unit(6, "cm"))
draw(ha2, test = "complex annotations")
ha2 = resize(ha, annotation_height = 1:3, height = unit(6, "cm"))
draw(ha2, test = "complex annotations")
ha2 = resize(ha, annotation_height = unit(c(1, 2, 3), c("null", "null", "cm")), height = unit(6, "cm"))
draw(ha2, test = "complex annotations")
ha2 = resize(ha, annotation_height = unit(c(2, 2, 3), c("cm", "null", "cm")), height = unit(6, "cm"))
draw(ha2, test = "complex annotations")
ha2 = resize(ha, annotation_height = unit(c(2, 2, 3), c("cm", "cm", "cm")))
draw(ha2, test = "complex annotations")
ha2 = resize(ha[, 1:2], annotation_height = 1, height = unit(4, "cm"))
draw(ha2, test = "complex annotations")
ha2 = resize(ha[, 1:2], annotation_height = c(1, 4), height = unit(4, "cm"))
draw(ha2, test = "complex annotations")
ha2 = resize(ha[, 1:2], height = unit(6, "cm"))
draw(ha2, test = "complex annotations")

ha2 = resize(ha, height = unit(6, "cm"))
draw(ha2, test = "complex annotations")

#### test anno_empty and self-defined anotation function
ha = HeatmapAnnotation(foo = anno_empty(), height = unit(4, "cm"))
draw(ha, 1:10, test = "anno_empty")
ha = HeatmapAnnotation(foo = anno_empty(), bar = 1:10, height = unit(4, "cm"))
draw(ha, 1:10, test = "anno_empty")

ha = HeatmapAnnotation(foo = function(index) {grid.rect()}, bar = 1:10 height = unit(4, "cm"))
draw(ha, 1:10, test = "self-defined function")


lt = lapply(1:10, function(x) cumprod(1 + runif(1000, -x/100, x/100)) - 1)
ha = HeatmapAnnotation(foo = 1:10, bar = sample(c("a", "b"), 10, replace = TRUE),
	anno = anno_horizon(lt), which = "row")
draw(ha, test = "complex annotations on row")

## test row annotation with no heatmap
rowAnnotation(foo = 1:10, bar = anno_points(10:1))


HeatmapAnnotation(1:10)

HeatmapAnnotation(data.frame(1:10))
