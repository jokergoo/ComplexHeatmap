

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

ha2 = re_size(ha, annotation_height = unit(1:3, "cm"))
draw(ha2, test = "complex annotations")
ha2 = re_size(ha, annotation_height = 1, height = unit(6, "cm"))
draw(ha2, test = "complex annotations")
ha2 = re_size(ha, annotation_height = 1:3, height = unit(6, "cm"))
draw(ha2, test = "complex annotations")
ha2 = re_size(ha, annotation_height = unit(c(1, 2, 3), c("null", "null", "cm")), height = unit(6, "cm"))
draw(ha2, test = "complex annotations")
ha2 = re_size(ha, annotation_height = unit(c(2, 2, 3), c("cm", "null", "cm")), height = unit(6, "cm"))
draw(ha2, test = "complex annotations")
ha2 = re_size(ha, annotation_height = unit(c(2, 2, 3), c("cm", "cm", "cm")))
draw(ha2, test = "complex annotations")
ha2 = re_size(ha[, 1:2], annotation_height = 1, height = unit(4, "cm"))
draw(ha2, test = "complex annotations")
ha2 = re_size(ha[, 1:2], annotation_height = c(1, 4), height = unit(4, "cm"))
draw(ha2, test = "complex annotations")
ha2 = re_size(ha[, 1:2], height = unit(6, "cm"))
draw(ha2, test = "complex annotations")

ha2 = re_size(ha, height = unit(6, "cm"))
draw(ha2, test = "complex annotations")

#### test anno_empty and self-defined anotation function
ha = HeatmapAnnotation(foo = anno_empty(), height = unit(4, "cm"))
draw(ha, 1:10, test = "anno_empty")
ha = HeatmapAnnotation(foo = anno_empty(), bar = 1:10, height = unit(4, "cm"))
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



ha = HeatmapAnnotation(summary = anno_summary(height = unit(4, "cm")))
v = sample(letters[1:2], 50, replace = TRUE)
split = sample(letters[1:2], 50, replace = TRUE)

Heatmap(v, top_annotation = ha, width = unit(1, "cm"), split = split)

ha = HeatmapAnnotation(summary = anno_summary(gp = gpar(fill = 2:3), height = unit(4, "cm")))
v = rnorm(50)
Heatmap(v, top_annotation = ha, width = unit(1, "cm"), split = split)



### auto adjust
m = matrix(rnorm(100), 10)
Heatmap(m, top_annotation = HeatmapAnnotation(foo = 1:10), column_dend_height = unit(4, "cm")) +
Heatmap(m, top_annotation = HeatmapAnnotation(bar = anno_points(1:10)),
	cluster_columns = FALSE)

fun = function(index) {
	grid.rect()
}
ha = HeatmapAnnotation(fun = fun, height = unit(4, "cm"))
draw(ha, 1:10, test = TRUE)

ha = rowAnnotation(fun = fun, width = unit(4, "cm"))
draw(ha, 1:10, test = TRUE)


## test anno_mark
m = matrix(rnorm(1000), nrow = 100)
ha1 = rowAnnotation(foo = anno_mark(at = c(1:4, 20, 60, 97:100), labels = month.name[1:10]))
Heatmap(m, name = "mat", cluster_rows = FALSE, right_annotation = ha1)
Heatmap(m, name = "mat", cluster_rows = FALSE) + ha1

split = rep("a", 100); split[c(1:4, 20, 60, 98:100)] = "b"
Heatmap(m, name = "mat", cluster_rows = FALSE, right_annotation = ha1, row_split = split, gap = unit(1, "cm"))
Heatmap(m, name = "mat", cluster_rows = FALSE, row_split = split, gap = unit(1, "cm")) + ha1

# ha has two annotations
ha2 = rowAnnotation(foo = anno_mark(at = c(1:4, 20, 60, 97:100), labels = month.name[1:10]), bar = 1:100)
Heatmap(m, name = "mat", cluster_rows = FALSE, right_annotation = ha2)
Heatmap(m, name = "mat", cluster_rows = FALSE) + ha2

Heatmap(m, name = "mat", cluster_rows = FALSE, right_annotation = ha2, row_split = split, gap = unit(1, "cm"))
Heatmap(m, name = "mat", cluster_rows = FALSE, row_split = split, gap = unit(1, "cm")) + ha2


## test anno_mark as column annotation
m = matrix(rnorm(1000), ncol = 100)
ha1 = columnAnnotation(foo = anno_mark(at = c(1:4, 20, 60, 97:100), labels = month.name[1:10]))
Heatmap(m, name = "mat", cluster_columns = FALSE, top_annotation = ha1)
ha1 %v% Heatmap(m, name = "mat", cluster_columns = FALSE)

split = rep("a", 100); split[c(1:4, 20, 60, 98:100)] = "b"
Heatmap(m, name = "mat", cluster_columns = FALSE, top_annotation = ha1, column_split = split, column_gap = unit(1, "cm"))
ha1 %v% Heatmap(m, name = "mat", cluster_columns = FALSE, column_split = split, gap = unit(1, "cm"))

# ha has two annotations
ha2 = HeatmapAnnotation(foo = anno_mark(at = c(1:4, 20, 60, 97:100), labels = month.name[1:10]), bar = 1:100)
Heatmap(m, name = "mat", cluster_columns = FALSE, top_annotation = ha2)
ha2 %v% Heatmap(m, name = "mat", cluster_columns = FALSE)

Heatmap(m, name = "mat", cluster_columns = FALSE, top_annotation = ha2, column_split = split, column_gap = unit(1, "cm"))
ha2 %v% Heatmap(m, name = "mat", cluster_columns = FALSE, column_split = split, column_gap = unit(1, "cm"))



### when there are only simple annotations
col_fun = colorRamp2(c(0, 10), c("white", "blue"))
ha = HeatmapAnnotation(
    foo = cbind(a = 1:10, b = 10:1), 
    bar = sample(letters[1:3], 10, replace = TRUE),
    col = list(foo = col_fun,
               bar = c("a" = "red", "b" = "green", "c" = "blue")
    ),
    simple_anno_size = unit(1, "cm")
)
draw(ha, test = TRUE)

set.seed(123)
mat1 = matrix(rnorm(80, 2), 8, 10)
mat1 = rbind(mat1, matrix(rnorm(40, -2), 4, 10))
rownames(mat1) = paste0("R", 1:12)
colnames(mat1) = paste0("C", 1:10)

mat2 = matrix(runif(60, max = 3, min = 1), 6, 10)
mat2 = rbind(mat2, matrix(runif(60, max = 2, min = 0), 6, 10))
rownames(mat2) = paste0("R", 1:12)
colnames(mat2) = paste0("C", 1:10)

ind = sample(12, 12)
mat1 = mat1[ind, ]
mat2 = mat2[ind, ]

ha1 = HeatmapAnnotation(foo1 = 1:10, 
	                    annotation_height = unit(1, "cm"),
	                    simple_anno_size_adjust = TRUE,
                        annotation_name_side = "left")
ha2 = HeatmapAnnotation(df = data.frame(foo1 = 1:10,
                                        foo2 = 1:10,
                                        foo4 = 1:10,
                                        foo5 = 1:10))
ht1 = Heatmap(mat1, name = "rnorm", top_annotation = ha1)
ht2 = Heatmap(mat2, name = "runif", top_annotation = ha2)

ht1 + ht2

##### test size of a single simple annotation

ha = HeatmapAnnotation(foo1 = 1:10, 
	simple_anno_size = unit(1, "cm")
)
ha = HeatmapAnnotation(foo1 = 1:10, 
	annotation_height = unit(1, "cm"),
	simple_anno_size_adjust = TRUE
)
ha = HeatmapAnnotation(foo1 = 1:10, 
	height = unit(1, "cm"),
	simple_anno_size_adjust = TRUE
)


## annotation with the same names

set.seed(123)
m = matrix(rnorm(100), 10)
ha1 = HeatmapAnnotation(foo = sample(c("a", "b"), 10, replace = TRUE))
ha2 = HeatmapAnnotation(foo = sample(c("b", "c"), 10, replace = TRUE))

Heatmap(m, top_annotation = ha1) + 
Heatmap(m, top_annotation = ha2)


ha1 = HeatmapAnnotation(foo = sample(c("a", "b"), 10, replace = TRUE),
	annotation_legend_param = list(
		foo = list(title = "letters", 
			       at = c("a", "b", "c"),
			       labels = c("A", "B", "C")
			  )
	))
ha2 = HeatmapAnnotation(foo = sample(c("b", "c"), 10, replace = TRUE))

Heatmap(m, top_annotation = ha1) + 
Heatmap(m, top_annotation = ha2)


library(ComplexHeatmap)
x = matrix(rnorm(6), ncol=3)
subtype_col = c("Basal" = "purple","Her2" = "black","Normal" = "blue")
h1 <- HeatmapAnnotation("Subtype" = c("Basal","Her2", "Normal"),
                        col = list("Subtype" = subtype_col))
h2 <- HeatmapAnnotation("Subtype" = c("Normal","Normal", "Basal"),
                        col = list("Subtype" = subtype_col))

Heatmap(x,top_annotation = h1) + Heatmap(x,top_annotation = h2)
