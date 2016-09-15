
library(circlize)
library(grid)
library(RColorBrewer)


mat = matrix(rnorm(80, 2), 8, 10)
mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
rownames(mat) = letters[1:12]
colnames(mat) = letters[1:10]

ht = Heatmap(mat)
draw(ht, test = TRUE)

ht = Heatmap(mat, col = colorRamp2(c(-3, 0, 3), c("green", "white", "red")))
draw(ht, test = TRUE)

ht = Heatmap(mat, name = "test")
draw(ht, test = TRUE)

ht = Heatmap(mat, column_title = "blablabla")
draw(ht, test = TRUE)

ht = Heatmap(mat, row_title = "blablabla")
draw(ht, test = TRUE)

ht = Heatmap(mat, column_title = "blablabla", column_title_side = "bottom")
draw(ht, test = TRUE)

ht = Heatmap(mat, column_title = "blablabla", column_title_gp = gpar(fontsize = 20, fontface = "bold"))
draw(ht, test = TRUE)

ht = Heatmap(mat, cluster_rows = FALSE)
draw(ht, test = TRUE)

ht = Heatmap(mat, clustering_distance_rows = "pearson")
draw(ht, test = TRUE)

ht = Heatmap(mat, clustering_distance_rows = function(x) dist(x))
draw(ht, test = TRUE)

ht = Heatmap(mat, clustering_distance_rows = function(x, y) 1 - cor(x, y))
draw(ht, test = TRUE)

ht = Heatmap(mat, clustering_method_rows = "single")
draw(ht, test = TRUE)

ht = Heatmap(mat, row_hclust_side = "right")
draw(ht, test = TRUE)

ht = Heatmap(mat, row_hclust_width = unit(1, "cm"))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_names_side = "left", row_hclust_side = "right", column_names_side = "top", column_hclust_side = "bottom")
draw(ht, test = TRUE)

ht = Heatmap(mat, show_row_names = FALSE)
draw(ht, test = TRUE)

mat2 = mat
rownames(mat2) = NULL
colnames(mat2) = NULL
ht = Heatmap(mat2)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_names_gp = gpar(fontsize = 20))
draw(ht, test = TRUE)

ht = Heatmap(mat, km = 2)
draw(ht, test = TRUE)

ht = Heatmap(mat, split = rep(c("A", "B"), 6))
draw(ht, test = TRUE)

ht = Heatmap(mat, split = data.frame(rep(c("A", "B"), 6), rep(c("C", "D"), each = 6)))
draw(ht, test = TRUE)

ht = Heatmap(mat, split = data.frame(rep(c("A", "B"), 6), rep(c("C", "D"), each = 6)), 
	combined_name_fun = function(x) paste(x, collapse = "\n"))
draw(ht, test = TRUE)


annotation = HeatmapAnnotation(df = data.frame(type = c(rep("A", 6), rep("B", 6))))
ht = Heatmap(mat, top_annotation = annotation)
draw(ht, test = TRUE)


annotation = HeatmapAnnotation(df = data.frame(type1 = rep(c("A", "B"), 6), type2 = rep(c("C", "D"), each = 6)))
ht = Heatmap(mat, bottom_annotation = annotation)
draw(ht, test = TRUE)


annotation = data.frame(value = rnorm(10))
annotation = HeatmapAnnotation(df = annotation)
ht = Heatmap(mat, top_annotation = annotation)
draw(ht, test = TRUE)

annotation = data.frame(value = rnorm(10))
value = 1:10
anno_points = function(index) {
	n = length(index)
	pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = range(value)))
	grid.points(seq_along(index), value[index], pch = 16, gp = gpar(fontsize = unit(5, "mm")))
	upViewport()
}
ha = HeatmapAnnotation(df = annotation, points = anno_points, annotation_height = c(1, 2))
ht = Heatmap(mat, top_annotation = ha, top_annotation_height = unit(2, "cm"), bottom_annotation = ha)
draw(ht, test = TRUE)

# character matrix
mat3 = matrix(sample(letters[1:6], 100, replace = TRUE), 10, 10)
rownames(mat3) = {x = letters[1:10]; x[1] = "aaaaaaaaaaaaaaaaaaaaaaa";x}
ht = Heatmap(mat3, rect_gp = gpar(col = "white"))
draw(ht, test = TRUE)

mat = matrix(1:9, 3, 3)
rownames(mat) = letters[1:3]
colnames(mat) = letters[1:3]

ht = Heatmap(mat, rect_gp = gpar(col = "white"), cell_fun = function(i, j, x, y, width, height) grid.text(mat[i, j], x = x, y = y),
	cluster_rows = FALSE, cluster_columns = FALSE, row_names_side = "left", column_names_side = "top")
draw(ht, test = TRUE)

##############
## matrix with zero column_hclust_side
mat0 = matrix(nrow = 12, ncol = 0)
rownames(mat0) = letters[1:12]
Heatmap(mat) + Heatmap(mat0)
