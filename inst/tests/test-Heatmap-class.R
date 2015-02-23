
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
ht = Heatmap(mat, row_names_side = "left", row_hclust_side = "right")
draw(ht, test = TRUE)
ht = Heatmap(mat, show_row_names = FALSE)
draw(ht, test = TRUE)
ht = Heatmap(mat, row_names_gp = gpar(fontsize = 20))
draw(ht, test = TRUE)

ht = Heatmap(mat, km = 2)
draw(ht, test = TRUE)
ht = Heatmap(mat, split = rep(c("A", "B"), 4))
draw(ht, test = TRUE)
ht = Heatmap(mat, split = data.frame(rep(c("A", "B"), 4), rep(c("C", "D"), each = 4)))
draw(ht, test = TRUE)


annotation = HeatmapAnnotation(df = data.frame(type = c(rep("A", 5), rep("B", 5))))
ht = Heatmap(mat, top_annotation = annotation)
draw(ht, test = TRUE)


annotation = HeatmapAnnotation(df = data.frame(type1 = rep(c("A", "B"), 5), type2 = rep(c("C", "D"), each = 5)))
ht = Heatmap(mat, bottom_annotation = annotation)
draw(ht, test = TRUE)

annotation = data.frame(value = rnorm(10))
rownames(annotation) = colnames(mat)
annotation = HeatmapAnnotation(df = annotation)
ht = Heatmap(mat, top_annotation = annotation)
draw(ht, test = TRUE)

annotation = data.frame(value = rnorm(10))
value = 1:10
anno_points = function(index) {
	n = length(index)
	pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = range(value)))
	grid.points(seq_along(index), value[index])
	upViewport()
}
ha = HeatmapAnnotation(df = annotation, points = anno_points)
ht = Heatmap(mat, top_annotation = ha, bottom_annotation = ha)
draw(ht, test = TRUE)

# character matrix
mat = matrix(sample(letters[1:6], 100, replace = TRUE), 10, 10)
ht = Heatmap(mat)
draw(ht, test = TRUE)
