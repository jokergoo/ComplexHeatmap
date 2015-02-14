
library(circlize)
library(grid)
library(RColorBrewer)


mat = matrix(rnorm(40, 2), 4, 10)
mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
rownames(mat) = letters[1:8]
colnames(mat) = letters[1:10]

ht = Heatmap(mat)
draw(ht)
ht = Heatmap(mat, col = colorRamp2(c(-3, 0, 3), c("green", "white", "red")))
draw(ht)
ht = Heatmap(mat, name = "test")
draw(ht)
ht = Heatmap(mat, column_title = "blablabla")
draw(ht)
ht = Heatmap(mat, row_title = "blablabla")
draw(ht)
ht = Heatmap(mat, column_title = "blablabla", column_title_side = "bottom")
draw(ht)
ht = Heatmap(mat, column_title = "blablabla", column_title_gp = gpar(fontsize = 20, fontface = "bold"))
draw(ht)
ht = Heatmap(mat, cluster_rows = FALSE)
draw(ht)
ht = Heatmap(mat, clustering_distance_rows = "pearson")
draw(ht)
ht = Heatmap(mat, clustering_distance_rows = function(x) dist(x))
draw(ht)
ht = Heatmap(mat, clustering_distance_rows = function(x, y) 1 - cor(x, y))
draw(ht)
ht = Heatmap(mat, clustering_method_rows = "single")
draw(ht)
ht = Heatmap(mat, row_hclust_side = "right")
draw(ht)
ht = Heatmap(mat, row_hclust_width = unit(1, "cm"))
draw(ht)
ht = Heatmap(mat, rownames_side = "left", row_hclust_side = "right")
draw(ht)
ht = Heatmap(mat, show_rownames = FALSE)
draw(ht)
ht = Heatmap(mat, rownames_gp = gpar(fontsize = 20))
draw(ht)

annotation = data.frame(type = c(rep("A", 5), rep("B", 5)))
rownames(annotation) = colnames(mat)
ht = Heatmap(mat, annotation)


annotation = data.frame(type1 = rep(c("A", "B"), 5), type2 = rep(c("C", "D"), each = 5))
rownames(annotation) = colnames(mat)
ht = Heatmap(mat, annotation)


# character matrix
mat = matrix(sample(letters[1:6], 100, replace = TRUE), 10, 10)
ht = Heatmap(mat, cluster_rows = FALSE, cluster_columns = FALSE)
draw(ht)
