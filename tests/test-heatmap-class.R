
source("Heatmap-class.R")
source("ColorMapping-class.R")
source("utils.R")

mat = matrix(rnorm(40, 2), 4, 10)
mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
rownames(mat) = letters[1:8]
colnames(mat) = letters[1:10]

ht = Heatmap(mat)
ht$draw()

ht = Heatmap(mat, col = colorRamp2(c(-3, 0, 3), c("green", "white", "red")))
ht$draw()
ht = Heatmap(mat, name = "test")
ht$draw()
ht = Heatmap(mat, title = "blablabla")
ht$draw()
ht = Heatmap(mat, title = "blablabla", title_side = "bottom")
ht$draw()
ht = Heatmap(mat, title = "blablabla", title_gp = gpar(fontsize = 20, fontface = "bold"))
ht$draw()
ht = Heatmap(mat, cluster_rows = FALSE)
ht$draw()
ht = Heatmap(mat, clustering_distance_rows = "pearson")
ht$draw()
ht = Heatmap(mat, clustering_distance_rows = function(x) dist(x))
ht$draw()
ht = Heatmap(mat, clustering_distance_rows = function(x, y) 1 - cor(x, y))
ht$draw()
ht = Heatmap(mat, clustering_method_rows = "single")
ht$draw()
ht = Heatmap(mat, row_hclust_side = "right")
ht$draw()
ht = Heatmap(mat, row_hclust_width = unit(1, "cm"))
ht$draw()
ht = Heatmap(mat, rownames_side = "left")
ht$draw()
ht = Heatmap(mat, show_rownames = FALSE)
ht$draw()
ht = Heatmap(mat, rownames_gp = gpar(fontsize = 20))
ht$draw()

annotation = data.frame(type = c(rep("A", 5), rep("B", 5)))
rownames(annotation) = colnames(mat)




