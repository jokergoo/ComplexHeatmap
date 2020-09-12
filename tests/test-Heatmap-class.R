library(circlize)
library(ComplexHeatmap)
library(GetoptLong)

set.seed(123)
nr1 = 10; nr2 = 8; nr3 = 6
nc1 = 6; nc2 = 8; nc3 = 10
mat = cbind(rbind(matrix(rnorm(nr1*nc1, mean = 1,   sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc1, mean = 0,   sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc1, mean = 0,   sd = 0.5), nr = nr3)),
    rbind(matrix(rnorm(nr1*nc2, mean = 0,   sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc2, mean = 1,   sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc2, mean = 0,   sd = 0.5), nr = nr3)),
    rbind(matrix(rnorm(nr1*nc3, mean = 0.5, sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc3, mean = 0.5, sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc3, mean = 1,   sd = 0.5), nr = nr3))
   )

rownames(mat) = paste0("row", seq_len(nrow(mat)))
colnames(mat) = paste0("column", seq_len(nrow(mat)))

ht = Heatmap(mat)
draw(ht, test = TRUE)
ht


ht = Heatmap(mat, col = colorRamp2(c(-3, 0, 3), c("green", "white", "red")))
draw(ht, test = TRUE)

ht = Heatmap(mat, name = "test")
draw(ht, test = TRUE)

ht = Heatmap(mat, rect_gp = gpar(col = "black"))
draw(ht, test = TRUE)

ht = Heatmap(mat, border = "red")
draw(ht, test = TRUE)

######## test title ##########
ht = Heatmap(mat, row_title = "blablabla")
draw(ht, test = TRUE)

ht = Heatmap(mat, row_title = "blablabla", row_title_side = "right")
draw(ht, test = TRUE)

ht = Heatmap(mat, row_title = "blablabla", row_title_gp = gpar(fontsize = 20, font = 2))
draw(ht, test = TRUE)

# ht = Heatmap(mat, row_title = "blablabla", row_title_rot = 45)
# draw(ht, test = TRUE)

ht = Heatmap(mat, row_title = "blablabla", row_title_rot = 0)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_title = "blablabla", row_title_gp = gpar(fill = "red", col = "white"))
draw(ht, test = TRUE)

ht = Heatmap(mat, column_title = "blablabla")
draw(ht, test = TRUE)

ht = Heatmap(mat, column_title = "blablabla", column_title_side = "bottom")
draw(ht, test = TRUE)

ht = Heatmap(mat, column_title = "blablabla", column_title_gp = gpar(fontsize = 20, font = 2))
draw(ht, test = TRUE)

# ht = Heatmap(mat, column_title = "blablabla", column_title_rot = 45)
# draw(ht, test = TRUE)

ht = Heatmap(mat, column_title = "blablabla", column_title_rot = 90)
draw(ht, test = TRUE)


### test clustering ####

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

ht = Heatmap(mat, row_dend_side = "right")
draw(ht, test = TRUE)

ht = Heatmap(mat, row_dend_width = unit(4, "cm"))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_dend_gp = gpar(lwd = 2, col = "red"))
draw(ht, test = TRUE)

dend = as.dendrogram(hclust(dist(mat)))
ht = Heatmap(mat, cluster_rows = dend)
draw(ht, test = TRUE)

library(dendextend)
dend = color_branches(dend, k = 3)
ht = Heatmap(mat, cluster_rows = dend)
draw(ht, test = TRUE)


ht = Heatmap(mat, cluster_columns = FALSE)
draw(ht, test = TRUE)

ht = Heatmap(mat, clustering_distance_columns = "pearson")
draw(ht, test = TRUE)

ht = Heatmap(mat, clustering_distance_columns = function(x) dist(x))
draw(ht, test = TRUE)

ht = Heatmap(mat, clustering_distance_columns = function(x, y) 1 - cor(x, y))
draw(ht, test = TRUE)

ht = Heatmap(mat, clustering_method_columns = "single")
draw(ht, test = TRUE)

ht = Heatmap(mat, column_dend_side = "bottom")
draw(ht, test = TRUE)

ht = Heatmap(mat, column_dend_height = unit(4, "cm"))
draw(ht, test = TRUE)

ht = Heatmap(mat, column_dend_gp = gpar(lwd = 2, col = "red"))
draw(ht, test = TRUE)

dend = as.dendrogram(hclust(dist(t(mat))))
ht = Heatmap(mat, cluster_columns = dend)
draw(ht, test = TRUE)

dend = color_branches(dend, k = 3)
ht = Heatmap(mat, cluster_columns = dend)
draw(ht, test = TRUE)


### test row/column order
od = c(seq(1, 24, by = 2), seq(2, 24, by = 2))
ht = Heatmap(mat, row_order = od)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_order = od, cluster_rows = TRUE)
draw(ht, test = TRUE)

ht = Heatmap(mat, column_order = od)
draw(ht, test = TRUE)

ht = Heatmap(mat, column_order = od, cluster_columns = TRUE)
draw(ht, test = TRUE)


#### test row/column names #####
ht = Heatmap(unname(mat))
draw(ht, test = TRUE)

ht = Heatmap(mat, show_row_names = FALSE)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_names_side = "left")
draw(ht, test = TRUE)

random_str2 = function(k) {
	sapply(1:k, function(i) paste(sample(letters, sample(5:10, 1)), collapse = ""))
}
ht = Heatmap(mat, row_labels = random_str2(24))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_names_gp = gpar(fontsize = 20))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_names_gp = gpar(fontsize = 1:24/2 + 5))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_names_rot = 45)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_names_rot = 45, row_names_side = "left")
draw(ht, test = TRUE)

ht = Heatmap(mat, show_column_names = FALSE)
draw(ht, test = TRUE)

ht = Heatmap(mat, column_names_side = "top")
draw(ht, test = TRUE)

ht = Heatmap(mat, column_labels = random_str2(24))
draw(ht, test = TRUE)

ht = Heatmap(mat, column_names_gp = gpar(fontsize = 20))
draw(ht, test = TRUE)

ht = Heatmap(mat, column_names_gp = gpar(fontsize = 1:24/2 + 5))
draw(ht, test = TRUE)

ht = Heatmap(mat, column_names_rot = 45)
draw(ht, test = TRUE)

### test annotations ####
anno = HeatmapAnnotation(
	foo = 1:24,
	df = data.frame(type = c(rep("A", 12), rep("B", 12))),
	bar = anno_barplot(24:1))
ht = Heatmap(mat, top_annotation = anno)
draw(ht, test = TRUE)

ht = Heatmap(mat, bottom_annotation = anno)
draw(ht, test = TRUE)

ht = Heatmap(mat, top_annotation = anno, bottom_annotation = anno)
draw(ht, test = TRUE)


### test split ####
ht = Heatmap(mat, km = 3)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_km = 3)
draw(ht, test = TRUE)

ht = Heatmap(mat, split = rep(c("A", "B"), times = c(6, 18)))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = rep(c("A", "B"), times = c(6, 18)))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = factor(rep(c("A", "B"), times = c(6, 18)), levels = c("B", "A")))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = rep(c("A", "B"), 12), row_gap = unit(5, "mm"))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = data.frame(rep(c("A", "B"), 12), rep(c("C", "D"), each = 12)))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = data.frame(rep(c("A", "B"), 12), rep(c("C", "D"), each = 12)),
	row_gap = unit(c(1, 2, 3), "mm"))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_km = 3, row_title = "foo")
draw(ht, test = TRUE)

ht = Heatmap(mat, row_km = 3, row_title = "cluster%s")
draw(ht, test = TRUE)

ht = Heatmap(mat, row_km = 3, row_title = "cluster%s", row_title_rot = 0)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_km = 3, row_title = "cluster%s", row_title_gp = gpar(fill = 2:4, col = "white"))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_km = 3, row_title = NULL)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_km = 3, row_names_gp = gpar(col = 2:4))
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = rep(c("A", "B"), times = c(6, 18)), row_km = 3)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = rep(c("A", "B"), times = c(6, 18)), row_km = 3, row_title = "cluster%s,group%s", row_title_rot = 0)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = 2)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = 2, row_title = "foo")
ht = Heatmap(mat, row_split = 2, row_title = "cluster%s")


dend = as.dendrogram(hclust(dist(mat)))
ht = Heatmap(mat, cluster_rows = dend, row_split = 2)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = 2, row_names_gp = gpar(col = 2:3))
draw(ht, test = TRUE)


### column split
ht = Heatmap(mat, column_km = 2)
draw(ht, test = TRUE)

ht = Heatmap(mat, column_km = 2, column_gap = unit(1, "cm"))
draw(ht, test = TRUE)

ht = Heatmap(mat, column_split = rep(c("A", "B"), times = c(6, 18)))
draw(ht, test = TRUE)

ht = Heatmap(mat, column_split = data.frame(rep(c("A", "B"), 12), rep(c("C", "D"), each = 12)),
	column_gap = unit(c(1, 2, 3), "mm"))
draw(ht, test = TRUE)

ht = Heatmap(mat, column_km = 2, column_title = "foo")
draw(ht, test = TRUE)

ht = Heatmap(mat, column_km = 2, column_title = "cluster%s")
draw(ht, test = TRUE)

ht = Heatmap(mat, column_km = 2, column_title = "cluster%s", column_title_rot = 90)
draw(ht, test = TRUE)

ht = Heatmap(mat, column_km = 2, column_title = "cluster%s", column_title_gp = gpar(fill = 2:3, col = "white"))
draw(ht, test = TRUE)

ht = Heatmap(mat, column_km = 2, column_title = NULL)
draw(ht, test = TRUE)

ht = Heatmap(mat, column_km = 2, column_names_gp = gpar(col = 2:3))
draw(ht, test = TRUE)

ht = Heatmap(mat, column_split = factor(rep(c("A", "B"), times = c(6, 18)), levels = c("A", "B")), column_km = 2)
draw(ht, test = TRUE)
ht = Heatmap(mat, column_split = factor(rep(c("A", "B"), times = c(6, 18)), levels = c("B", "A")), column_km = 2)


ht = Heatmap(mat, column_split = rep(c("A", "B"), times = c(6, 18)), column_km = 2, 
	column_title = "cluster%s,group%s", column_title_rot = 90)
draw(ht, test = TRUE)

ht = Heatmap(mat, column_split = 3)
draw(ht, test = TRUE)

dend = as.dendrogram(hclust(dist(t(mat))))
ht = Heatmap(mat, cluster_columns = dend, column_split = 3)
draw(ht, test = TRUE)

ht = Heatmap(mat, top_annotation = anno, bottom_annotation = anno, column_km = 2)
draw(ht, test = TRUE)

ht = Heatmap(mat, top_annotation = anno, bottom_annotation = anno, column_split = 3)
draw(ht, test = TRUE)

### combine row and column split
ht = Heatmap(mat, row_km = 3, column_km = 3)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = 3, column_split = 3)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_km = 3, column_split = 3)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_split = rep(c("A", "B"), 12), 
	column_split = rep(c("C", "D"), 12))
draw(ht, test = TRUE)

ht = Heatmap(mat, top_annotation = anno,
	row_split = rep(c("A", "B"), 12), 
	row_names_gp = gpar(col = 2:3), row_gap = unit(2, "mm"),
	column_split = 3,
	column_names_gp = gpar(col = 2:4), column_gap = unit(4, "mm")
)
draw(ht, test = TRUE)


#### character matrix
mat3 = matrix(sample(letters[1:6], 100, replace = TRUE), 10, 10)
rownames(mat3) = {x = letters[1:10]; x[1] = "aaaaaaaaaaaaaaaaaaaaaaa";x}
ht = Heatmap(mat3, rect_gp = gpar(col = "white"))
draw(ht, test = TRUE)


### cell_fun
mat = matrix(1:9, 3, 3)
rownames(mat) = letters[1:3]
colnames(mat) = letters[1:3]

ht = Heatmap(mat, rect_gp = gpar(col = "white"), cell_fun = function(j, i, x, y, width, height, fill) grid.text(mat[i, j], x = x, y = y),
	cluster_rows = FALSE, cluster_columns = FALSE, row_names_side = "left", column_names_side = "top",
	column_names_rot = 0)
draw(ht, test = TRUE)


### test the size
ht = Heatmap(mat)
ht = prepare(ht)
ht@heatmap_param[c("width", "height")]
ht@matrix_param[c("width", "height")]

ht = Heatmap(mat, width = unit(10, "cm"), height = unit(10, "cm"))
ht = prepare(ht)
ht@heatmap_param[c("width", "height")]
ht@matrix_param[c("width", "height")]
draw(ht, test = TRUE)

ht = Heatmap(mat, width = unit(10, "cm"))
ht = prepare(ht)
ht@heatmap_param[c("width", "height")]
ht@matrix_param[c("width", "height")]
draw(ht, test = TRUE)

ht = Heatmap(mat, heatmap_width = unit(10, "cm"), heatmap_height = unit(10, "cm"))
ht = prepare(ht)
ht@heatmap_param[c("width", "height")]
ht@matrix_param[c("width", "height")]
draw(ht, test = TRUE)

ht = Heatmap(mat, heatmap_width = unit(10, "cm"))
ht = prepare(ht)
ht@heatmap_param[c("width", "height")]
ht@matrix_param[c("width", "height")]
draw(ht, test = TRUE)

ht = Heatmap(mat, use_raster = TRUE)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_km = 2, use_raster = TRUE)
draw(ht, test = TRUE)

ht = Heatmap(mat, row_km = 2, column_km = 2, use_raster = TRUE)
draw(ht, test = TRUE)

#### test global padding
ra = rowAnnotation(foo = 1:3)
ht = Heatmap(mat, show_column_names = FALSE) + ra
draw(ht)

ht = Heatmap(matrix(rnorm(100), 10), row_km = 2, row_title = "")
draw(ht)

if(0) {
ht = Heatmap(matrix(rnorm(100), 10), heatmap_width = unit(5, "mm"))
draw(ht)
}
