
library(circlize)
library(grid)
library(RColorBrewer)


mat = matrix(rnorm(40, 2), 4, 10)
mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
rownames(mat) = letters[1:8]
colnames(mat) = letters[1:10]

ht1 = Heatmap(mat)
ht2 = Heatmap(mat)

ht_list = ht1 + ht2

ht_list
draw(ht_list, heatmap_legend_side = "bottom")

ht1 = Heatmap(mat, km = 2)
ht2 = Heatmap(mat)

ht_list = ht1 + ht2

draw(ht_list)


df = data.frame(type = c("a", "a", "a", "a", "b", "b", "b", "b"))
ha = HeatmapAnnotation(df = df, which = "row", width = unit(1, "cm"))
df2 = data.frame(type = c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"))
ha2 = HeatmapAnnotation(df = df2)
ht1 = Heatmap(mat, top_annotation = ha2)
ht1 + ha

value = 1:8
anno_points = function(index) {
	n = length(index)
	pushViewport(viewport(yscale = c(0.5, n+0.5), xscale = range(value)))
	grid.rect()
	grid.points(seq_along(index), value[index])
	upViewport()
}
ha = HeatmapAnnotation(points = anno_points, which = "row", width = unit(1, "cm"))
ht1 + ha
