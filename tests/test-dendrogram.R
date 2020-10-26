library(circlize)
library(ComplexHeatmap)
library(GetoptLong)

if(!exists("cut_dendrogram")) {
	cut_dendrogram = ComplexHeatmap:::cut_dendrogram
}

library(dendextend)

m = matrix(rnorm(100), 10)
dend1 = as.dendrogram(hclust(dist(m)))
dend1 = adjust_dend_by_x(dend1, sort(runif(10)))

m = matrix(rnorm(50), nr = 5)
dend2 = as.dendrogram(hclust(dist(m)))

dend3 = as.dendrogram(hclust(dist(m[1:2, ])))


dend_merge = merge_dendrogram(dend3, 
	list(set(dend1, "branches_col", "red"), 
		 set(dend2, "branches_col", "blue"))
)

grid.dendrogram(dend_merge, test = TRUE, facing = "bottom")
grid.dendrogram(dend_merge, test = TRUE, facing = "top")
grid.dendrogram(dend_merge, test = TRUE, facing = "left")
grid.dendrogram(dend_merge, test = TRUE, facing = "right")

grid.dendrogram(dend_merge, test = TRUE, facing = "bottom", order = "reverse")
grid.dendrogram(dend_merge, test = TRUE, facing = "top", order = "reverse")
grid.dendrogram(dend_merge, test = TRUE, facing = "left", order = "reverse")
grid.dendrogram(dend_merge, test = TRUE, facing = "right", order = "reverse")


m = matrix(rnorm(100), 10)
dend1 = as.dendrogram(hclust(dist(m)))
dend1 = adjust_dend_by_x(dend1, unit(1:10, "cm"))
grid.dendrogram(dend1, test = TRUE)

dl = cut_dendrogram(dend1, k = 3)
grid.dendrogram(dl$upper, test = TRUE)


m1 = matrix(rnorm(100), nr = 10)
m2 = matrix(rnorm(80), nr = 8)
m3 = matrix(rnorm(50), nr = 5)
dend1 = as.dendrogram(hclust(dist(m1)))
dend2 = as.dendrogram(hclust(dist(m2)))
dend3 = as.dendrogram(hclust(dist(m3)))
dend_p = as.dendrogram(hclust(dist(rbind(colMeans(m1), colMeans(m2), colMeans(m3)))))
dend_m = merge_dendrogram(dend_p, list(dend1, dend2, dend3))
grid.dendrogram(dend_m, test = T)

dend_m = merge_dendrogram(dend_p, list(dend1, dend2, dend3), only_parent = TRUE)
grid.dendrogram(dend_m, test = T)

require(dendextend)
dend1 = color_branches(dend1, k = 1, col = "red")
dend2 = color_branches(dend2, k = 1, col = "blue")
dend3 = color_branches(dend3, k = 1, col = "green")
dend_p = color_branches(dend_p, k = 1, col = "orange")
dend_m = merge_dendrogram(dend_p, list(dend1, dend2, dend3))
grid.dendrogram(dend_m, test = T)


m = matrix(rnorm(120), nc = 12)
colnames(m) = letters[1:12]
fa = rep(c("a", "b", "c"), times = c(2, 4, 6))
dend = cluster_within_group(m, fa)
grid.dendrogram(dend, test = TRUE)


# stack overflow problem
m = matrix(1, nrow = 1000, ncol = 10)
m[1, 2] = 2
dend = as.dendrogram(hclust(dist(m)))
grid.dendrogram(dend, test = T)

# node attr
m = matrix(rnorm(100), 10)
dend = as.dendrogram(hclust(dist(m)))
require(dendextend)
dend1 = color_branches(dend, k = 2, col = 1:2)
grid.dendrogram(dend1, test = T)
dend1 = dend
dend1 = dendrapply(dend, function(d) {
	attr(d, "nodePar") = list(pch = sample(20, 1), cex = runif(1, min = 0.3, max = 1.3), col = rand_color(1))
	d
})
grid.dendrogram(dend1, test = T)

Heatmap(m, cluster_rows = dend1, cluster_columns = dend1)

d1 = ComplexHeatmap:::dend_edit_node(dend, method = "top-bottom", function(d, index) {
	attr(d, "depth") = length(index)
	d
})

d2 = ComplexHeatmap:::dend_edit_node(dend, method = "bottom-top", function(d, index) {
	attr(d, "depth") = length(index)
	d
})

identical(d1, d2)
