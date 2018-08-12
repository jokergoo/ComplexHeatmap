m = matrix(rnorm(100), 10)
dend1 = as.dendrogram(hclust(dist(m)))
dend1 = adjust_dend_by_x(dend1, x = sort(runif(10)))

m = matrix(rnorm(50), nr = 5)
dend2 = as.dendrogram(hclust(dist(m)))

dend3 = as.dendrogram(hclust(dist(m[1:2, ])))


dend_merge = merge(dend3, 
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


dend1 = as.dendrogram(hclust(dist(m)))
dend1 = adjust_dend_by_x(dend1, x = unit(1:10, "cm"))
grid.dendrogram(dend1, test = TRUE)

dl = cut_dendrogram(dend1, k = 3)
grid.dendrogram(dl$upper, test = TRUE)
