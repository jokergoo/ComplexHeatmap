

mat = matrix(rnorm(100), 10, 10)
rownames(mat) = letters[1:10]
colnames(mat) = letters[1:10]

ht = heatmap(mat, colorRamp2(c(-1, 0, 1), c("green", "white", "red")), name = "test")


ht$draw_heatmap_body()
ht$draw_hclust(which = "row", side = 2)
ht$draw_hclust(which = "row", side = 4)
ht$draw_hclust(which = "column", side = 1)
ht$draw_hclust(which = "column", side = 3)

ht$draw_dimnames()
