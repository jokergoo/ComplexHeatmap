

mat = matrix(rnorm(100), 10, 10)
rownames(mat) = letters[1:10]
colnames(mat) = letters[1:10]

annotation = data.frame(type = sample(c("A", "B"), 10, replace = TRUE), stringsAsFactors = FALSE)
rownames(annotation) = colnames(mat)
annotation_color = list(type = c(A = "red", B = "blue"))

ht = Heatmap(mat, colorRamp2(c(-1, 0, 1), c("green", "white", "red")), name = "test",
	annotation = annotation, annotation_color = annotation_color)


ht$draw_heatmap_body()
ht$draw_hclust(which = "row", side = 2)
ht$draw_hclust(which = "row", side = 4)
ht$draw_hclust(which = "column", side = 1)
ht$draw_hclust(which = "column", side = 3)

ht$draw_dimnames()
