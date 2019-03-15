
context("test annotation size for a single HeatmapAnntoation object with only one annotation.")
ComplexHeatmap:::dev.null()

size_num = function(ha) {
	s = sapply(ha@anno_list, function(anno) {
		convertHeight(size(anno), "mm", valueOnly = TRUE)
	})
	unname(s)
}

test_that("a single simple annotation", {
	ha = HeatmapAnnotation(foo = 1:10)
	expect_that(size_num(ha), equals(5))

	ha = HeatmapAnnotation(foo = cbind(1:10, 10:1))
	expect_that(size_num(ha), equals(10))

	ha = HeatmapAnnotation(foo = 1:10)
	expect_that(size_num(ha), equals(5))

	ha = HeatmapAnnotation(foo1 = 1:10, 
		simple_anno_size = unit(1, "cm")
	)
	expect_that(size_num(ha), equals(10))

	ha = HeatmapAnnotation(foo1 = 1:10, 
		annotation_height = unit(1, "cm"),
		simple_anno_size_adjust = TRUE
	)
	expect_that(size_num(ha), equals(10))

	ha = HeatmapAnnotation(foo1 = 1:10, 
		height = unit(1, "cm"),
		simple_anno_size_adjust = TRUE
	)
	expect_that(size_num(ha), equals(10))

	ha = HeatmapAnnotation(foo1 = 1:10, 
		annotation_height = unit(1, "cm")
	)
	expect_that(size_num(ha), equals(5))
	
	ha = HeatmapAnnotation(foo1 = 1:10, 
		height = unit(1, "cm")
	)
	expect_that(size_num(ha), equals(5))

	ht_opt$simple_anno_size = unit(1, "cm")
	ha = HeatmapAnnotation(foo1 = 1:10)
	expect_that(size_num(ha), equals(10))
	ht_opt$simple_anno_size = unit(5, "mm")

})

test_that("a single complex annotation", {
	ha = HeatmapAnnotation(foo = anno_points(1:10))
	expect_that(size_num(ha), equals(10))

	ha = HeatmapAnnotation(foo = anno_points(1:10),
		annotation_height = unit(2, "cm"))
	expect_that(size_num(ha), equals(20))

	ha = HeatmapAnnotation(foo = anno_points(1:10),
		height = unit(2, "cm"))
	expect_that(size_num(ha), equals(20))
})

test_that("multiple simple annotations", {
	ha = HeatmapAnnotation(foo = 1:10, bar = 10:1)
	expect_that(size_num(ha), equals(c(5, 5)))

	ha = HeatmapAnnotation(foo = cbind(1:10, 10:1), bar = 10:1)
	expect_that(size_num(ha), equals(c(10, 5)))

	ha = HeatmapAnnotation(foo = cbind(1:10, 10:1), bar = 10:1, 
		simple_anno_size = unit(1, "cm")
	)
	expect_that(size_num(ha), equals(c(20, 10)))

	ha = HeatmapAnnotation(foo = cbind(1:10, 10:1), bar = 10:1, 
		annotation_height = unit(c(2, 1), "cm"),
		simple_anno_size_adjust = TRUE
	)
	expect_that(size_num(ha), equals(c(20, 10)))

	ha = HeatmapAnnotation(foo = cbind(1:10, 10:1), bar = 10:1, gap = unit(0, "mm"),
		height = unit(3, "cm"),
		simple_anno_size_adjust = TRUE
	)
	expect_that(size_num(ha), equals(c(20, 10)))

	ha = HeatmapAnnotation(foo = cbind(1:10, 10:1), bar = 10:1, 
		annotation_height = unit(c(2, 1), "cm")
	)
	expect_that(size_num(ha), equals(c(10, 5)))

	ha = HeatmapAnnotation(foo = cbind(1:10, 10:1), bar = 10:1, gap = unit(0, "mm"),
		height = unit(3, "cm")
	)
	expect_that(size_num(ha), equals(c(10, 5)))

})

test_that("multiple complex annotations", {
	ha = HeatmapAnnotation(foo = anno_points(1:10), bar = anno_barplot(10:1))
	expect_that(size_num(ha), equals(c(10, 10)))

	ha = HeatmapAnnotation(
		foo = anno_points(1:10, height = unit(2, "cm")), 
		bar = anno_barplot(10:1, height =unit(3, "cm")))
	expect_that(size_num(ha), equals(c(20, 30)))

	ha = HeatmapAnnotation(
		foo = anno_points(1:10, height = unit(2, "cm")), 
		bar = anno_barplot(10:1, height =unit(3, "cm")),
		annotation_height = unit(c(1, 2), "cm"))
	expect_that(size_num(ha), equals(c(10, 20)))

	ha = HeatmapAnnotation(
		foo = anno_points(1:10, height = unit(2, "cm")), 
		bar = anno_barplot(10:1, height =unit(3, "cm")),
		gap = unit(0, "mm"),
		height = unit(10, "cm"))
	expect_that(size_num(ha), equals(c(40, 60)))
})

test_that("mix of simple and complex annotations", {
	ha = HeatmapAnnotation(
		foo = 1:10, 
		bar = anno_barplot(10:1))
	expect_that(size_num(ha), equals(c(5, 10)))

	ha = HeatmapAnnotation(
		foo = 1:10, 
		bar = anno_barplot(10:1),
		simple_anno_size = unit(1, "cm"))
	expect_that(size_num(ha), equals(c(10, 10)))

	ha = HeatmapAnnotation(
		foo = 1:10, 
		bar = anno_barplot(10:1),
		annotation_height = unit(c(1, 2), "cm"))
	expect_that(size_num(ha), equals(c(10, 20)))

	ha = HeatmapAnnotation(
		foo = 1:10, 
		bar = anno_barplot(10:1),
		gap = unit(0, "mm"),
		height = unit(3, "cm"))
	expect_that(size_num(ha), equals(c(5, 25)))

	ha = HeatmapAnnotation(
		foo = 1:10, 
		bar = anno_barplot(10:1),
		gap = unit(0, "mm"),
		height = unit(3, "cm"),
		simple_anno_size_adjust = TRUE)
	expect_that(size_num(ha), equals(c(10, 20)))

	ha = HeatmapAnnotation(
		foo = 1:10, 
		bar = anno_barplot(10:1),
		gap = unit(0, "mm"),
		height = unit(3, "cm"),
		simple_anno_size_adjust = FALSE)
	expect_that(size_num(ha), equals(c(5, 25)))

})


test_that("annotation_height as fractions", {
	ha = HeatmapAnnotation(foo = 1:10, 
		bar = cbind(1:10, 10:1),
		pt = anno_points(1:10),
		gap = unit(0, "mm"),
		annotation_height = 1,
		height = unit(6, "cm"))
	expect_that(size_num(ha), equals(c(20, 20, 20)))

	ha = HeatmapAnnotation(foo = 1:10, 
		bar = cbind(1:10, 10:1),
		pt = anno_points(1:10),
		gap = unit(0, "mm"),
		annotation_height = 1:3,
		height = unit(6, "cm"))
	expect_that(size_num(ha), equals(c(10, 20, 30)))

	ha = HeatmapAnnotation(foo = 1:10, 
		bar = cbind(1:10, 10:1),
		pt = anno_points(1:10),
		gap = unit(0, "mm"),
		annotation_height = unit(c(1, 2, 3), c("null", "null", "cm")),
		height = unit(6, "cm"))
	expect_that(size_num(ha), equals(c(10, 20, 30)))

	ha = HeatmapAnnotation(foo = 1:10, 
		bar = cbind(1:10, 10:1),
		pt = anno_points(1:10),
		gap = unit(0, "mm"),
		annotation_height = unit(c(2, 2, 3), c("cm", "null", "cm")),
		height = unit(6, "cm"))
	expect_that(size_num(ha), equals(c(20, 10, 30)))

})

context("test heatmap annotations on multiple heatmaps")
set.seed(123)
mat1 = matrix(rnorm(80, 2), 8, 10)
mat1 = rbind(mat1, matrix(rnorm(40, -2), 4, 10))
rownames(mat1) = paste0("R", 1:12)
colnames(mat1) = paste0("C", 1:10)

mat2 = matrix(runif(60, max = 3, min = 1), 6, 10)
mat2 = rbind(mat2, matrix(runif(60, max = 2, min = 0), 6, 10))
rownames(mat2) = paste0("R", 1:12)
colnames(mat2) = paste0("C", 1:10)

ind = sample(12, 12)
mat1 = mat1[ind, ]
mat2 = mat2[ind, ]

test_that("only one heatmap has annotations", {
	ha = HeatmapAnnotation(foo = 1:10)
	ht = Heatmap(mat1, top_annotation = ha) + Heatmap(mat2)
	ht = draw(ht)
	expect_that(size_num(ht@ht_list[[1]]@top_annotation), equals(5))

	ha = HeatmapAnnotation(foo = 1:10, bar = anno_points(1:10))
	ht = Heatmap(mat1, top_annotation = ha) + Heatmap(mat2)
	ht = draw(ht)
	expect_that(size_num(ht@ht_list[[1]]@top_annotation), equals(c(5, 10)))
})

test_that("both heatmaps have annotations", {
	# both heatmaps only have simple annotations
	ha1 = HeatmapAnnotation(foo1 = 1:10)
	ha2 = HeatmapAnnotation(foo1 = 1:10,
	                        foo2 = 1:10,
	                        foo4 = 1:10,
	                        foo5 = 1:10,
	                        gap = unit(0, "mm"))
	ht1 = Heatmap(mat1, top_annotation = ha1)
	ht2 = Heatmap(mat2, top_annotation = ha2)
	ht_list = draw(ht1 + ht2)
	expect_that(size_num(ht_list@ht_list[[1]]@top_annotation), equals(5))
	expect_that(size_num(ht_list@ht_list[[2]]@top_annotation), equals(c(5, 5, 5, 5)))

	ha1 = HeatmapAnnotation(foo1 = 1:10,
		simple_anno_size_adjust = TRUE)
	ha2 = HeatmapAnnotation(foo1 = 1:10,
	                        foo2 = 1:10,
	                        foo4 = 1:10,
	                        foo5 = 1:10,
	                        gap = unit(0, "mm"))
	ht1 = Heatmap(mat1, top_annotation = ha1)
	ht2 = Heatmap(mat2, top_annotation = ha2)
	ht_list = draw(ht1 + ht2)
	expect_that(size_num(ht_list@ht_list[[1]]@top_annotation), equals(20))
	expect_that(size_num(ht_list@ht_list[[2]]@top_annotation), equals(c(5, 5, 5, 5)))

	ha1 = HeatmapAnnotation(foo1 = 1:10,
		                    simple_anno_size = unit(1, "cm"))
	ha2 = HeatmapAnnotation(foo1 = 1:10,
	                        foo2 = 1:10,
	                        foo4 = 1:10,
	                        foo5 = 1:10,
	                        gap = unit(0, "mm"),
	                        simple_anno_size = unit(6, "mm"))
	ht1 = Heatmap(mat1, top_annotation = ha1)
	ht2 = Heatmap(mat2, top_annotation = ha2)
	ht_list = draw(ht1 + ht2)
	expect_that(size_num(ht_list@ht_list[[1]]@top_annotation), equals(10))
	expect_that(size_num(ht_list@ht_list[[2]]@top_annotation), equals(c(6, 6, 6, 6)))

	ha1 = HeatmapAnnotation(foo1 = 1:10,
		                    simple_anno_size = unit(1, "cm"),
		                    simple_anno_size_adjust = TRUE)
	ha2 = HeatmapAnnotation(foo1 = 1:10,
	                        foo2 = 1:10,
	                        foo4 = 1:10,
	                        foo5 = 1:10,
	                        gap = unit(0, "mm"),
	                        simple_anno_size = unit(6, "mm"))
	ht1 = Heatmap(mat1, top_annotation = ha1)
	ht2 = Heatmap(mat2, top_annotation = ha2)
	ht_list = draw(ht1 + ht2)
	expect_that(size_num(ht_list@ht_list[[1]]@top_annotation), equals(24))
	expect_that(size_num(ht_list@ht_list[[2]]@top_annotation), equals(c(6, 6, 6, 6)))

	ha1 = HeatmapAnnotation(foo1 = 1:10,
		                    simple_anno_size = unit(4, "cm"))
	ha2 = HeatmapAnnotation(foo1 = 1:10,
	                        foo2 = 1:10,
	                        foo4 = 1:10,
	                        foo5 = 1:10,
	                        gap = unit(0, "mm"),
	                        simple_anno_size_adjust = TRUE)
	ht1 = Heatmap(mat1, top_annotation = ha1)
	ht2 = Heatmap(mat2, top_annotation = ha2)
	ht_list = draw(ht1 + ht2)
	expect_that(size_num(ht_list@ht_list[[1]]@top_annotation), equals(40))
	expect_that(size_num(ht_list@ht_list[[2]]@top_annotation), equals(c(10, 10, 10, 10)))

	# both heatmaps have complex annotations
	ha1 = HeatmapAnnotation(foo1 = anno_points(1:10),
		                    foo2 = anno_barplot(10:1, height = unit(2, "cm")),
		                    gap = unit(0, "mm"))
	ha2 = HeatmapAnnotation(foo3 = anno_points(1:10, height = unit(2, "cm")),
		                    foo4 = anno_barplot(10:1),
		                    gap = unit(0, "mm"))
	ht1 = Heatmap(mat1, top_annotation = ha1)
	ht2 = Heatmap(mat2, top_annotation = ha2)
	ht_list = draw(ht1 + ht2)
	expect_that(size_num(ht_list@ht_list[[1]]@top_annotation), equals(c(10, 20)))
	expect_that(size_num(ht_list@ht_list[[2]]@top_annotation), equals(c(20, 10)))

	ha1 = HeatmapAnnotation(foo1 = anno_points(1:10),
		                    foo2 = anno_barplot(10:1),
		                    gap = unit(0, "mm"))
	ha2 = HeatmapAnnotation(foo3 = anno_points(1:10, height = unit(2, "cm")),
		                    foo4 = anno_barplot(10:1),
		                    gap = unit(0, "mm"))
	ht1 = Heatmap(mat1, top_annotation = ha1)
	ht2 = Heatmap(mat2, top_annotation = ha2)
	ht_list = draw(ht1 + ht2)
	expect_that(size_num(ht_list@ht_list[[1]]@top_annotation), equals(c(15, 15)))
	expect_that(size_num(ht_list@ht_list[[2]]@top_annotation), equals(c(20, 10)))

	## one heatmap has simpel annotation and the other one has complex annotation
	ha1 = HeatmapAnnotation(foo1 = 1:10,
		                    gap = unit(0, "mm"))
	ha2 = HeatmapAnnotation(foo3 = anno_points(1:10),
		                    gap = unit(0, "mm"))
	ht1 = Heatmap(mat1, top_annotation = ha1)
	ht2 = Heatmap(mat2, top_annotation = ha2)
	ht_list = draw(ht1 + ht2)
	expect_that(size_num(ht_list@ht_list[[1]]@top_annotation), equals(5))
	expect_that(size_num(ht_list@ht_list[[2]]@top_annotation), equals(10))

	ha1 = HeatmapAnnotation(foo1 = 1:10,
		                    gap = unit(0, "mm"),
		                    simple_anno_size_adjust = TRUE)
	ha2 = HeatmapAnnotation(foo3 = anno_points(1:10),
		                    gap = unit(0, "mm"))
	ht1 = Heatmap(mat1, top_annotation = ha1)
	ht2 = Heatmap(mat2, top_annotation = ha2)
	ht_list = draw(ht1 + ht2)
	expect_that(size_num(ht_list@ht_list[[1]]@top_annotation), equals(10))
	expect_that(size_num(ht_list@ht_list[[2]]@top_annotation), equals(10))


	ha1 = HeatmapAnnotation(foo1 = 1:10,
							foo2 = anno_points(1:10),
		                    gap = unit(0, "mm"))
	ha2 = HeatmapAnnotation(foo3 = anno_points(1:10),
		                    gap = unit(0, "mm"))
	ht1 = Heatmap(mat1, top_annotation = ha1)
	ht2 = Heatmap(mat2, top_annotation = ha2)
	ht_list = draw(ht1 + ht2)
	expect_that(size_num(ht_list@ht_list[[1]]@top_annotation), equals(c(5, 10)))
	expect_that(size_num(ht_list@ht_list[[2]]@top_annotation), equals(15))

	ha1 = HeatmapAnnotation(foo1 = 1:10,
							foo2 = anno_points(1:10),
		                    gap = unit(0, "mm"))
	ha2 = HeatmapAnnotation(foo3 = anno_points(1:10, height = unit(2, "cm")),
		                    gap = unit(0, "mm"))
	ht1 = Heatmap(mat1, top_annotation = ha1)
	ht2 = Heatmap(mat2, top_annotation = ha2)
	ht_list = draw(ht1 + ht2)
	expect_that(size_num(ht_list@ht_list[[1]]@top_annotation), equals(c(5, 15)))
	expect_that(size_num(ht_list@ht_list[[2]]@top_annotation), equals(20))
})


ComplexHeatmap:::dev.off2()
