
context("Test annotation rasterization")

test_that("raster parameters are passed down to single annotations", {
	ha = HeatmapAnnotation(foo = 1:10, use_raster = TRUE, raster_quality = 3)
	rp = ha@anno_list[["foo"]]@raster_param
	expect_true(rp$use_raster)
	expect_equal(rp$raster_quality, 3)

	ha = HeatmapAnnotation(foo = 1:10)
	expect_false(ha@anno_list[["foo"]]@raster_param$use_raster)
})

# annotations built inside UpSet(), oncoPrint() and pheatmap() are never given a
# `use_raster` argument, so `ht_opt$annotation_use_raster` is the only way to
# rasterize them
test_that("ht_opt$annotation_use_raster reaches internally built annotations", {
	on.exit(ht_opt$annotation_use_raster <- NULL)

	m = make_comb_mat(list(a = 1:5, b = 3:8, c = 4:10))
	expect_false(UpSet(m)@top_annotation@anno_list[[1]]@raster_param$use_raster)

	ht_opt$annotation_use_raster = TRUE
	expect_true(UpSet(m)@top_annotation@anno_list[[1]]@raster_param$use_raster)

	# an explicit argument still wins over the global
	expect_false(HeatmapAnnotation(foo = 1:10, use_raster = FALSE)@anno_list[["foo"]]@raster_param$use_raster)
})

# the graphics drawn by `draw_fun` are in absolute units, so the rasterized
# result must occupy the same area no matter how large the temporary image is
test_that("rasterized annotation does not shrink when raster_quality increases", {
	skip_if_not_installed("png")

	filled_fraction = function(quality) {
		f = tempfile(fileext = ".png")
		png(f, width = 200, height = 200)
		pushViewport(viewport(width = unit(1, "npc"), height = unit(1, "npc")))
		rasterize_in_viewport(
			function() grid.rect(width = unit(10, "mm"), height = unit(10, "mm"),
				gp = gpar(fill = "red", col = NA)),
			raster_device = "png", raster_quality = quality)
		popViewport()
		dev.off()

		img = png::readPNG(f)
		file.remove(f)
		mean(img[, , 1] > 0.5 & img[, , 2] < 0.5)
	}

	q1 = filled_fraction(1)
	q3 = filled_fraction(3)

	expect_gt(q1, 0.01)
	expect_lt(abs(q1 - q3)/q1, 0.1)
})
