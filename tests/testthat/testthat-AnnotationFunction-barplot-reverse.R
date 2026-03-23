test_that("anno_barplot reverse numbers do not expand scale", {
	anno_reverse_without_numbers = anno_barplot(1:10,
		add_numbers = FALSE,
		height = unit(2, "cm"),
		axis_param = list(direction = "reverse"))
	anno_reverse_with_numbers = anno_barplot(1:10,
		add_numbers = TRUE,
		height = unit(2, "cm"),
		axis_param = list(direction = "reverse"))

	expect_equal(anno_reverse_with_numbers@data_scale, anno_reverse_without_numbers@data_scale)
	expect_gt(
		convertHeight(anno_reverse_with_numbers@extended[1], "mm", valueOnly = TRUE),
		convertHeight(anno_reverse_without_numbers@extended[1], "mm", valueOnly = TRUE)
	)
})
