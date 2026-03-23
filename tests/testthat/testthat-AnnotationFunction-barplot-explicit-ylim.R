test_that("anno_barplot keeps explicit ylim when numbers are added", {
	anno_normal_without_numbers = anno_barplot(1:10,
		add_numbers = FALSE,
		ylim = c(0, 8),
		height = unit(2, "cm"))
	anno_normal_with_numbers = anno_barplot(1:10,
		add_numbers = TRUE,
		ylim = c(0, 8),
		height = unit(2, "cm"))

	expect_equal(anno_normal_with_numbers@data_scale, anno_normal_without_numbers@data_scale)
	expect_gt(
		convertHeight(anno_normal_with_numbers@extended[3], "mm", valueOnly = TRUE),
		convertHeight(anno_normal_without_numbers@extended[3], "mm", valueOnly = TRUE)
	)

	anno_reverse_without_numbers = anno_barplot(1:10,
		add_numbers = FALSE,
		ylim = c(0, 8),
		height = unit(2, "cm"),
		axis_param = list(direction = "reverse"))
	anno_reverse_with_numbers = anno_barplot(1:10,
		add_numbers = TRUE,
		ylim = c(0, 8),
		height = unit(2, "cm"),
		axis_param = list(direction = "reverse"))

	expect_equal(anno_reverse_with_numbers@data_scale, anno_reverse_without_numbers@data_scale)
	expect_gt(
		convertHeight(anno_reverse_with_numbers@extended[1], "mm", valueOnly = TRUE),
		convertHeight(anno_reverse_without_numbers@extended[1], "mm", valueOnly = TRUE)
	)
})
