

cm = ColorMapping(name = "test",
	colors = c("blue", "white", "red"),
	levels = c("a", "b", "c"))

test_that("color mapping is discrete", {
	expect_that(show(cm), prints_text("Discrete color mapping"))
	expect_that(map_to_colors(cm, "a"), is_identical_to("#0000FFFF"))
	expect_that(map_to_colors(cm, "d"), throws_error("cannot map some of the levels"))
	expect_that(map_to_colors(cm, c("a", "a", "b", "c")), is_identical_to(c("#0000FFFF", "#0000FFFF", "#FFFFFFFF", "#FF0000FF")))
})

cm = ColorMapping(name = "test",
	col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red")))

test_that("color mapping is continuous", {
	expect_that(show(cm), prints_text("Continuous color mapping"))
	expect_that(map_to_colors(cm, 0), is_identical_to("#0000FFFF"))
	expect_that(map_to_colors(cm, 2), is_identical_to("#FF0000FF"))
	expect_that(map_to_colors(cm, seq(-1, 2, length = 4)), is_identical_to(c("#0000FFFF", "#0000FFFF", "#FF0000FF", "#FF0000FF")))
})

cm = ColorMapping(name = "test",
	colors = c("blue", "white", "red"),
	levels = c(1, 2, 3))

test_that("color mapping is discrete but with numeric levels", {
	expect_that(show(cm), prints_text("Discrete color mapping"))
	expect_that(map_to_colors(cm, 1), is_identical_to("#0000FFFF"))
	expect_that(map_to_colors(cm, "1"), is_identical_to("#0000FFFF"))
	expect_that(map_to_colors(cm, 5), throws_error("cannot map some of the levels"))
	expect_that(map_to_colors(cm, c(1, 1, 2, 2)), is_identical_to(c("#0000FFFF", "#0000FFFF", "#FFFFFFFF", "#FFFFFFFF")))
})


pdf(NULL)
cm = ColorMapping(name = "test",
	colors = c("blue", "white", "red"),
	levels = c("a", "b", "c"))
color_mapping_legend(cm)

cm = ColorMapping(name = "test",
	col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red")))
color_mapping_legend(cm)

cm = ColorMapping(name = "test",
	colors = c("blue", "white", "red"),
	levels = c(1, 2, 3))
color_mapping_legend(cm)

dev.off()
