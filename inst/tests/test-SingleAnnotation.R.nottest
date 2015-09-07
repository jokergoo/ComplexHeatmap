
pdf(NULL)
anno = SingleAnnotation(name = "test", value = c("a", "a", "a", "b", "b", "b"))
draw(anno, 1:5)
draw(anno, c(1, 4, 3, 5, 2))
dev.off()

test_that("discrete color mapping", {
	expect_that(show(anno), prints_text("An annotaiton with discrete color mapping"))
})

pdf(NULL)
anno = SingleAnnotation(value = c("a", "a", "a", "b", "b", "b"), col = c("a" = "red", "b" = "blue"))
draw(anno, 1:5)
draw(anno, c(1, 4, 3, 5, 2))

anno = SingleAnnotation(value = 1:10)
draw(anno, 1:10)
dev.off()

test_that("continuous color mapping", {
	expect_that(show(anno), prints_text("An annotaiton with continuous color mapping"))
})

pdf(NULL)
anno = SingleAnnotation(value = 1:10, col = colorRamp2(c(1, 10), c("blue", "red")))
draw(anno, 1:10)

value = 1:10
anno = SingleAnnotation(fun = function(index) {
	n = length(index)
	x = (seq_len(n) - 0.5) / n
	y  = (value - min(value))/(max(value) - min(value))
	grid.points(x, y, default.units = "npc")
})
draw(anno, 1:10)
dev.off()

test_that("self defined function", {
	expect_that(show(anno), prints_text("An annotation with self-defined function"))
})

pdf(NULL)
anno = SingleAnnotation(value = c("a", "a", "a", "b", "b", "b"), col = c("a" = "red", "b" = "blue"), which = "column")
draw(anno, 1:5)
dev.off()
