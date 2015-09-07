mat = matrix(rnorm(40), nr = 4, ncol = 10)
rownames(mat) = letters[1:4]
colnames(mat) = letters[1:10]

d1 = dist(mat)
d2 = dist2(mat)

test_that("test dist and dist2", {
	expect_that(all(abs(d1 - d2) < 1e-10), is_identical_to(TRUE))
})

test_that("test default colors", {
	col = default_col(c("a", "b", "c"))
	expect_that(is.atomic(col), is_identical_to(TRUE))
	col = default_col(factor(c("a", "b", "c")))
	expect_that(is.atomic(col), is_identical_to(TRUE))
	col = default_col(1:10)
	expect_that(is.function(col), is_identical_to(TRUE))
})
