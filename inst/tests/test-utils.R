mat = matrix(rnorm(40), nr = 4, ncol = 10)
rownames(mat) = letters[1:4]
colnames(mat) = letters[1:10]

d1 = dist(mat)
d2 = dist2(mat)

test_that("test dist and dist2", {
	expect_that(all(abs(d1 - d2) < 1e-10), is_identical_to(TRUE))
})
