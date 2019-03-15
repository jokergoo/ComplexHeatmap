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



# things needed to be tested
# 1. the order
# 2. if the sum of sizes are larger than xlim

make_plot = function(pos1, pos2, range) {
	oxpd = par("xpd")
	par(xpd = NA)
	plot(NULL, xlim = c(0, 4), ylim = range, ann = FALSE)
	col = rand_color(nrow(pos1), transparency = 0.5)
	rect(0.5, pos1[, 1], 1.5, pos1[, 2], col = col)
	rect(2.5, pos2[, 1], 3.5, pos2[, 2], col = col)
	segments(1.5, rowMeans(pos1), 2.5, rowMeans(pos2))
	par(xpd = oxpd)
}

range = c(0, 10)
pos1 = rbind(c(1, 2), c(5, 7))
make_plot(pos1, smartAlign2(pos1, range = range), range)

range = c(0, 10)
pos1 = rbind(c(-0.5, 2), c(5, 7))
make_plot(pos1, smartAlign2(pos1, range = range), range)

pos1 = rbind(c(-1, 2), c(3, 4), c(5, 6), c(7, 11))
pos1 = pos1 + runif(length(pos1), max = 0.3, min = -0.3)
par(mfrow = c(3, 3))
for(i in 1:9) {
	ind = sample(4, 4)
	make_plot(pos1[ind, ], smartAlign2(pos1[ind, ], range = range), range)
}
par(mfrow = c(1, 1))

pos1 = rbind(c(3, 6), c(4, 7))
make_plot(pos1, smartAlign2(pos1, range = range), range)

pos1 = rbind(c(1, 8), c(3, 10))
make_plot(pos1, smartAlign2(pos1, range = range), range)
make_plot(pos1, smartAlign2(pos1, range = range, range_fixed = FALSE), range)
