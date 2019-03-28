lt = list(
	a = c("h", "t", "j", "u", "w"),
	b = c("b", "n", "v", "m", "k", "u", "j", "w", "x", "z"),
	c = c("x", "g", "b", "h", "u", "s", "n", "m", "r", "l", "q", "i", "o", "d", "z")
)


test_that("test default list_to_matrix", {
	m = list_to_matrix(lt)
	expect_that(rownames(m), is_identical_to(sort(unique(unlist(lt)))))
	expect_that(colnames(m), is_identical_to(names(lt)))
	expect_that(unname(m["u", ]), is_identical_to(c(1, 1, 1)))
	expect_that(unname(m["j", ]), is_identical_to(c(1, 1, 0)))
})

test_that("test list_to_matrix with universal_set", {
	m = list_to_matrix(lt, universal_set = letters)
	expect_that(rownames(m), is_identical_to(letters))
	expect_that(unname(m["y", ]), is_identical_to(c(0, 0, 0)))
})

test_that("test list_to_matrix with universal_set which is smaller than the input set", {
	m = list_to_matrix(lt, universal_set = letters[1:10])
	expect_that(rownames(m), is_identical_to(letters[1:10]))
	expect_that(unname(m["a", ]), is_identical_to(c(0, 0, 0)))
})

test_that("test default make_comb_mat", {
	m = make_comb_mat(lt)

	tb = table(table(unlist(lt)))
	expect_that(tb[names(tb) == 1][[1]], equals(sum(comb_size(m)[comb_degree(m) == 1])))
	expect_that(tb[names(tb) == 2][[1]], equals(sum(comb_size(m)[comb_degree(m) == 2])))
	expect_that(tb[names(tb) == 3][[1]], equals(sum(comb_size(m)[comb_degree(m) == 3])))

	tb = table(unlist(lt))
	expect_that(extract_comb(m, "111"), is_identical_to(names(tb[tb == 3])))

	m1 = make_comb_mat(lt)
	m2 = make_comb_mat(list_to_matrix(lt))
	attr(m1, "x") = NULL
	attr(m2, "x") = NULL
	attr(m1, "lt") = NULL
	attr(m2, "lt") = NULL
	expect_that(m1, equals(m2))
	
	m1 = make_comb_mat(lt)
	m2 = make_comb_mat(list_to_matrix(lt))
	expect_that(sort(extract_comb(m1, "111")), is_identical_to(sort(extract_comb(m2, "111"))))
	expect_that(sort(extract_comb(m1, "011")), is_identical_to(sort(extract_comb(m2, "011"))))

	m1 = make_comb_mat(lt, universal_set = letters)
	m2 = make_comb_mat(list_to_matrix(lt, universal_set = letters))
	attr(m1, "x") = NULL
	attr(m2, "x") = NULL
	attr(m1, "lt") = NULL
	attr(m2, "lt") = NULL
	expect_that(m1, equals(m2))
})


test_that("test default make_comb_mat with universal_set", {
	m = make_comb_mat(lt, universal_set = letters)

	expect_that(length(comb_size(m)), equals(8))
	expect_that("000" %in% comb_name(m), is_identical_to(TRUE))
	expect_that(0 %in% comb_degree(m), is_identical_to(TRUE))
	expect_that(sort(extract_comb(m, "000")), is_identical_to(sort(setdiff(letters, unlist(lt)))))
})


test_that("test default make_comb_mat with universal_set which is smaller than the input set", {
	m = make_comb_mat(lt, universal_set = letters[1:10])

	expect_that("000" %in% comb_name(m), is_identical_to(TRUE))
	expect_that(length(comb_size(m)) < 8, is_identical_to(TRUE))
	expect_that(sort(extract_comb(m, "000")), is_identical_to(sort(setdiff(letters[1:10], unlist(lt)))))

	m = make_comb_mat(lt, universal_set = letters[1:10], remove_empty_comb_set = FALSE)
	expect_that(length(comb_size(m)), equals(8))
})

test_that("test matrix with no names", {
	m = list_to_matrix(lt, universal_set = letters)
	rownames(m) = NULL

	m2 = make_comb_mat(m)
	expect_that(extract_comb(m2, "000"), equals(which(rowSums(m) == 0)))
})

# test GRanges
library(GenomicRanges)

#  1        0         0         0         0
#  ++++++++++++++++++++++++++++++++++++++++
#     -------      ---------     ---------
#   ------     ------    ------      ----
#      -----     -----       ----------
#       ++++++    +++++++     +++++++

gr1 = GRanges(seqnames = "chr1",
	ranges = IRanges(start = c(4, 17, 31), end = c(10, 25, 39)))
gr2 = GRanges(seqnames = "chr1",
	ranges = IRanges(start = c(2, 13, 23, 35), end = c(7, 18, 28, 38)))
gr3 = GRanges(seqnames = "chr1",
	ranges = IRanges(start = c(5, 15, 27), end = c(9, 19, 36)))
universal = GRanges(seqnames = "chr1",
	ranges = IRanges(start = 1, end = 40))
bg = GRanges(seqnames = "chr1",
	ranges = IRanges(start = c(6, 16, 28), end = c(11, 22, 34)))

lt = list(gr1 = gr1, gr2 = gr2, gr3 = gr3)

test_that("test default make_comb_mat with GRanges", {
	m = make_comb_mat(lt)

	expect_that(unname(set_size(m)), equals(c(sum(GenomicRanges::width(gr1)), sum(GenomicRanges::width(gr2)), sum(GenomicRanges::width(gr3)))))
	expect_that(comb_size(m)[["111"]], equals(sum(GenomicRanges::width( GenomicRanges::intersect(GenomicRanges::intersect(gr1, gr2), gr3) ))))

	m = make_comb_mat(lt, universal_set = universal)
	expect_that(length(comb_size(m)), equals(8))
	expect_that(comb_size(m)[["000"]], equals(sum(GenomicRanges::width( GenomicRanges::setdiff(universal, GenomicRanges::union(GenomicRanges::union(gr1, gr2), gr3))))))

	m = make_comb_mat(lt, universal_set = bg, remove_empty_comb_set = FALSE)
	expect_that(length(comb_size(m)), equals(8))
	expect_that(comb_size(m)[["000"]], equals(sum(GenomicRanges::width( GenomicRanges::setdiff(bg, GenomicRanges::union(GenomicRanges::union(gr1, gr2), gr3))))))
})
