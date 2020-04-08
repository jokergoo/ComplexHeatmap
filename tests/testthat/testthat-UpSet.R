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
	attr(m1, "env") = NULL
	attr(m2, "env") = NULL
	expect_that(m1, equals(m2))
	
	m1 = make_comb_mat(lt)
	m2 = make_comb_mat(list_to_matrix(lt))
	expect_that(sort(extract_comb(m1, "111")), is_identical_to(sort(extract_comb(m2, "111"))))
	expect_that(sort(extract_comb(m1, "011")), is_identical_to(sort(extract_comb(m2, "011"))))

	m1 = make_comb_mat(lt, universal_set = letters)
	m2 = make_comb_mat(list_to_matrix(lt, universal_set = letters))
	attr(m1, "data") = NULL
	attr(m2, "data") = NULL
	attr(m1, "param") = NULL
	attr(m2, "param") = NULL
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

	m = make_comb_mat(lt, universal_set = letters[1:10])
	expect_that(length(comb_size(m)), equals(5))
})

test_that("test matrix with no names", {
	m = list_to_matrix(lt, universal_set = letters)
	rownames(m) = NULL

	m2 = make_comb_mat(m)
	expect_that(extract_comb(m2, "000"), equals(which(rowSums(m) == 0)))
})

### test with intersect and union mode
lt = list(
	a = c("h", "t", "j", "u", "w"),
	b = c("b", "n", "v", "m", "k", "u", "j", "w", "x", "z"),
	c = c("x", "g", "b", "h", "u", "s", "n", "m", "r", "l", "q", "i", "o", "d", "z")
)

if("package:ComplexHeatmap" %in% search()) {
	expand_mode = ComplexHeatmap:::expand_mode
}

test_that("test expand_mode()", {
	expect_that(expand_mode(c(1, 1, 0), "intersect"), 
		equals(rbind(c(1, 0, 0), c(0, 1, 0), c(1, 1, 0))))
	expect_that(expand_mode(c(1, 1, 0), "union"), 
		equals(rbind(c(1, 0, 0), c(1, 1, 0), c(1, 0, 1), c(1, 1, 1), c(0, 1, 0), c(0, 1, 1))))
	
})

test_that("test 'intersect' mode", {
	m1 = make_comb_mat(lt)
	m2 = make_comb_mat(lt, mode = "intersect")
	expect_that(sort(extract_comb(m2, "110")), 
		equals(sort(c(extract_comb(m1, "110"), extract_comb(m1, "111")))))

	expect_that(sort(extract_comb(m2, "011")), 
		equals(sort(c(extract_comb(m1, "011"), extract_comb(m1, "111")))))

	expect_that(sort(extract_comb(m2, "001")), 
		equals(sort(lt$c)))
})

test_that("test 'union' mode", {
	m2 = make_comb_mat(lt, mode = "union")

	expect_that(sort(extract_comb(m2, "011")), 
		equals(sort(unique(c(lt$b, lt$c)))))

	expect_that(sort(extract_comb(m2, "111")), 
		equals(sort(unique(c(lt$a, lt$b, lt$c)))))

	expect_that(sort(extract_comb(m2, "001")), 
		equals(sort(unique(c(lt$c)))))

})

lt = list(
	a = c("h", "t", "j", "u", "w"),
	b = c("b", "n", "v", "m", "k", "u", "j", "w", "x", "z"),
	c = c("x", "g", "b", "h", "u", "s", "n", "m", "r", "l", "q", "i", "o", "d", "z")
)
if("package:ComplexHeatmap" %in% search()) {
	make_comb_mat_from_list = ComplexHeatmap:::make_comb_mat_from_list
	make_comb_mat_from_matrix = ComplexHeatmap:::make_comb_mat_from_matrix
}
test_that("test make_comb_mat_from_matrix() and make_comb_mat_from_list()", {
	lt = list(
		a = c("h", "t", "j", "u", "w"),
		b = c("b", "n", "v", "m", "k", "u", "j", "w", "x", "z"),
		c = c("x", "g", "b", "h", "u", "s", "n", "m", "r", "l", "q", "i", "o", "d", "z")
	)
	m = list_to_matrix(lt)

	cm1 = make_comb_mat_from_list(lt, mode = "distinct")
	cm2 = make_comb_mat_from_matrix(m, mode = "distinct")
	expect_that(comb_size(cm1), equals(comb_size(cm2)))

	cm1 = make_comb_mat_from_list(lt, mode = "intersect")
	cm2 = make_comb_mat_from_matrix(m, mode = "intersect")
	expect_that(comb_size(cm1), equals(comb_size(cm2)))

	cm1 = make_comb_mat_from_list(lt, mode = "union")
	cm2 = make_comb_mat_from_matrix(m, mode = "union")
	expect_that(comb_size(cm1), equals(comb_size(cm2)))

	## four sets
	lt = list(
		a = c("h", "t", "j", "u", "w"),
		b = c("b", "n", "v", "m", "k", "u", "j", "w", "x", "z"),
		c = c("x", "g", "b", "h", "u", "s", "n", "m", "r", "l", "q", "i", "o", "d", "z"),
		d = c("a", "g", "s", "d", "r", "t", "q")
	)
	m = list_to_matrix(lt)

	cm1 = make_comb_mat_from_list(lt, mode = "distinct")
	cm2 = make_comb_mat_from_matrix(m, mode = "distinct")
	expect_that(comb_size(cm1), equals(comb_size(cm2)))

	cm1 = make_comb_mat_from_list(lt, mode = "intersect")
	cm2 = make_comb_mat_from_matrix(m, mode = "intersect")
	expect_that(comb_size(cm1), equals(comb_size(cm2)))

	cm1 = make_comb_mat_from_list(lt, mode = "union")
	cm2 = make_comb_mat_from_matrix(m, mode = "union")
	expect_that(comb_size(cm1), equals(comb_size(cm2)))

	## five sets
	lt = list(
		a = c("h", "t", "j", "u", "w"),
		b = c("b", "n", "v", "m", "k", "u", "j", "w", "x", "z"),
		c = c("x", "g", "b", "h", "u", "s", "n", "m", "r", "l", "q", "i", "o", "d", "z"),
		d = c("a", "g", "s", "d", "r", "t", "q"),
		e = c("g", "e", "s", "q", "h", "f", "b", "d", "k", "p")
	)
	m = list_to_matrix(lt)

	cm1 = make_comb_mat_from_list(lt, mode = "distinct")
	cm2 = make_comb_mat_from_matrix(m, mode = "distinct")
	expect_that(comb_size(cm1), equals(comb_size(cm2)))

	cm1 = make_comb_mat_from_list(lt, mode = "intersect")
	cm2 = make_comb_mat_from_matrix(m, mode = "intersect")
	expect_that(comb_size(cm1), equals(comb_size(cm2)))

	cm1 = make_comb_mat_from_list(lt, mode = "union")
	cm2 = make_comb_mat_from_matrix(m, mode = "union")
	expect_that(comb_size(cm1), equals(comb_size(cm2)))
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
})


lt = list(
	a = c("h", "t", "j", "u", "w"),
	b = c("b", "n", "v", "m", "k", "u", "j", "w", "x", "z"),
	c = c("x", "g", "b", "h", "u", "s", "n", "m", "r", "l", "q", "i", "o", "d", "z")
)
test_that("test with an empty set", {
	lt$d = character(0)
	m = list_to_matrix(lt)
	expect_that(ncol(m), equals(4))

	lt$d = NULL
	lt = c(lt, list(d = NULL))
	m = list_to_matrix(lt)
	expect_that(ncol(m), equals(4))

	lt$d = character(0)
	cm = make_comb_mat(lt)
	expect_that(unname(set_size(cm)["d"]), equals(0))

	lt$d = NULL
	lt = c(lt, list(d = NULL))
	cm = make_comb_mat(lt)
	expect_that(unname(set_size(cm)["d"]), equals(0))

	lt$d = character(0)
	cm = make_comb_mat_from_list(lt, "distinct")
	expect_that(unname(set_size(cm)["d"]), equals(0))

	lt$d = NULL
	lt = c(lt, list(d = NULL))
	cm = make_comb_mat_from_list(lt, "distinct")
	expect_that(unname(set_size(cm)["d"]), equals(0))

})


### test subset methods
lt = list(
	a = c("h", "t", "j", "u", "w"),
	b = c("b", "n", "v", "m", "k", "u", "j", "w", "x", "z"),
	c = c("x", "g", "b", "h", "u", "s", "n", "m", "r", "l", "q", "i", "o", "d", "z")
)
m = list_to_matrix(lt)
cm1 = make_comb_mat_from_list(lt, "distinct")
cm2 = make_comb_mat_from_matrix(m, "distinct")
test_that("test subset method on sets and combination sets", {
	expect_that(comb_size(cm1[, 1:5]), equals(comb_size(cm1)[1:5]))
	expect_that(comb_size(cm1[1:5]), equals(comb_size(cm1)[1:5]))
	expect_that(comb_size(cm1[full_comb_code(3, TRUE)]), equals(c(comb_size(cm1), "000" = 0)))

	cm1_2sets = make_comb_mat_from_list(lt[1:2], "distinct")
	expect_that(comb_size(cm1[c("a", "b"), ]), equals(comb_size(cm1_2sets)))

	expect_that(comb_size(cm2[, 1:5]), equals(comb_size(cm2)[1:5]))
	expect_that(comb_size(cm2[1:5]), equals(comb_size(cm2)[1:5]))
	expect_that(comb_size(cm2[full_comb_code(3, TRUE)]), equals(c(comb_size(cm2), "000" = 0)))

	cm2_2sets = make_comb_mat_from_matrix(m[, 1:2], "distinct")
	expect_that(comb_size(cm2[c("a", "b"), ]), equals(comb_size(cm2_2sets)))


	expect_that(length(comb_size(cm1[, 1:5][3:1, ])), equals(5))
})


lt = list(
	a = c("a", "b", "c", "d", "e"),
	b = c("a", "c"),
	c = c("a", "b"),
	d = NULL
)
cm = make_comb_mat(lt)

test_that("test subset method by set, reorder", {
	expect_that(set_name(cm[4:1, ]), equals(c("d", "c", "b", "a")))
	expect_that(set_name(cm[c("d", "c", "b", "a"), ]), equals(c("d", "c", "b", "a")))

	expect_that(set_name(t(cm)[, 4:1]), equals(c("d", "c", "b", "a")))
	expect_that(set_name(t(cm)[, c("d", "c", "b", "a")]), equals(c("d", "c", "b", "a")))
})

test_that("test subset method by set, remove empty sets", {
	expect_that(set_name(cm[1:3, ]), equals(c("a", "b", "c")))
	expect_that(set_name(cm[3:1, ]), equals(c("c", "b", "a")))
	expect_that(set_name(cm[c("a", "b", "c"), ]), equals(c("a", "b", "c")))
	expect_that(set_name(cm[c("c", "b", "a"), ]), equals(c("c", "b", "a")))

	expect_that(set_name(t(cm)[, 1:3]), equals(c("a", "b", "c")))
	expect_that(set_name(t(cm)[, 3:1]), equals(c("c", "b", "a")))
	expect_that(set_name(t(cm)[, c("a", "b", "c")]), equals(c("a", "b", "c")))
	expect_that(set_name(t(cm)[, c("c", "b", "a")]), equals(c("c", "b", "a")))
})

test_that("test subset method by set, add new empty sets", {
	expect_that(set_name(cm[c("a", "b", "c", "d", "e"), ]), equals(c("a", "b", "c", "d", "e")))
	expect_that(set_name(cm[c("e", "d", "c", "b", "a"), ]), equals(c("e", "d", "c", "b", "a")))
	expect_that(set_name(cm[c("a", "b", "c", "e"), ]), equals(c("a", "b", "c", "e")))
	expect_that(set_name(cm[c("e", "c", "b", "a"), ]), equals(c("e", "c", "b", "a")))

	expect_that(set_name(t(cm)[, c("a", "b", "c", "d", "e")]), equals(c("a", "b", "c", "d", "e")))
	expect_that(set_name(t(cm)[, c("e", "d", "c", "b", "a")]), equals(c("e", "d", "c", "b", "a")))
	expect_that(set_name(t(cm)[, c("a", "b", "c", "e")]), equals(c("a", "b", "c", "e")))
	expect_that(set_name(t(cm)[, c("e", "c", "b", "a")]), equals(c("e", "c", "b", "a")))
})

test_that("test subset method by set, only a part of non-empty sets", {
	cm2 = make_comb_mat(lt[c("a", "b")])
	expect_that(comb_size(cm[c("a", "b"), ]), equals(comb_size(cm2)))

	lt2 = lt
	lt2$e = character(0)
	cm2 = make_comb_mat(lt2[c("a", "b", "e")])
	expect_that(comb_size(cm[c("a", "b", "e"), ]), equals(comb_size(cm2)))

})

test_that("test subset method both by combination sets and sets", {
	cm2 = cm[c("c", "b", "a"), 3:1]
	expect_that(set_name(cm2), equals(c("c", "b", "a")))
	expect_that(unname(comb_size(cm2)), equals(c(1, 1, 1)))

	cm2 = cm[c("c", "b", "a", "e"), 3:1]
	expect_that(set_name(cm2), equals(c("c", "b", "a", "e")))
	expect_that(unname(comb_size(cm2)), equals(c(1, 1, 1)))

	expect_error(cm[c("a", "b", "e"), 1:3])

	cm2 = t(cm)[3:1, c("c", "b", "a")]
	expect_that(set_name(cm2), equals(c("c", "b", "a")))
	expect_that(unname(comb_size(cm2)), equals(c(1, 1, 1)))

	cm2 = cm[c("c", "b", "a", "e"), 3:1]
	expect_that(set_name(cm2), equals(c("c", "b", "a", "e")))
	expect_that(unname(comb_size(cm2)), equals(c(1, 1, 1)))

	expect_error(t(cm)[1:3, c("a", "b", "e")])

})

lt1 = list(
	a = c("a", "b", "c"),
	b = c("a", "c")
)

lt2 = list(
	b = c("a", "c"),
	c = c("a", "d")
)

cm1 = make_comb_mat(lt1)
cm2 = make_comb_mat(lt2)

cm_lt = normalize_comb_mat(cm1, cm2)
cm1_new = cm_lt[[1]]
cm2_new = cm_lt[[2]]

test_that("test normalize_comb_mat()", {
	expect_that(set_name(cm1_new), equals(set_name(cm2_new)))
	expect_that(set_name(cm1_new), equals(c("a", "b", "c")))
	expect_that(comb_name(cm1_new), equals(comb_name(cm2_new)))

	expect_error(normalize_comb_mat(cm1, t(cm2)))
})

lt = list(
	a = c("a", "b", "c", "d", "e"),
	b = c("a", "c"),
	c = c("a", "b")
)
cm = make_comb_mat(lt)
test_that("test set_name<-", {
	set_name(cm) = c("A", "B", "C")
	expect_that(set_name(cm), equals(c("A", "B", "C")))
	expect_error(set_name(cm) <- c("A", "B"))
})
