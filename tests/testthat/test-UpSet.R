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
})


test_that("test default make_comb_mat with universal_set", {
	m = make_comb_mat(lt, universal_set = letters)

	expect_that(length(comb_size(m)), is_identical_to(8))
	expect_that("000" %in% comb_name(m), is_identical_to(TRUE))
	expect_that(0 %in% comb_degree(m), is_identical_to(TRUE))

})


test_that("test default make_comb_mat with universal_set which is smaller than the input set", {
	m = make_comb_mat(lt, universal_set = letters[1:10])

	expect_that("000" %in% comb_name(m), is_identical_to(TRUE))
	expect_that(0 %in% comb_degree(m), is_identical_to(TRUE))

})

# test GRanges