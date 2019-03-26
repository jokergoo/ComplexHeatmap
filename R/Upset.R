
make_comb_mat_from_matrix = function(x, mode, top_n_sets = Inf, min_set_size = -Inf, complement_size = NULL) {
	# check whether x is a binary matrix
	if(is.data.frame(x)) {
		lc = sapply(x, function(x) {
			if(is.numeric(x)) {
				all(x == 0 | x == 1)
			} else if(is.logical(x)) {
				TRUE
			} else {
				FALSE
			}
		})
	} else if(is.matrix(x)) {
		lc = apply(x, 2, function(x) {
			if(is.numeric(x)) {
				all(x == 0 | x == 1)
			} else if(is.logical(x)) {
				TRUE
			} else {
				FALSE
			}
		})
	}
	if(sum(lc) < 1) {
		stop_wrap("Can not find columns which are logical or only contain 0 or 1.")
	} else {
		x = x[, lc, drop = FALSE]
	}
	if(is.null(colnames(x))) {
		stop_wrap("The matrix or the data frame must have column names.")
	}

	x = as.matrix(x) + 0
	set_size = colSums(x)
	l = set_size >= min_set_size & rank(max(set_size) - set_size) <= top_n_sets

	set_size = set_size[l]
	x = x[, l, drop = FALSE]
	x = x[rowSums(x) > 0, , drop = FALSE]
	
	comb_mat = unique(x)
	rn = apply(comb_mat, 1, binaryToInt)
	tb = table(apply(x, 1, binaryToInt))
	comb_size = as.vector(tb[as.character(rn)])
	
	rownames(comb_mat) = NULL
	comb_mat = t(comb_mat)

	nc = ncol(comb_mat)
	comb_mat2 = matrix(nrow = nrow(comb_mat), ncol = nc*(nc-1)/2)
	rownames(comb_mat2) = rownames(comb_mat)
	if(mode == "intersect") {
		if(nc > 1) {
			ic = 0
			for(i in 1:(nc-1)) {
				for(j in (i+1):nc) {
					ic = ic + 1
					comb_mat2[, ic] = (comb_mat[, i] & comb_mat[, j]) + 0
				}
			}
		}
	} else if(mode == "union") {
		if(nc > 1) {
			ic = 0
			for(i in 1:(nc-1)) {
				for(j in (i+1):nc) {
					ic = ic + 1
					comb_mat2[, ic] = (comb_mat[, i] | comb_mat[, j]) + 0
				}
			}
		}
	}
	comb_mat2 = comb_mat2[, colSums(comb_mat2) > 0, drop = FALSE]

	if(mode %in% c("intersect", "union")) {
		comb_mat = unique(comb_mat2, MARGIN = 2)
		comb_size = apply(comb_mat, 2, function(query) {
			if(mode == "intersect") {
				l_subset = query == 1
				l = apply(x, 1, function(y) all(y[l_subset] == 1))
			} else if(mode == "union") {
				l_subset = query == 1
				l = apply(x, 1, function(y) {
					if(all(y[!l_subset] == 0)) {
						sum(y[l_subset]) > 0
					} else {
						FALSE
					}
				})
			}
			sum(l)
		})
	}

	if(!is.null(complement_size)) {
		comb_mat = cbind(rep(0, nrow(comb_mat)), comb_mat)
		comb_size = c(complement_size, comb_size)
	}

	attr(comb_mat, "set_size") = set_size
	attr(comb_mat, "comb_size") = comb_size
	attr(comb_mat, "mode") = mode
	attr(comb_mat, "set_on_rows") = TRUE
	attr(comb_mat, "x") = x
	class(comb_mat) = c("comb_mat", "matrix")
	return(comb_mat)

}

make_comb_mat_from_list = function(lt, mode, value_fun = length, top_n_sets = Inf, min_set_size = -Inf, complement_size = NULL) {
	n = length(lt)
    nm = names(lt)
    if(is.null(nm)) {
    	stop_wrap("The list must have names.")
    }

    if(inherits(lt[[1]], "GRanges")) {
    	union = getFromNamespace("union", ns = "BiocGenerics")
    	intersect = getFromNamespace("intersect", ns = "BiocGenerics")
    	setdiff = getFromNamespace("setdiff", ns = "BiocGenerics")
    } else if(inherits(lt[[1]], "IRanges")) {
    	union = getFromNamespace("union", ns = "BiocGenerics")
    	intersect = getFromNamespace("intersect", ns = "BiocGenerics")
    	setdiff = getFromNamespace("setdiff", ns = "BiocGenerics")
    }

    if(inherits(lt[[1]], "GRanges")) {
    	set_size = sapply(lt, function(x) {
	    	value_fun(union(x, x[NULL]))
	    })
    } else if(inherits(lt[[1]], "IRanges")) {
    	set_size = sapply(lt, function(x) {
	    	value_fun(union(x, x[NULL]))
	    })
    } else {
	    set_size = sapply(lt, function(x) {
	    	value_fun(union(x, NULL))
	    })
	}

	l = set_size >= min_set_size & rank(max(set_size) - set_size) <= top_n_sets

	set_size = set_size[l]
	lt = lt[l]
	n = length(lt)
    nm = names(lt)
    
    comb_mat = matrix(FALSE, nrow = n, ncol = sum(choose(n, 1:n)))
    rownames(comb_mat) = nm
    j = 1
    for(k in 1:n) {
        comb = combn(n, k)
        for(i in 1:ncol(comb)) {
            comb_mat[comb[, i], j] = TRUE
            j = j + 1
        }
    }

    get_comb_size = function(lt, mode, do = rep(TRUE, length(lt)), value_fun = length) {
        set1_index = which(do)
        set2_index = which(!do)

        s = lt[[ set1_index[1] ]]
        if(mode == "distinct") {
	        for(i in set1_index[-1]) {
	            s = intersect(s, lt[[ i ]])
	        }

	        for(i in set2_index) {
	            s = setdiff(s, lt[[ i ]])
	        }
	    } else if(mode == "intersect") {
	    	for(i in set1_index[-1]) {
	            s = intersect(s, lt[[ i ]])
	        }
	    } else if(mode == "union") {
	    	for(i in set1_index[-1]) {
	            s = union(s, lt[[ i ]])
	        }
	    }
        value_fun(s)
    }

    comb_size = numeric(ncol(comb_mat))
    for(i in seq_len(ncol(comb_mat))) {
        comb_size[i] = get_comb_size(lt, mode = mode, comb_mat[, i], value_fun = value_fun)
    }

    comb_mat = comb_mat + 0

    if(!is.null(complement_size)) {
		comb_mat = cbind(rep(0, nrow(comb_mat)), comb_mat)
		comb_size = c(complement_size, comb_size)
	}

    attr(comb_mat, "set_size") = set_size
	attr(comb_mat, "comb_size") = comb_size
	attr(comb_mat, "mode") = mode
	attr(comb_mat, "set_on_rows") = TRUE
	attr(comb_mat, "lt") = lt
	class(comb_mat) = c("comb_mat", "matrix")
	return(comb_mat)
}

# == title
# Convert a List of Sets to a Binary Matrix
#
# == param
# -lt A list of vectors.
#
# == details
# It converts the list which have m sets to a binary matrix with n rows and m columns
# where n is the number of union of all sets in the list.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# list_to_matrix(lt)
list_to_matrix = function(lt) {
	cn = unique(unlist(lt))
	mat = matrix(0, nrow = length(cn), ncol = length(lt))
	rownames(mat) = cn
	colnames(mat) = names(lt)
	for(i in seq_along(lt)) {
		mat[unique(lt[[i]]), i] = 1
	}
	return(mat)
}

# == title
# Make a Combination Matrix for UpSet Plot
#
# == param
# -... The input sets. If it is represented as a single variable, it should be a matrix/data frame
#     or a list. If it is multiple variables, it should be name-value pairs, see Input section for explanation.
# -mode The mode for forming the combination set, see Mode section.
# -top_n_sets Number of sets with largest size.
# -min_set_size Ths minimal set size that is used for generating the combination matrix.
# -complement_size The size for the complement of all sets. If it is specified, the combination
#                  set name will be like "00...".
# -value_fun For each combination set, how to calculate the size? If it is a scalar set, 
#      the length of the vector is the size of the set, while if it is a region-based set,
#      (i.e. ``GRanges`` or ``IRanges`` object), the sum of widths of regions in the set is
#      calculated as the size of the set.
#
# == Input
# To represent multiple sets, the variable can be represented as: 
#
# 1. A list of sets where each set is a vector, e.g.:
#
#     list(set1 = c("a", "b", "c"),
#          set2 = c("b", "c", "d", "e"),
#          ...)
#
# 2. A binary matrix/data frame where rows are elements and columns are sets, e.g.:
#
#       a b c
#     h 1 1 1
#     t 1 0 1
#     j 1 0 0
#     u 1 0 1
#     w 1 0 0
#     ...
#
# If the variable is a data frame, the binary columns (only contain 0 and 1) and the logical
# columns are only kept.
#
# The set can be genomic regions, then it can only be represented as a list of ``GRanges`` objects.
#
# == Mode
# E.g. for three sets (A, B, C), the UpSet approach splits the combination of selecting elements
# in the set or not in the set and calculates the sizes of the combination sets. For three sets,
# all possible combinations are:
#
#     A B C
#     1 1 1
#     1 1 0
#     1 0 1
#     0 1 1
#     1 0 0
#     0 1 0
#     0 0 1
# 
# A value of 1 means to select that set and 0 means not to select that set. E.g., "1 1 0"
# means to select set A, B while not set C. Note there is no "0 0 0", because the background 
# size is not of interest here. With the code of selecting and not selecting the sets, next
# we need to define how to calculate the size of that combination set. There are three modes:
#
# 1. ``distinct`` mode: 1 means in that set and 0 means not in that set, then "1 1 0" means a
# set of elements also in set A and B, while not in C (i.e. ``setdiff(intersect(A, B), C)``). Under
# this mode, the seven combination sets are the seven partitions in the Venn diagram and they
# are mutually exclusive.
#
# 2. ``intersect`` mode: 1 means in that set and 0 is not taken into account, then, "1 1 0" means
# a set of elements in set A and B, and they can also in C or not in C (i.e. ``intersect(A, B)``).
# Under this mode, the seven combination sets can overlap.
#
# 3. ``union`` mode: 1 means in that set and 0 is not taken into account. When there are multiple
# 1, the relationship is OR. Then, "1 1 0" means a set of elements in set A or B, and they can also in C or not in C (i.e. ``union(A, B)``).
# Under this mode, the seven combination sets can overlap.
#
# == value
# A matrix also in a class of ``comb_mat``.
#
# Following functions can be applied to it: `set_name`, `comb_name`, `set_size`, `comb_size`, `comb_degree`,
# `extract_comb` and `t.comb_mat`.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m = make_comb_mat(lt)
#
# mat = list_to_matrix(lt)
# mat
# m = make_comb_mat(mat)
#
# \dontrun{
# library(circlize)
# library(GenomicRanges)
# lt = lapply(1:4, function(i) generateRandomBed())
# lt = lapply(lt, function(df) GRanges(seqnames = df[, 1], 
#     ranges = IRanges(df[, 2], df[, 3])))
# names(lt) = letters[1:4]
# m = make_comb_mat(lt)
# }
make_comb_mat = function(..., mode = c("distinct", "intersect", "union"),
	top_n_sets = Inf, min_set_size = -Inf, complement_size = NULL, value_fun) {

	lt = list(...)

	mode = match.arg(mode)[1]
	if(length(lt) == 1) {
		lt = lt[[1]]
		if(!is.null(dim(lt))) {
			return(make_comb_mat_from_matrix(lt, mode = mode, top_n_sets = top_n_sets, min_set_size = min_set_size, complement_size = complement_size))
		}
	}

	if(missing(value_fun)) {
		if(inherits(lt[[1]], "GRanges")) {
			value_fun = function(x) sum(as.numeric(getFromNamespace("width", ns = "BiocGenerics")(x)))
		} else if(inherits(lt[[1]], "IRanges")) {
			value_fun = function(x) sum(as.numeric(getFromNamespace("width", ns = "BiocGenerics")(x)))
		} else {
			value_fun = length
		}
	}
	make_comb_mat_from_list(lt, value_fun, mode = mode, top_n_sets = top_n_sets, min_set_size = min_set_size, complement_size = complement_size)
}


binaryToInt = function(x) {
	sum(x * 2^(rev(seq_along(x)) - 1))
}

intToBinary = function(x, len) {
	rev(as.integer(intToBits(x))[1:len])
}

# == title
# Set Names
#
# == param
# -m A combination matrix returned by `make_comb_mat`.
#
# == value
# A vector of set names.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m = make_comb_mat(lt)
# set_name(m)
set_name = function(m) {
	set_on_rows = attr(m, "set_on_rows")
	if(set_on_rows) {
		rownames(m)
	} else {
		colnames(m)
	}
}

# == title
# Set Sizes
#
# == param
# -m A combination matrix returned by `make_comb_mat`.
#
# == value
# A vector of set sizes.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m = make_comb_mat(lt)
# set_size(m)
set_size = function(m) {
	attr(m, "set_size")
}

# == title
# Sizes of the Combination sets
#
# == param
# -m A combination matrix returned by `make_comb_mat`.
#
# == value
# A vector of sizes of the combination sets.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m = make_comb_mat(lt)
# comb_size(m)
comb_size = function(m) {
	attr(m, "comb_size")
}

# == title
# Names of the Combination sets
#
# == param
# -m A combination matrix returned by `make_comb_mat`.
#
# == details
# The name of the combination sets are formatted as a string
# of binary bits. E.g. for three sets of "a", "b", "c", the combination
# set with name "101" corresponds to select set a, not select set b
# and select set c. The definition of "select" depends on the value of
# ``mode`` from `make_comb_mat`.
# 
# == value
# A vector of names of the combination sets.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m = make_comb_mat(lt)
# comb_name(m)
comb_name = function(m) {
	set_on_rows = attr(m, "set_on_rows")
	if(set_on_rows) {
		apply(m, 2, paste, collapse = "")
	} else {
		apply(m, 1, paste, collapse = "")
	}
}

# == title
# Degrees of the Combination sets
#
# == param
# -m A combination matrix returned by `make_comb_mat`.
#
# == details
# The degree for a combination set is the number of sets that are selected.
#
# == value
# A vector of degrees of the combination sets.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m = make_comb_mat(lt)
# comb_degree(m)
comb_degree = function(m) {
	set_on_rows = attr(m, "set_on_rows")
	if(set_on_rows) {
		colSums(m)
	} else {
		rowSums(m)
	}
}

# == title
# Extract Elements in a Combination set
#
# == param
# -m A combination matrix returned by `make_comb_mat`.
# -comb_name The valid combination set name should be from `comb_name`.
#
# == details
# It returns the combination set.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m = make_comb_mat(lt)
# extract_comb(m, "110")
extract_comb = function(m, comb_name) {

	if(grepl("^0+$", comb_name)) {
		stop_wrap(qq("Cannot extract elements for the complement set '@{comb_name}'."))
	}

	all_comb_names = comb_name(m)
	if(!comb_name %in% all_comb_names) {
		stop_wrap(paste0("Cannot find a combination name:	", comb_name, ", valid combination name should be in `comb_name(m)`."))
	}

	query = as.numeric(strsplit(comb_name, "")[[1]])

	x = attr(m, "x")
	lt = attr(m, "lt")
	mode = attr(m, "mode")
	if(!is.null(x)) {
		if(mode == "distinct") {
			l = apply(x, 1, function(y) all(y == query))
		} else if(mode == "intersect") {
			l_subset = query == 1
			l = apply(x, 1, function(y) all(y[l_subset] == 1))
		} else if(mode == "union") {
			l_subset = query == 1
			l = apply(x, 1, function(y) {
				if(all(y[!l_subset] == 0)) {
					sum(y[l_subset]) > 0
				} else {
					FALSE
				}
			})
		}

		rn = rownames(x)
		if(is.null(rn)) {
			return(seq_len(nrow(x))[l])
		} else {
			return(rn[l])
		}
	}
	if(!is.null(lt)) {

		if(inherits(lt[[1]], "GRanges")) {
	    	union = getFromNamespace("union", ns = "BiocGenerics")
	    	intersect = getFromNamespace("intersect", ns = "BiocGenerics")
	    	setdiff = getFromNamespace("setdiff", ns = "BiocGenerics")
	    } else if(inherits(lt[[1]], "IRanges")) {
	    	union = getFromNamespace("union", ns = "BiocGenerics")
	    	intersect = getFromNamespace("intersect", ns = "BiocGenerics")
	    	setdiff = getFromNamespace("setdiff", ns = "BiocGenerics")
	    }

		do_comb = function(lt, mode, do = rep(TRUE, length(lt))) {
	        set1_index = which(do)
	        set2_index = which(!do)

	        s = lt[[ set1_index[1] ]]
	        
	        if(mode == "distinct") {
		        for(i in set1_index[-1]) {
		            s = intersect(s, lt[[ i ]])
		        }

		        for(i in set2_index) {
		            s = setdiff(s, lt[[ i ]])
		        }
		    } else if(mode == "intersect") {
		    	for(i in set1_index[-1]) {
		            s = intersect(s, lt[[ i ]])
		        }
		    } else if(mode == "union") {
		    	for(i in set1_index[-1]) {
		            s = union(s, lt[[ i ]])
		        }
		    }
	        s
	    }

	    do = as.logical(as.numeric(strsplit(comb_name, "")[[1]]))
	    do_comb(lt, mode, do)
	}
}

# == title
# Transpost the Combination Matrix
#
# == param
# -x A combination matrix returned by `make_comb_mat`.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m = make_comb_mat(lt)
# t(m)
t.comb_mat = function(x) {
	x2 = t.default(x)
	attr(x2, "set_on_rows") = !attr(x, "set_on_rows")
	x2
}

# == title
# Subset the Combination Matrix
#
# == param
# -x A combination matrix returned by `make_comb_mat`.
# -i Indices on rows.
# -j Indices on columns.
# -drop It is always reset to ``FALSE`` internally.
#
# == details
# If sets are on rows of the combination matrix, the row indices correspond
# to sets and column indices correspond to combination sets, and if sets are
# on columns of the combination matrix, rows correspond to the combination sets.
#
# If the index is one-dimension, e.g. ``x[i]``, the index always corresponds to the combination sets.
#
# You should not subset by the sets. It will give you wrong combination set size. The subsetting
# on sets are only used internally.
#
# This subsetting method is mainly for subsetting combination sets, i.e., users
# can first use `comb_size` to get the size of each combination set, and filter them
# by the size.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m = make_comb_mat(lt)
# m2 = m[, comb_size(m) >= 3]
# comb_size(m2)
# m[comb_size(m) >= 3]
"[.comb_mat" = function(x, i, j, drop = FALSE) {
	set_size = attr(x, "set_size")
	comb_size = attr(x, "comb_size")
	set_on_rows = attr(x, "set_on_rows")
	mode = attr(x, "mode")

	class(x) = "matrix"
	if(set_on_rows) {
		if(nargs() == 2) {
			x2 = x[, i, drop = FALSE]
			comb_size = comb_size[i]
		} else if(missing(i)) {
			x2 = x[, j, drop = FALSE]
			comb_size = comb_size[j]
		} else if(missing(j)) {
			x2 = x[i, , drop = FALSE]
			set_size = set_size[i]
		} else {
			x2 = x[i, j, drop = FALSE]
			set_size = set_size[i]
			comb_size = comb_size[j]
		}
	} else {
		if(nargs() == 2) {
			x2 = x[i, , drop = FALSE]
			comb_size = comb_size[i]
		} else if(missing(i)) {
			x2 = x[, j, drop = FALSE]
			set_size = set_size[j]
		} else if(missing(j)) {
			x2 = x[i, , drop = FALSE]
			comb_size = comb_size[i]
		} else {
			x2 = x[i, j, drop = FALSE]
			comb_size = comb_size[i]
			set_size = set_size[j]
		}
	}

	attr(x2, "set_size") = set_size
	attr(x2, "comb_size") = comb_size
	attr(x2, "mode") = mode
	attr(x2, "set_on_rows") = set_on_rows
	attr(x2, "x") = attr(x, "x")
	attr(x2, "lt") = attr(x, "lt")
	class(x2) = c("comb_mat", "matrix")
	return(x2)
}

# == title
# Print the comb_mat Object
#
# == param
# -x A combination matrix returned by `make_comb_mat`.
# -... Other arguments
#
print.comb_mat = function(x, ...) {
	set_size = attr(x, "set_size")
	comb_size = attr(x, "comb_size")
	set_on_rows = attr(x, "set_on_rows")
	mode = attr(x, "mode")

	cat("A combination matrix with", length(set_size), "sets and", length(comb_size), "combinations.\n")
	cat("  ranges of #combination set: c(", min(comb_size), ", ", max(comb_size), ").\n", sep = "")
	cat("  mode for the combination size: ", mode, ".\n", sep = "")
	if(set_on_rows) {
		cat("  sets are on rows.\n")
	} else {
		cat("  sets are on columns\n")
	}
	cat("
Utility functions that can be applied:
- set_name(): name of the sets.
- set_size(): size of the sets.
- comb_name(): name of the combination sets.
- comb_size(): size of the combination sets.
- comb_degree(): degree of the combination sets.
- extract_comb(): extract elements in the specific combination sets.
- t(): transpose the combination matrix.
- '[': subset the combination matrix.
")

}

# == title
# Make the UpSet plot
#
# == param
# -m A combination matrix returned by `make_comb_mat`. The matrix can be transposed to switch
#    the position of sets and combination sets.
# -comb_col The color for the dots representing combination sets.
# -pt_size The point size for the dots representing combination sets.
# -lwd The line width for the combination sets.
# -set_order The order of sets.
# -comb_order The order of combination sets.
# -top_annotation A `HeatmapAnnotation` object on top of the combination matrix.
# -right_annotation A `HeatmapAnnotation` object on the right of the combination matrix.
# -row_names_side The side of row names.
# -... Other arguments passed to `Heatmap`.
#
# == details
# By default, the sets are on rows and combination sets are on columns. The positions of the
# two types of sets can be switched by transposing the matrix.
#
# When sets are on rows, the default top annotation is the barplot showing the size of each
# combination sets and the default right annotation is the barplot showing the size of the sets.
# The annotations are simply constructed by `HeatmapAnnotation` and `anno_barplot` with some
# parameters pre-set. Users can check the source code of `upset_top_annotation` and
# `upset_right_annotation` to find out how the annotations are defined.
#
# To change or to add annotations, users just need to define a new `HeatmapAnnotation` object.
# E.g. if we want to change the side of the axis and name on top annotation:
#
#     Upset(..., top_annotation = 
#         HeatmapAnnotation(
#            "Intersection size" = anno_barplot(
#                comb_size(m), 
#                border = FALSE, 
#                gp = gpar(fill = "black"), 
#                height = unit(2, "cm"),
#                axis_param = list(side = "right")
#            ), 
#            annotation_name_side = "right", 
#            annotation_name_rot = 0)
#     )
#
# To add more annotations on top, users just add it in `HeatmapAnnotation`:
#
#     Upset(..., top_annotation = 
#         HeatmapAnnotation(
#            "Intersection size" = anno_barplot(
#                comb_size(m), 
#                border = FALSE, 
#                gp = gpar(fill = "black"), 
#                height = unit(2, "cm"),
#                axis_param = list(side = "right")
#            ), 
#            "anno1" = anno_points(...),
#            "anno2" = some_vector, 
#            annotation_name_side = "right", 
#            annotation_name_rot = 0)
#     )
#
# And so is for the right annotations.
#
# `UpSet` returns a `Heatmap-class` object, which means, you can add it with other heatmaps and annotations
# by ``+`` or `\%v\%`.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m = make_comb_mat(lt)
# UpSet(m)
# UpSet(t(m))
# 
# m = make_comb_mat(lt, mode = "union")
# UpSet(m)
# UpSet(m, comb_col = c(rep(2, 3), rep(3, 3), 1))
#
#
# # compare two UpSet plots
# set.seed(123)
# lt1 = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m1 = make_comb_mat(lt1)
# set.seed(456)
# lt2 = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m2 = make_comb_mat(lt2)
#
# max1 = max(c(set_size(m1), set_size(m2)))
# max2 = max(c(comb_size(m1), comb_size(m2)))
#
# UpSet(m1, top_annotation = upset_top_annotation(m1, ylim = c(0, max2)),
#     right_annotation = upset_right_annotation(m1, ylim = c(0, max1)),
#     column_title = "UpSet1") +
# UpSet(m2, top_annotation = upset_top_annotation(m2, ylim = c(0, max2)),
#     right_annotation = upset_right_annotation(m2, ylim = c(0, max1)),
#     column_title = "UpSet2")
#
UpSet = function(m, 
	comb_col = "black",
	pt_size = unit(3, "mm"), lwd = 2,
	set_order = order(set_size(m), decreasing = TRUE), 
	comb_order = if(attr(m, "set_on_rows")) {
			order.comb_mat(m[set_order, ], decreasing = TRUE)
		} else {
			order.comb_mat(m[, set_order], decreasing = TRUE)
		},
	top_annotation = upset_top_annotation(m),
	right_annotation = upset_right_annotation(m),
	row_names_side = "left",
	...) {

	set_on_rows = attr(m, "set_on_rows")
	mode = attr(m, "mode")

	m2 = m
	
	class(m2) = "matrix"

	pt_size = pt_size
	lwd = lwd

	if(set_on_rows) {
		n_comb = ncol(m)
		if(length(comb_col == 1)) comb_col = rep(comb_col, n_comb)

		layer_fun = function(j, i, x, y, w, h, fill) {
			nr = round(1/as.numeric(h[1]))
			nc = round(1/as.numeric(w[1]))
			subm = matrix(pindex(m2, i, j), nrow = nr, byrow = FALSE)
			for(k in seq_len(nr)) {
				if(k %% 2) {
					grid.rect(y = k/nr, height = 1/nr, just = "top", gp = gpar(fill = "#F0F0F0", col = NA))
				}
			}
			grid.points(x, y, size = pt_size, pch = 16, gp = gpar(col = ifelse(pindex(m2, i, j), comb_col[j], "#CCCCCC")))
			jj = unique(j)
			for(k in seq_len(nc)) {
		        if(sum(subm[, k]) >= 2) {
		            i_min = min(which(subm[, k] > 0))
		            i_max = max(which(subm[, k] > 0))
		            grid.lines(c(k - 0.5, k - 0.5)/nc, (nr - c(i_min, i_max) + 0.5)/nr, gp = gpar(col = comb_col[jj[k]], lwd = lwd))
		        }
		    }
		}

		# check top annotation
		# if it is specified by upset_top_annotation and gp(col) is not set
		ra = top_annotation
		if(length(ra) == 1) {
			ta_call = substitute(top_annotation)
			ta_call = as.list(ta_call)
			if(as.character(ta_call[[1]]) == "upset_top_annotation") {
				if(!"gp" %in% names(as.list(ta_call))) {
					ra@anno_list[[1]]@fun@var_env$gp$fill = comb_col
					ra@anno_list[[1]]@fun@var_env$gp$col = comb_col
				}
			}
		}

		ht = Heatmap(m2, cluster_rows = FALSE, cluster_columns = FALSE, rect_gp = gpar(type = "none"),
			layer_fun = layer_fun, show_heatmap_legend = FALSE,
			top_annotation = ra,
			right_annotation = right_annotation,
			row_names_side = row_names_side,
			row_order = set_order, column_order = comb_order, ...)
	} else {
		n_comb = nrow(m)
		if(length(comb_col == 1)) comb_col = rep(comb_col, n_comb)

		layer_fun = function(j, i, x, y, w, h, fill) {
			nr = round(1/as.numeric(h[1]))
			nc = round(1/as.numeric(w[1]))
			subm = matrix(pindex(m2, i, j), nrow = nr, byrow = FALSE)
			for(k in seq_len(nc)) {
				if(k %% 2) {
					grid.rect(x = k/nc, width = 1/nc, just = "right", gp = gpar(fill = "#F0F0F0", col = NA))
				}
			}
			grid.points(x, y, size = pt_size, pch = 16, gp = gpar(col = ifelse(pindex(m2, i, j), comb_col[i], "#CCCCCC")))
			ii = unique(i)
			for(k in seq_len(nr)) {
		        if(sum(subm[k, ]) >= 2) {
		            i_min = min(which(subm[k, ] > 0))
		            i_max = max(which(subm[k, ] > 0))
		            grid.lines((c(i_min, i_max) - 0.5)/nc, (nr - c(k ,k) + 0.5)/nr, gp = gpar(col = comb_col[ii[k]], lwd = lwd))
		        }
		    }
		}

		ra = right_annotation
		if(length(ra) == 1) {
			ta_call = substitute(top_annotation)
			ta_call = as.list(ta_call)
			if(as.character(ta_call[[1]]) == "upset_right_annotation") {
				if(!"gp" %in% names(as.list(ta_call))) {
					ra@anno_list[[1]]@fun@var_env$gp$fill = comb_col
					ra@anno_list[[1]]@fun@var_env$gp$col = comb_col
				}
			}
		}
		ht = Heatmap(m2, cluster_rows = FALSE, cluster_columns = FALSE, rect_gp = gpar(type = "none"),
			layer_fun = layer_fun, show_heatmap_legend = FALSE,
			top_annotation = top_annotation,
			right_annotation = ra,
			row_order = comb_order, column_order = set_order, ...)
	}
	ht
}

# == title
# Order of the Combination Sets
#
# == param
# -m A combination matrix returned by `make_comb_mat`.
# -on On sets or on combination sets?
# -decreasing Whether the ordering is applied decreasingly.
#
# == details
# It first sorts by the degree of the combination sets then
# by the combination matrix.
#
order.comb_mat = function(m, decreasing = TRUE, on = "comb_set") {
	if(on == "set") {
		return(order(set_size(m), decreasing = decreasing))
	} else {
		set_on_rows = attr(m, "set_on_rows")
		if(set_on_rows) {
			lt = list(comb_degree(m))
			lt = c(lt, as.list(as.data.frame(t(m))))
			lt$decreasing = decreasing
			do.call(order, lt)
		} else {
			lt = list(comb_degree(m))
			lt = c(lt, as.list(as.data.frame(m)))
			lt$decreasing = decreasing
			do.call(order, lt)
		}
	}
}

# == title
# Default UpSet Top Annotation
#
# == param
# -m A combination matrix which is as same as the one for `UpSet`.
# -gp Graphic parameters for bars.
# -height The height of the top annotation.
# -show_annotation_name Whether show annotation names?
# -annotation_name_gp Graphic parameters for anntation names.
# -annotation_name_offset Offset to the annotation name, a `grid::unit` object.
# -annotation_name_side Side of the annotation name.
# -annotation_name_rot Rotation of the annotation name, it can only take values in ``c(00, 90, 180, 270)``.
# -... Passed to `anno_barplot`.
#
# == details
# The default top annotation is actually barplot implemented by `anno_barplot`. For
# how to set the top annotation or bottom annotation in `UpSet`, please refer to `UpSet`.
#
upset_top_annotation = function(m, 
	gp = gpar(fill = "black"), 
	height = unit(ifelse(set_on_rows, 3, 2), "cm"),
	show_annotation_name = TRUE,
	annotation_name_gp = gpar(),
	annotation_name_offset = NULL,
	annotation_name_side = "left",
	annotation_name_rot = 0,
	...) {
	set_on_rows = attr(m, "set_on_rows")
	
	if(set_on_rows) {
		ha = HeatmapAnnotation("Intersection\nsize" = anno_barplot(comb_size(m), 
					border = FALSE, gp = gp, height = height, ...), 
				show_annotation_name = show_annotation_name,
				annotation_name_gp = annotation_name_gp,
				annotation_name_offset = annotation_name_offset,
				annotation_name_side = annotation_name_side,
				annotation_name_rot = annotation_name_rot)
	} else {
		ha = HeatmapAnnotation("Set\nsize" = anno_barplot(set_size(m), border = FALSE, 
					gp = gp, height = height, ...),
				show_annotation_name = show_annotation_name,
				annotation_name_gp = annotation_name_gp,
				annotation_name_offset = annotation_name_offset,
				annotation_name_side = annotation_name_side,
				annotation_name_rot = annotation_name_rot)
	}

	mode = attr(m, "mode")
	if(set_on_rows) {
		if(mode %in% c("distinct", "intersect")) {
			names(ha) = "Intersection\nsize"
		} else {
			names(ha) = "Union\nsize"
		}
	}
	return(ha)
}

# == title
# Default UpSet Right Annotation
#
# == param
# -m A combination matrix which is as same as the one for `UpSet`.
# -gp Graphic parameters for bars.
# -width Width of the right annotation.
# -show_annotation_name Whether show annotation names?
# -annotation_name_gp Graphic parameters for anntation names.
# -annotation_name_offset Offset to the annotation name, a `grid::unit` object.
# -annotation_name_side Side of the annotation name.
# -annotation_name_rot Rotation of the annotation name, it can only take values in ``c(00, 90, 180, 270)``.
# -... Passed to `anno_barplot`.
#
# == details
# The default right annotation is actually barplot implemented by `anno_barplot`. For
# how to set the right annotation or left annotation in `UpSet`, please refer to `UpSet`.
#
upset_right_annotation = function(m,
	gp = gpar(fill = "black"),  
	width = unit(ifelse(set_on_rows, 2, 3), "cm"),
	show_annotation_name = TRUE,
	annotation_name_gp = gpar(),
	annotation_name_offset = NULL,
	annotation_name_side = "bottom",
	annotation_name_rot = NULL,
	...) {
	set_on_rows = attr(m, "set_on_rows")

	if(set_on_rows) {
		ha = rowAnnotation("Set size" = anno_barplot(set_size(m), border = FALSE, 
					gp = gp, width = width, ...),
				show_annotation_name = show_annotation_name,
				annotation_name_gp = annotation_name_gp,
				annotation_name_offset = annotation_name_offset,
				annotation_name_side = annotation_name_side,
				annotation_name_rot = annotation_name_rot)
	} else {
		ha = rowAnnotation("Intersection\nsize" = anno_barplot(comb_size(m), 
					border = FALSE, gp = gp, width = width, ...),
				show_annotation_name = show_annotation_name,
				annotation_name_gp = annotation_name_gp,
				annotation_name_offset = annotation_name_offset,
				annotation_name_side = annotation_name_side,
				annotation_name_rot = annotation_name_rot)
	}

	mode = attr(m, "mode")
	if(!set_on_rows) {
		if(mode %in% c("distinct", "intersect")) {
			names(ha) = "Intersection\nsize"
		} else {
			names(ha) = "Union size"
		}
	}
	return(ha)
}

# == title
# Normalize a list of combination matrice
#
# == param
# -... If it is a single argument, the value should be a list of combination matrices.
#
# == details
# It normalizes a list of combination matrice to make them have same number and order of sets and combination sets.
#
# The sets (by `set_name`) from all combination matrice should be the same.
#
normalize_comb_mat = function(...) {
	lt = list(...)
	if(length(lt) == 1) {
		if(!is.list(lt)) {
			stop_wrap("If you only specify one argument, it must be a list of comb_mat objects.")
		}
		lt = lt[[1]]
	}

	n = length(lt)
	if(n == 1) {
		stop_wrap("There should be at least two combination matrices.")
	}

	set1 = set_name(lt[[1]])
	for(i in 2:n) {
		if(!setequal(set1, set_name(lt[[i]]))) {
			stop_wrap("The sets of all combination matrices should be identical.")
		}
	}
	all_set_name = set_name(lt[[1]])
	n_set = length(all_set_name)

	all_comb_size = lapply(lt, function(x) {
		set_on_rows = attr(x, "set_on_rows")
		if(set_on_rows) {
			code = apply(x, 2, binaryToInt)
		} else {
			code = apply(x, 1, binaryToInt)
		}
		structure(comb_size(x), names = code)
	})

	all_code = unique(unlist(lapply(all_comb_size, names)))
	comb_mat = do.call(cbind, lapply(as.numeric(all_code), intToBinary, len = n_set))
	rownames(comb_mat) = all_set_name

	all_comb_size = lapply(all_comb_size, function(x) {
		x2 = structure(rep(0, length(all_code)), names = all_code)
		x2[names(x)] = x
		x2
	})

	for(i in seq_along(lt)) {
		x = lt[[i]]
		set_on_rows = attr(x, "set_on_rows")
		attr = attributes(x)
		attr = attr[!names(attr) %in% c("dim", "dimname")]

		if(set_on_rows) {
			x2 = comb_mat
		} else {
			x2 = t(comb_mat)
		}
		for(nm in names(attr)) {
			attr(x2, nm) = attr[[nm]]
		}
		attr(x2, "set_size") = attr$set_size[all_set_name]
		attr(x2, "comb_size") = all_comb_size[[i]]
		
		lt[[i]] = x2
	}
	return(lt)
}

# == title
# str method
#
# == param
# -object A combination matrix returned by `make_comb_mat`.
# -... Other arguments.
#
str.comb_mat = function(object, ...) {
	class(object) = "matrix"
	cat(" A comb_mat class object\n")
	str(object)
}
