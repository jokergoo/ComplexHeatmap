
# special case: the matrix can have no row names
make_comb_mat_from_matrix = function(x, mode, top_n_sets = Inf, min_set_size = -Inf, 
	universal_set = NULL, complement_size = NULL, set_on_rows = TRUE) {

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
	} else {
		stop_wrap("The input should be a matrix or in a format that can be converted to a matrix.")
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
	if(any(is.na(x))) {
		warning_wrap("The matrix contains NA values. Convert to 0/FALSE.")
		x[is.na(x)] = 0
	}

	if(is.null(rownames(x))) {
		x_has_rownames = FALSE
	} else {
		x_has_rownames = TRUE
	}

	# when there is no row name in x, there is no universal_set
	if(!is.null(universal_set)) {
		if(!x_has_rownames) {
			stop_wrap("`x` should have row names when `universal_set` is set.")
		}
	}

	if(is.null(complement_size)) {
		k = sum(rowSums(x) == 0)
		if(k) {
			complement_size = k
		}
	}

	if(!is.null(universal_set)) {

    	x = x[intersect(rownames(x), universal_set), , drop = FALSE]
    	if(nrow(x) == 0) {
    		stop_wrap("There is no combination set left after intersecting to `universal_set`.")
    	}
    	complement_size = length(setdiff(universal_set, rownames(x)))
    }

    set_size = colSums(x)
	l = set_size >= min_set_size & rank(max(set_size) - set_size) <= top_n_sets

	set_size = set_size[l]
	x = x[, l, drop = FALSE]
	
    # universal_set has higher priority than complement_size
	x0 = x
	x = x[rowSums(x) > 0, , drop = FALSE]
	
	if(mode == "distinct") {
		comb_size = table(apply(x, 1, binaryToInt))		
	} else {
		comb_size = table(unlist(apply(x, 1, function(code) {
			code2 = expand_mode(code, mode)
			apply(code2, 1, binaryToInt)
		})))
	}
	class(comb_size) = NULL

	n_set = ncol(x)
	comb_mat = sapply(names(comb_size), function(x) intToBinary(as.integer(x), n_set))
	if(!is.matrix(comb_mat)) comb_mat = matrix(comb_mat, ncol = 1)
	rownames(comb_mat) = colnames(x)
	colnames(comb_mat) = NULL
	
	if(!is.null(complement_size)) {
		comb_mat = cbind(rep(0, nrow(comb_mat)), comb_mat)
		comb_size = c(complement_size, comb_size)
	}

	attributes(comb_size) = NULL

	if(!set_on_rows) {
		comb_mat = t.default(comb_mat)
	}

	attr(comb_mat, "set_size") = unname(set_size)
	attr(comb_mat, "comb_size") = comb_size
	
	attr(comb_mat, "data") = x0

	param = list(mode = mode, 
		universal_set = universal_set, 
		set_on_rows = set_on_rows)
	attr(comb_mat, "param") = param
	
	class(comb_mat) = c("comb_mat", "matrix")
	comb_mat = comb_mat[order.comb_mat(comb_mat)]
	return(comb_mat)
}

# code: a vector of 0 or 1, from distinct mode
expand_mode = function(code, mode = c("intersect", "union")) {
	mode = match.arg(mode)[1]
	if(mode == "intersect") {
		l_one = which(code == 1)
		n = length(l_one)
		code2 = matrix(0, nrow = 2^n - 1, ncol = length(code))
		k = 0
		for(i in seq_len(n)) {
			cm = combn(l_one, i, x_is_set = TRUE)
			for(j in seq_len(ncol(cm))) {
				k = k + 1
				code2[k, cm[, j]] = 1
			}
		}
	} else if(mode == "union") {
		l_one = which(code == 1)
		n = length(l_one)
		all_n = length(code)
		code2 = matrix(0, nrow = 2^(all_n-1)*n, ncol = length(code))
		k = 0
		for(i in seq_len(n)) {
			# any combination of the remaining all_n - 1
			other = setdiff(1:all_n, l_one[1:i])
			for(i2 in 0:(all_n - i)) {
				cm = combn(other, i2, x_is_set = TRUE)
				for(j in seq_len(ncol(cm))) {
					k = k + 1
					code2[k, l_one[i]] = 1
					code2[k, cm[, j]] = 1
				}
			}
		}
		code2 = code2[seq_len(k), , drop = FALSE]
	}
	return(code2)
}

combn = function(x, m, x_is_set = FALSE) {
	if(length(x) == 1 && x_is_set) {
		if(m == 0) {
			return(utils::combn(1, 0))
		} else {
			return(matrix(x, nrow = 1))
		}
	} else {
		utils::combn(x, m)
	}
}

make_comb_mat_from_list = function(lt, mode, value_fun = length, top_n_sets = Inf, 
	min_set_size = -Inf, universal_set = NULL, complement_size = NULL,
	set_on_rows = TRUE) {

	n = length(lt)
	if(n > 15) {
		stop_wrap("Currently number of sets <= 15 is only supported when the input is a list.")
	}
    nm = names(lt)
    if(is.null(nm)) {
    	stop_wrap("The list must have names.")
    }

    if(inherits(lt, "GRangesList")) {
    	lt = as.list(lt)
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

    if(!is.null(universal_set)) {
    	lt = lapply(lt, function(x) intersect(x, universal_set))
    	complement_set = universal_set
    	for(i in seq_along(lt)) {
    		complement_set = setdiff(complement_set, lt[[i]])
    	}
    	complement_size = value_fun(complement_set)
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

    l = comb_size > 0
    comb_mat = comb_mat[, l, drop = FALSE]
    comb_size = comb_size[l]

    if(!is.null(complement_size)) {
		comb_mat = cbind(rep(0, nrow(comb_mat)), comb_mat)
		comb_size = c(complement_size, comb_size)
	}

	attributes(comb_size) = NULL

    if(!set_on_rows) {
		comb_mat = t.default(comb_mat)
	}

	attr(comb_mat, "set_size") = unname(set_size)
	attr(comb_mat, "comb_size") = comb_size
	
	attr(comb_mat, "data") = lt

	param = list(mode = mode, 
		value_fun = value_fun,
		universal_set = universal_set, 
		set_on_rows = set_on_rows)
	attr(comb_mat, "param") = param
	
	class(comb_mat) = c("comb_mat", "matrix")
	comb_mat = comb_mat[order.comb_mat(comb_mat)]
	return(comb_mat)
}

# == title
# Convert a List of Sets to a Binary Matrix
#
# == param
# -lt A list of vectors.
# -universal_set The universal set.
#
# == details
# It converts the list which have m sets to a binary matrix with n rows and m columns
# where n is the size of universal set.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 5),
#           b = sample(letters, 10),
#           c = sample(letters, 15))
# list_to_matrix(lt)
# list_to_matrix(lt, universal_set = letters)
list_to_matrix = function(lt, universal_set = NULL) {
	if(!is.null(universal_set)) {
		lt = lapply(lt, function(x) intersect(x, universal_set))
	} else {
		universal_set = unique(unlist(lt))
	}

	mat = matrix(0, nrow = length(universal_set), ncol = length(lt))
	rownames(mat) = sort(universal_set)
	colnames(mat) = names(lt)
	for(i in seq_along(lt)) {
		mat[as.character(unique(lt[[i]])), i] = 1
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
# -universal_set The universal set. If it is set, the size of the complement set of all sets is also calculated.
#                It if is specified, ``complement_size`` is ignored.
# -complement_size The size for the complement of all sets. If it is specified, the combination
#                  set name will be like "00...".
# -value_fun For each combination set, how to calculate the size? If it is a scalar set, 
#      the length of the vector is the size of the set, while if it is a region-based set,
#      (i.e. ``GRanges`` or ``IRanges`` object), the sum of widths of regions in the set is
#      calculated as the size of the set.
# -set_on_rows Used internally.
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
# require(circlize)
# require(GenomicRanges)
# lt = lapply(1:4, function(i) generateRandomBed())
# lt = lapply(lt, function(df) GRanges(seqnames = df[, 1], 
#     ranges = IRanges(df[, 2], df[, 3])))
# names(lt) = letters[1:4]
# m = make_comb_mat(lt)
# }
make_comb_mat = function(..., mode = c("distinct", "intersect", "union"),
	top_n_sets = Inf, min_set_size = -Inf,
	universal_set = NULL, complement_size = NULL, 
	value_fun = NULL, set_on_rows = TRUE) {

	lt = list(...)

	if("remove_complement_set" %in% names(lt)) {
		stop_wrap("Argument `remove_complement_set` has been removed.")
	}

	mode = match.arg(mode)[1]
	if(length(lt) == 1) {
		lt = lt[[1]]
		if(length(dim(lt)) == 2) { # a matrix

			if(ncol(lt) > 31) {
				stop_wrap("Only support number of sets <= 31.")
			}

			m = make_comb_mat_from_matrix(lt, mode = mode, top_n_sets = top_n_sets, 
				min_set_size = min_set_size, universal_set = universal_set, complement_size = complement_size,
				set_on_rows = set_on_rows)
			return(m)
		}
	}

	if(is.null(value_fun)) {
		if(inherits(lt[[1]], "GRanges")) {
			value_fun = function(x) sum(as.numeric(getFromNamespace("width", ns = "BiocGenerics")(x)))
		} else if(inherits(lt[[1]], "IRanges")) {
			value_fun = function(x) sum(as.numeric(getFromNamespace("width", ns = "BiocGenerics")(x)))
		} else {
			value_fun = length
		}
	}

	if(length(lt) > 31) {
		stop_wrap("Only support number of sets <= 31.")
	}

	# if lt is a list of atomic sets, convert to the matrix because it is more efficient
	if(is.atomic(lt[[1]])) {
		m = make_comb_mat_from_matrix(list_to_matrix(lt), mode = mode, top_n_sets = top_n_sets, 
			min_set_size = min_set_size, universal_set = universal_set, complement_size = complement_size,
			set_on_rows = set_on_rows)
		return(m)
	}
	
	m = make_comb_mat_from_list(lt, value_fun, mode = mode, top_n_sets = top_n_sets, min_set_size = min_set_size, 
		universal_set = universal_set, complement_size = complement_size, set_on_rows = set_on_rows)
	return(m)
}

# == title
# Full set of code of combination sets
#
# == param
# -n Number of sets
# -complement Whether include the code for complement set?
#
# == example
# full_comb_code(2)
# full_comb_code(3)
# full_comb_code(4)
# full_comb_code(4, TRUE)
full_comb_code = function(n, complement = FALSE) {
	comb_mat = matrix(0, nrow = n, ncol = sum(choose(n, (!complement + 0):n)))
    j = 1
    for(k in 1:n) {
        comb = combn(n, k)
        for(i in 1:ncol(comb)) {
            comb_mat[comb[, i], j] = 1
            j = j + 1
        }
    }

    lt = list(colSums(comb_mat))
	lt = c(lt, as.list(as.data.frame(t(comb_mat))))
	lt$decreasing = TRUE
	od = do.call(order, lt)

    apply(comb_mat[, od, drop = FALSE], 2, paste, collapse = "")
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
	set_on_rows = attr(m, "param")$set_on_rows
	if(set_on_rows) {
		rownames(m)
	} else {
		colnames(m)
	}
}

# == title
# Modify Set Names
#
# == param
# -x A combination matrix returned by `make_comb_mat`.
# -value New set names.
# -... Other arguments.
#
# == example
# set.seed(123)
# lt = list(a = sample(letters, 10),
#           b = sample(letters, 15),
#           c = sample(letters, 20))
# m = make_comb_mat(lt)
# set_name(m) = c("A", "B", "C")
# m
"set_name<-" = function (x, ..., value) {
	old_set_name = set_name(x)
	n1 = length(old_set_name)
	n2 = length(value)
	param = attr(x, "param")
	set_on_rows = param$set_on_rows
	data = attr(x, "data")

	if(n1 != n2) {
		stop_wrap("New set names should have the same length as the old ones.")
	}

	if(set_on_rows) {
		attr(x, "dimnames")[[1]] = value
	} else {
		attr(x, "dimnames")[[2]] = value
	}
	if(is.matrix(data)) {
		colnames(data) = value
	} else {
		names(data) = value
	}
	attr(x, "data") = data
	return(x)
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
	structure(attr(m, "set_size"), names = set_name(m))
}

# == title
# Complement Set Size
#
# == param
# -m A combination matrix returned by `make_comb_mat`.
#
# == value
# If there is no complement set, it returns zero.
#
complement_size = function(m) {
	sz = comb_size(m)
	l = grepl("^0+$", names(sz))
	if(any(l)) {
		return(unname(sz[l]))
	} else {
		return(0)
	}
}

# == title
# Sizes of the Combination sets
#
# == param
# -m A combination matrix returned by `make_comb_mat`.
# -degree degree of the intersection. The value can be a vector.
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
comb_size = function(m, degree = NULL) {
	x = structure(attr(m, "comb_size"), names = comb_name(m))
	if(is.null(degree)) {
		return(x)
	} else {
		x[comb_degree(m) %in% degree]
	}
}

# == title
# Names of the Combination sets
#
# == param
# -m A combination matrix returned by `make_comb_mat`.
# -readable Whether the combination represents as e.g. "A&B&C".
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
# comb_name(m, readable = TRUE)
comb_name = function(m, readable = FALSE) {
	set_on_rows = attr(m, "param")$set_on_rows
	if(set_on_rows) {
		nm = apply(m, 2, paste, collapse = "")
	} else {
		nm = apply(m, 1, paste, collapse = "")
	}

	if(readable) {
		mode = attr(m, "param")$mode
		sn = set_name(m)

		if(mode == "distinct") {
			nm = sapply(strsplit(nm, ""), function(x) {
				x = as.numeric(x)
				sn2 = paste(ifelse(x, "", "(!"), sn, ifelse(x, "", ")"), sep = "")
				paste(sn2, collapse = "&")
			})
		} else if(mode == "intersect") {
			nm = sapply(strsplit(nm, ""), function(x) {
				l = as.logical(as.numeric(x))
				paste(sn[l], collapse = "&")
			})
		} else if(mode == "union") {
			nm = sapply(strsplit(nm, ""), function(x) {
				l = as.logical(as.numeric(x))
				paste(sn[l], collapse = "|")
			})
		}
	}

	return(nm)
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
	set_on_rows = attr(m, "param")$set_on_rows
	if(set_on_rows) {
		d = colSums(m)
	} else {
		d = rowSums(m)
	}
	structure(d, names = comb_name(m))
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
	if(length(comb_name) != 1) {
		stop_wrap('`comb_name` should be length one.')
	}

	all_comb_names = comb_name(m)
	if((!comb_name %in% all_comb_names) && comb_name %in% full_comb_code(length(set_size(m)))) {
		return(NULL)
	} else if(!comb_name %in% all_comb_names) {
		stop_wrap(paste0("Cannot find a combination name:	", comb_name, ", valid combination name should be in `comb_name(m)`."))
	}

	query = as.numeric(strsplit(comb_name, "")[[1]])

	data = attr(m, "data")
	param = attr(m, "param")
	universal_set = param$universal_set
	mode = param$mode

	is_complement_set = function(comb_name) {
		grepl("^0+$", comb_name)
	}

	if(is.matrix(data)) {
		x = data
		if(!is.null(universal_set)) {
			if(is_complement_set(comb_name)) {
				return(setdiff(universal_set, rownames(x)))
			}
		}

		if(mode == "distinct") {
			l = apply(x, 1, function(y) all(y == query))
		} else if(mode == "intersect") {
			l_subset = query == 1
			l = apply(x, 1, function(y) all(y[l_subset] == 1))
		} else if(mode == "union") {
			l_subset = query == 1
			l = apply(x, 1, function(y) {
				any(y[l_subset] == 1)
			})
		}

		rn = rownames(x)
		if(is.null(universal_set)) {
			if(is.null(rn)) {
				return(seq_len(nrow(x))[l])
			} else {
				return(rn[l])
			}
		} else {
			return(rn[l])
		}
	} else if(is.list(data)) {
		lt = data

		if(inherits(lt[[1]], "GRanges")) {
	    	union = getFromNamespace("union", ns = "BiocGenerics")
	    	intersect = getFromNamespace("intersect", ns = "BiocGenerics")
	    	setdiff = getFromNamespace("setdiff", ns = "BiocGenerics")
	    } else if(inherits(lt[[1]], "IRanges")) {
	    	union = getFromNamespace("union", ns = "BiocGenerics")
	    	intersect = getFromNamespace("intersect", ns = "BiocGenerics")
	    	setdiff = getFromNamespace("setdiff", ns = "BiocGenerics")
	    }

	    if(is_complement_set(comb_name)) {
			s = universal_set
			for(i in seq_along(lt)) {
				s = setdiff(s, lt[[i]])
			}
			return(s)
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
	attr(x2, "param")$set_on_rows = !attr(x, "param")$set_on_rows
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
	set_on_rows = attr(x, "param")$set_on_rows
	
	if(set_on_rows) {
		if(nargs() == 2) {
			x2 = subset_by_comb_ind(x, i, 2)
		} else if(missing(i)) {
			x2 = subset_by_comb_ind(x, j, 2)
		} else if(missing(j)) {
			x2 = subset_by_set_ind(x, i, 1)
		} else {
			if(can_both_subset(x, i)) {
				x2 = subset_by_comb_ind(x, j, 2)
				x2 = subset_by_set_ind(x2, i, 1)
			} else {
				stop_wrap("Cannot apply subsetting on combination sets and sets simultaneously.")
			}
		}
	} else {
		if(nargs() == 2) {
			x2 = subset_by_comb_ind(x, i, 1)
		} else if(missing(i)) {
			x2 = subset_by_set_ind(x, j, 2)
		} else if(missing(j)) {
			x2 = subset_by_comb_ind(x, i, 1)
		} else {
			if(can_both_subset(x, j)) {
				x2 = subset_by_comb_ind(x, i, 1)
				x2 = subset_by_set_ind(x2, j, 2)
			} else {
				stop_wrap("Cannot apply subsetting on combination sets and sets simultaneously.")
			}
		}
	}

	return(x2)
}

can_both_subset = function(x, set_selected) {
	set_name = set_name(x)
	set_size = set_size(x)
	if(is.numeric(set_selected)) set_selected = set_name[set_selected]

	non_empty_set = set_name[set_size > 0]

	length(setdiff(non_empty_set, set_selected)) == 0
}

# when comb_set changes, set does not need to change
# it should also allow new empty comb sets
subset_by_comb_ind = function(x, ind, margin = 2) {

	comb_size = comb_size(x)
	set_size = set_size(x)

	class(x) = "matrix"
	n = nrow(x)
	if(is.numeric(ind) || is.logical(ind)) {
		if(margin == 1) {
			x2 = x[ind, , drop = FALSE]
		} else {
			x2 = x[, ind, drop = FALSE]
		}
		comb_size = unname(comb_size[ind])
	} else if(is.character(ind)) {
		ind = unique(ind)
		if(!all(grepl("^(0|1)+$", ind))) {
			stop_wrap("code for combination set should only contain 0 and 1.")
		}
		code = strsplit(ind, "")
		if(any(sapply(code, length) != n)) {
			stop_wrap(qq("code should have @{n} digits."))
		}
		# construct a new comb_mat
		if(margin == 1) {
			x2 = do.call(rbind, lapply(code, as.integer))
			colnames(x2) = names(set_size)
		} else {
			x2 = do.call(cbind, lapply(code, as.integer))
			rownames(x2) = names(set_size)
		}

		comb_size2 = comb_size[ind]
		comb_size2[is.na(comb_size2)] = 0
		comb_size = unname(comb_size2)
	} else {
		stop_wrap("index should be either numeric or character.")
	}

	attr(x2, "set_size") = attr(x, "set_size")
	attr(x2, "comb_size") = comb_size
	attr(x2, "data") = attr(x, "data")
	attr(x2, "param") = attr(x, "param")
	class(x2) = c("comb_mat", "matrix")
	return(x2)
}

# following scenarios do not need re run make_comb_mat:
# 1. add new empty sets
# 2. reorder
# 3. remove empty sets
subset_by_set_ind = function(x, ind, margin = 1) {
	
	data = attr(x, "data")
	param = attr(x, "param")
	class(x) = "matrix"

	if(is.logical(ind)) ind = which(ind)

	ind = unique(ind)

	set_size = set_size(x)
	set_name = names(set_size)
	n = length(set_size)
	empty_set = set_name[set_size == 0]
	non_empty_set = set_name[set_size > 0]
	which_empty_set = which(set_size == 0)

	comb_size = comb_size(x)

	# just reorder the sets
	reorder_set = FALSE
	if(is.numeric(ind)) {
		if(length(setdiff(ind, 1:n))) {
			stop_wrap(qq("nummeric index should not exceed @{n}"))
		}
		if(length(ind) == n) {
			reorder_set = TRUE
		}
		# if the indices not selected are only empty sets
		if(length(setdiff(ind, 1:n)) == 0) {
			if(length(setdiff(setdiff(1:n, ind), which_empty_set)) == 0) {
				reorder_set = TRUE
			}
		}
	}
	if(is.character(ind)) {
		if(length(ind) == n && length(setdiff(ind, set_name)) == 0) {
			reorder_set = TRUE
		}
		if(length(setdiff(ind, set_name)) == 0) {
			if(length(setdiff(setdiff(set_name, ind), empty_set)) == 0) {
				reorder_set = TRUE
			}
		}
	}
	if(reorder_set) {
		if(margin == 1) {
			x = x[ind, , drop = FALSE]
		} else {
			x = x[, ind, drop = FALSE]
		}
		attr(x, "set_size") = set_size[ind]
		attr(x, "comb_size") = comb_size
		if(is.matrix(data)) {
			data = data[, ind, drop = FALSE]
		} else {
			data = data[ind]
		}
		attr(x, "data") = data
		attr(x, "param") = param
		class(x) = c("comb_mat", "matrix")
		return(x)
	}

	rerun = FALSE
	if(is.numeric(ind)) {
		rerun = TRUE
	} else if(is.character(ind)) {
		# index is a subset of the set name
		if(length(setdiff(ind, non_empty_set)) == 0) {
			rerun = TRUE
		}
	}
	if(rerun) {
		if(is.matrix(data)) {
			param$x = data[, ind, drop = FALSE]
			x2 = do.call(make_comb_mat_from_matrix, param)
		} else {
			param$lt = data[ind]
			x2 = do.call(make_comb_mat_from_list, param)
		}
		return(x2)
	}

	## add new empty sets
	if(is.character(ind)) {
		new_set = setdiff(ind, set_name)
		# if ind includes all non-empty sets and has additional empty new sets
		if(length(new_set) && length(setdiff(setdiff(set_name, empty_set), ind)) == 0) {
			all_set = ind
			non_empty_set = setdiff(set_name, empty_set)
			n2 = length(all_set)
			if(margin == 1) {
				x2 = matrix(0, nrow = length(all_set), ncol = ncol(x))
				rownames(x2) = all_set
				for(rn in non_empty_set) x2[rn, ] = x[rn, ]
			} else {
				x2 = matrix(0, ncol = length(all_set), nrow = nrow(x))
				colnames(x2) = all_set
				for(cn in non_empty_set) x2[, cn] = x[, cn]
			}
			set_size2 = structure(rep(0, n2), names = all_set)
			set_size2[non_empty_set] = set_size[non_empty_set]

			if(is.matrix(data)) {
				data2 = matrix(0, nrow = nrow(data), ncol = n2)
				rownames(data2) = rownames(data)
				colnames(data2) = all_set
				data2[, non_empty_set] = data[, non_empty_set]
			} else {
				data2 = rep(list(), n2)
				names(data2) = all_set
				data2[non_empty_set] = data[non_empty_set]
			}

			attr(x2, "set_size") = set_size2
			attr(x2, "comb_size") = attr(x, "comb_size")
			attr(x2, "data") = data
			attr(x2, "param") = attr(x, "param")
			class(x2) = c("comb_mat", "matrix")
			return(x2)
		}
	}

	# partial existed sets and partial empty sets
	if(is.character(ind)) {
		which_existed = which(ind %in% set_name)
		which_new = which(! ind %in% set_name)

		x2 = subset_by_set_ind(x, which_existed, margin = margin)
		x2 = subset_by_set_ind(x2, ind, margin = margin)
		return(x2)
	}

	stop_wrap("subsetting failed.")
}

# == title
# Print the comb_mat Object
#
# == param
# -x A combination matrix returned by `make_comb_mat`.
# -... Other arguments
#
print.comb_mat = function(x, ...) {
	set_size = set_size(x)
	comb_size = comb_size(x)
	set_on_rows = attr(x, "param")$set_on_rows
	mode = attr(x, "param")$mode

	ow =  getOption("width")
	options(width = 9999)
	on.exit(options(width = ow))

	cat("A combination matrix with", length(set_size), "sets and", length(comb_size), "combinations.\n")
	cat("  ranges of combination set size: c(", min(comb_size), ", ", max(comb_size), ").\n", sep = "")
	cat("  mode for the combination size: ", mode, ".\n", sep = "")
	if(set_on_rows) {
		cat("  sets are on rows.\n")
		comb_mat = t.default(x)
	} else {
		cat("  sets are on columns\n")
		comb_mat = x
	}
	if(!is.null(attr(x, "universal_set"))) {
		cat("  universal set is set.\n")
	}
	cat("\n")
	class(comb_mat) = "matrix"
	comb_mat[comb_mat == 1] = "x" 
	comb_mat[comb_mat == "0"] = "" 
	df = as.data.frame(comb_mat, stringsAsFactors = FALSE)
	df = cbind(df, code = names(comb_size), "size" = comb_size)
	df[, 1] = paste0(" ", df[, 1])
	colnames(df)[1] = paste0(" ", colnames(df)[1])
	df2 = df[order(comb_size, decreasing = TRUE), , drop = FALSE]
	if(nrow(df) <= 8) {
		cat("Combination sets are:\n")
		print(df, row.names = FALSE)
	} else {
		cat("Top 8 combination sets are:\n")
		print(df2[1:8, ], row.names = FALSE)
	}

	cat("\n")
	cat("Sets are:\n")
	sz = set_size(x)
	cz = complement_size(x)
	if(cz > 0) {
		sz = c(sz, "complement" = cz)
	}
	sz = data.frame(set = names(sz), size = sz)
	sz[, 1] = paste0(" ", sz[, 1])
	colnames(sz)[1] = paste0(" ", colnames(sz)[1])
	print(sz, row.names = FALSE)

# 	cat("
# Utility functions that can be applied:
# - set_name(): name of the sets.
# - set_size(): size of the sets.
# - comb_name(): name of the combination sets.
# - comb_size(): size of the combination sets.
# - comb_degree(): degree of the combination sets.
# - extract_comb(): extract elements in the specific combination set.
# - t(): transpose the combination matrix on the UpSet plot.
# - '[': subset the combination matrix.
# ")

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
# -bg_col Color for the background rectangles.
# -bg_pt_col Color for the dots representing the set is not selected.
# -set_order The order of sets.
# -comb_order The order of combination sets.
# -top_annotation A `HeatmapAnnotation` object on top of the combination matrix.
# -left_annotation A `HeatmapAnnotation` object on top of the combination matrix.
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
	bg_col = "#F0F0F0", bg_pt_col = "#CCCCCC",
	set_order = order(set_size(m), decreasing = TRUE), 
	comb_order = if(attr(m, "param")$set_on_rows) {
			order.comb_mat(m[set_order, ], decreasing = TRUE)
		} else {
			order.comb_mat(m[, set_order], decreasing = TRUE)
		},
	top_annotation = upset_top_annotation(m),
	right_annotation = upset_right_annotation(m),
	left_annotation = NULL,
	row_names_side = "left",
	...) {

	param = attr(m, "param")
	set_on_rows = param$set_on_rows
	mode = param$mode

	m2 = m
	
	class(m2) = "matrix"

	pt_size = pt_size
	lwd = lwd

	if(!is.null(left_annotation)) {
		if(missing(right_annotation)) {
			right_annotation = NULL
		}
		if(missing(row_names_side)) {
			row_names_side = "right"
		}
	}

	if(length(bg_col) == 1) bg_col = c(bg_col, "white")

	n_set = length(set_size(m))
	bg_col = rep(bg_col, times = n_set)
	bg_col = bg_col[seq_len(n_set)]

	if(set_on_rows) {
		n_comb = ncol(m)
		if(length(comb_col == 1)) comb_col = rep(comb_col, n_comb)

		layer_fun = function(j, i, x, y, w, h, fill) {
			nr = round(1/as.numeric(h[1]))
			nc = round(1/as.numeric(w[1]))
			subm = matrix(pindex(m2, i, j), nrow = nr, byrow = FALSE)
			for(k in seq_len(nr)) {
				grid.rect(y = k/nr, height = 1/nr, just = "top", gp = gpar(fill = bg_col[k], col = NA))
			}
			grid.points(x, y, size = pt_size, pch = 16, gp = gpar(col = ifelse(pindex(m2, i, j), comb_col[j], bg_pt_col)))
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
			if("upset_top_annotation" %in% as.character(ta_call[[1]])) {
				if(!"gp" %in% names(as.list(ta_call))) {
					ra@anno_list[[1]]@fun@var_env$gp$fill = comb_col
					ra@anno_list[[1]]@fun@var_env$gp$col = comb_col
				}
			}
		}

		ht = Heatmap(m2, cluster_rows = FALSE, cluster_columns = FALSE, rect_gp = gpar(type = "none"),
			layer_fun = layer_fun, show_heatmap_legend = FALSE,
			top_annotation = ra,
			right_annotation = right_annotation, left_annotation = left_annotation,
			row_names_side = row_names_side, col = c("0" = bg_pt_col, "1" = comb_col[1]),
			row_order = set_order, column_order = comb_order, ...)
	} else {
		n_comb = nrow(m)
		if(length(comb_col == 1)) comb_col = rep(comb_col, n_comb)

		layer_fun = function(j, i, x, y, w, h, fill) {
			nr = round(1/as.numeric(h[1]))
			nc = round(1/as.numeric(w[1]))
			subm = matrix(pindex(m2, i, j), nrow = nr, byrow = FALSE)
			for(k in seq_len(nc)) {
				grid.rect(x = k/nc, width = 1/nc, just = "right", gp = gpar(fill = bg_col[k], col = NA))
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
			ta_call = substitute(right_annotation)
			ta_call = as.list(ta_call)
			if(as.character(ta_call[[1]]) == "upset_right_annotation") {
				if(!"gp" %in% names(as.list(ta_call))) {
					ra@anno_list[[1]]@fun@var_env$gp$fill = comb_col
					ra@anno_list[[1]]@fun@var_env$gp$col = comb_col
				}
			}
		}
		la = left_annotation
		if(length(la) == 1) {
			ta_call = substitute(right_annotation)
			ta_call = as.list(ta_call)
			if(as.character(ta_call[[1]]) == "upset_left_annotation") {
				if(!"gp" %in% names(as.list(ta_call))) {
					la@anno_list[[1]]@fun@var_env$gp$fill = comb_col
					la@anno_list[[1]]@fun@var_env$gp$col = comb_col
				}
			}
		}
		ht = Heatmap(m2, cluster_rows = FALSE, cluster_columns = FALSE, rect_gp = gpar(type = "none"),
			layer_fun = layer_fun, show_heatmap_legend = FALSE,
			top_annotation = top_annotation,
			right_annotation = ra, left_annotation = la, col = c("0" = bg_pt_col, "1" = comb_col[1]),
			row_order = comb_order, column_order = set_order, ...)
	}
	ht@heatmap_param$type = "UpSet"
	attr(ht, "UpSet") = TRUE
	ht
}

format_genomic_coor = function(x) {
	x2 = character(length(x))
	l1 = x >= 1e6
	x2[l1] = paste(x[l1]/1000000, "MB", sep = " ")
	l2 = x >= 1e3 & !l1
	x2[l2] = paste(x[l2]/1000, "KB", sep = " ")
	
	l3 = !(l1 | l2)
	last_unit = "bp"
	if(all(l1 | x == 0)) {
		last_unit = "MB"
	} else if(all(l1 | l2 | x == 0)) {
		last_unit = "KB"
	}
	x2[l3] = paste(x[l3], last_unit, sep = " ")
	gsub("\\.(\\d\\d)\\d*", "\\.\\1", x2)
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
		set_on_rows = attr(m, "para")$set_on_rows
		if(set_on_rows) {
			lt = list(comb_degree(m))
			class(m) = "matrix"
			lt = c(lt, as.list(as.data.frame(t(m))))
			lt$decreasing = decreasing
			do.call(order, lt)
		} else {
			lt = list(comb_degree(m))
			class(m) = "matrix"
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
# If you want to use `decorate_annotation` function, the annotation name for the "sets"
# is ``set_size`` and the annotation name for the "intersection sets" are ``intersection_size``
# and if under the union mode, it is ``union_size``.
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

	set_on_rows = attr(m, "param")$set_on_rows
	if(set_on_rows) {
		x = comb_size(m)
		if(inherits(attr(m, "data")[[1]], "GRanges")) {
			attr(x, "labels_format") = format_genomic_coor
		}
		ha = HeatmapAnnotation("intersection_size" = anno_barplot(x, 
					border = FALSE, gp = gp, height = height, ...), 
				show_annotation_name = show_annotation_name,
				annotation_name_gp = annotation_name_gp,
				annotation_name_offset = annotation_name_offset,
				annotation_name_side = annotation_name_side,
				annotation_name_rot = annotation_name_rot,
				annotation_label = "Intersection\nsize")
	} else {
		x = set_size(m)
		if(inherits(attr(m, "data")[[1]], "GRanges")) {
			attr(x, "labels_format") = format_genomic_coor
		}
		ha = HeatmapAnnotation("set_size" = anno_barplot(x, border = FALSE, 
					gp = gp, height = height, ...),
				show_annotation_name = show_annotation_name,
				annotation_name_gp = annotation_name_gp,
				annotation_name_offset = annotation_name_offset,
				annotation_name_side = annotation_name_side,
				annotation_name_rot = annotation_name_rot,
				annotation_label = "Set\nsize")
	}

	mode = attr(m, "param")$mode
	if(set_on_rows) {
		if(mode %in% c("distinct", "intersect")) {
			# names(ha) = "intersection_size"
		} else {
			names(ha) = "union_size"
			ha@anno_list[[1]]@label = "Union\nsize"
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
# -... Passed to `anno_barplot`, e.g. to set ``add_numbers``.
#
# == details
# The default right annotation is actually barplot implemented by `anno_barplot`. For
# how to set the right annotation or left annotation in `UpSet`, please refer to `UpSet`.
#
# If you want to use `decorate_annotation` function, the annotation name for the "sets"
# is ``set_size`` and the annotation name for the "intersection sets" are ``intersection_size``
# and if under the union mode, it is ``union_size``.
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

	set_on_rows = attr(m, "param")$set_on_rows

	if(set_on_rows) {
		x = set_size(m)
		if(inherits(attr(m, "data")[[1]], "GRanges")) {
			attr(x, "labels_format") = format_genomic_coor
		}
		ha = rowAnnotation("set_size" = anno_barplot(x, border = FALSE, 
					gp = gp, width = width, ...),
				show_annotation_name = show_annotation_name,
				annotation_name_gp = annotation_name_gp,
				annotation_name_offset = annotation_name_offset,
				annotation_name_side = annotation_name_side,
				annotation_name_rot = annotation_name_rot,
				annotation_label = "Set size")
	} else {
		x = comb_size(m)
		if(inherits(attr(m, "data")[[1]], "GRanges")) {
			attr(x, "labels_format") = format_genomic_coor
		}
		ha = rowAnnotation("intersection_size" = anno_barplot(x, 
					border = FALSE, gp = gp, width = width, ...),
				show_annotation_name = show_annotation_name,
				annotation_name_gp = annotation_name_gp,
				annotation_name_offset = annotation_name_offset,
				annotation_name_side = annotation_name_side,
				annotation_name_rot = annotation_name_rot,
				annotation_label = "Intersection\nsize")
	}

	mode = attr(m, "param")$mode
	if(!set_on_rows) {
		if(mode %in% c("distinct", "intersect")) {
			# names(ha) = "intersection_size"
		} else {
			names(ha) = "union_size"
			ha@anno_list[[1]]@label = "Union size"
		}
	}
	return(ha)
}

# == title
# UpSet Left Annotation
#
# == param
# -m A combination matrix which is as same as the one for `UpSet`.
# -gp Graphic parameters for bars.
# -axis_param Parameters for axis.
# -width Width of the left annotation.
# -show_annotation_name Whether show annotation names?
# -annotation_name_gp Graphic parameters for anntation names.
# -annotation_name_offset Offset to the annotation name, a `grid::unit` object.
# -annotation_name_side Side of the annotation name.
# -annotation_name_rot Rotation of the annotation name, it can only take values in ``c(00, 90, 180, 270)``.
# -... Passed to `anno_barplot`, e.g. to set ``add_numbers``.
#
upset_left_annotation = function(m,
	gp = gpar(fill = "black"),  
	axis_param = list(direction = "reverse"),
	width = unit(ifelse(set_on_rows, 2, 3), "cm"),
	show_annotation_name = TRUE,
	annotation_name_gp = gpar(),
	annotation_name_offset = NULL,
	annotation_name_side = "bottom",
	annotation_name_rot = NULL,
	...) {

	set_on_rows = attr(m, "param")$set_on_rows

	if(!"direction" %in% names(axis_param)) {
		axis_param$direction = "reverse"
	}

	if(set_on_rows) {
		x = set_size(m)
		if(inherits(attr(m, "data")[[1]], "GRanges")) {
			attr(x, "labels_format") = format_genomic_coor
		}
		ha = rowAnnotation("set_size" = anno_barplot(x, border = FALSE, 
					gp = gp, width = width, axis_param = axis_param, ...),
				show_annotation_name = show_annotation_name,
				annotation_name_gp = annotation_name_gp,
				annotation_name_offset = annotation_name_offset,
				annotation_name_side = annotation_name_side,
				annotation_name_rot = annotation_name_rot,
				annotation_label = "Set size")
	} else {
		x = comb_size(m)
		if(inherits(attr(m, "data")[[1]], "GRanges")) {
			attr(x, "labels_format") = format_genomic_coor
		}
		ha = rowAnnotation("intersection_size" = anno_barplot(x, 
					border = FALSE, gp = gp, width = width, axis_param = axis_param, ...),
				show_annotation_name = show_annotation_name,
				annotation_name_gp = annotation_name_gp,
				annotation_name_offset = annotation_name_offset,
				annotation_name_side = annotation_name_side,
				annotation_name_rot = annotation_name_rot,
				annotation_label = "Intersection\nsize")
	}

	mode = attr(m, "param")$mode
	if(!set_on_rows) {
		if(mode %in% c("distinct", "intersect")) {
			# names(ha) = "intersection_size"
		} else {
			names(ha) = "union_size"
			ha@anno_list[[1]]@label = "Union size"
		}
	}
	return(ha)
}

# == title
# Normalize a list of combination matrice
#
# == param
# -... Combination matrices.
# -full_comb_sets Whether the combination matrices contain the full sets of combination sets?
# -complement_set Whether the combination matrices also contain the complement set?
#
# == details
# It normalizes a list of combination matrice to make them have same number and order of sets and combination sets.
#
# The sets (by `set_name`) from all combination matrice should be the same.
#
normalize_comb_mat = function(..., full_comb_sets = FALSE, complement_set = FALSE) {

	lt = list(...)
	input_is_comb_mat = FALSE
	if(length(lt) == 1) {
		if(inherits(lt[[1]], "comb_mat")) {
			input_is_comb_mat = TRUE
		}
		if(is.list(lt[[1]])) {
			lt = lt[[1]]
		}
	}

	n = length(lt)

	all_set_names = sort(unique(unlist(lapply(lt, set_name))))
	if(any(sapply(lt, function(x) attr(x, "param")$set_on_rows) != attr(lt[[1]], "param")$set_on_rows)) {
		stop_wrap("Combination sets should be all on rows or on columns.")
	}
	n_set = length(all_set_names)

	lt = lapply(lt, function(m) {
		if(attr(m, "param")$set_on_rows) {
			m[all_set_names, ]
		} else {
			m[, all_set_names]
		}
	})

	if(full_comb_sets) {
		all_comb_names = full_comb_code(n_set)
	} else {
		all_comb_names = sort(unique(unlist(lapply(lt, comb_name))))
	}
	if(complement_set) {
		all_comb_names = union(all_comb_names, strrep("0", n_set))
	} 
	lt = lapply(lt, function(m) {
		if(attr(m, "param")$set_on_rows) {
			m = m[, all_comb_names]
		} else {
			m = m[all_set_names, ]
		}
		m[order.comb_mat(m)]
	})
	if(input_is_comb_mat) {
		return(lt[[1]])
	} else {
		return(lt)
	}
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
