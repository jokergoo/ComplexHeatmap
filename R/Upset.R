
make_comb_mat_from_matrix = function(x, mode, top_n_sets = Inf, min_set_size = -Inf) {
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
	comb_mat2 = matrix(nr = nrow(comb_mat), nc = nc*(nc-1)/2)
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

	attr(comb_mat, "set_size") = set_size
	attr(comb_mat, "comb_size") = comb_size
	attr(comb_mat, "mode") = mode
	attr(comb_mat, "set_on_rows") = TRUE
	attr(comb_mat, "x") = x
	class(comb_mat) = c("comb_mat", "matrix")
	return(comb_mat)

}

make_comb_mat_from_list = function(lt, mode, value_fun = length, top_n_sets = Inf, min_set_size = -Inf) {
	n = length(lt)
    nm = names(lt)
    if(is.null(nm)) {
    	stop_wrap("The list must have names.")
    }

    if(inherits(lt[[1]], "GRanges")) {
    	set_size = sapply(lt, function(x) {
	    	value_fun(union(x, GRanges()))
	    })
    } else if(inherits(lt[[1]], "IRanges")) {
    	set_size = sapply(lt, function(x) {
	    	value_fun(union(x, IRanges()))
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
# 	      b = sample(letters, 15),
# 	      c = sample(letters, 20))
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
# Make a Combination matrix for UpSet Plot
#
# == param
# -...
# -mode
# -top_n_sets
# -min_set_size
# -value_fun
make_comb_mat = function(..., mode = c("distinct", "intersect", "union"),
	top_n_sets = Inf, min_set_size = -Inf, value_fun) {

	lt = list(...)

	mode = match.arg(mode)[1]
	if(length(lt) == 1) {
		lt = lt[[1]]
		if(!is.null(dim(lt))) {
			return(make_comb_mat_from_matrix(lt, mode = mode, top_n_sets = top_n_sets, min_set_size = min_set_size))
		}
	}

	if(missing(value_fun)) {
		if(inherits(lt[[1]], "GRanges")) {
			value_fun = function(x) sum(as.numeric(end(x) - start(x) + 1))
		} else if(inherits(lt[[1]], "IRanges")) {
			value_fun = function(x) sum(as.numeric(end(x) - start(x) + 1))
		} else {
			value_fun = length
		}
	}
	make_comb_mat_from_list(lt, value_fun, mode = mode, top_n_sets = top_n_sets, min_set_size = min_set_size)
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
# 	      b = sample(letters, 15),
# 	      c = sample(letters, 20))
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
# 	      b = sample(letters, 15),
# 	      c = sample(letters, 20))
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
# 	      b = sample(letters, 15),
# 	      c = sample(letters, 20))
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
# 	      b = sample(letters, 15),
# 	      c = sample(letters, 20))
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
# 	      b = sample(letters, 15),
# 	      c = sample(letters, 20))
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

extract_comb = function(m, comb_name) {
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
# 	      b = sample(letters, 15),
# 	      c = sample(letters, 20))
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
# -j Indices on columns
# -drop It is always reset to ``FALSE`` internally.
#
"[.comb_mat" = function(x, i, j, drop = FALSE) {
	set_size = attr(x, "set_size")
	comb_size = attr(x, "comb_size")
	set_on_rows = attr(x, "set_on_rows")
	mode = attr(x, "mode")

	class(x) = "matrix"

	if(set_on_rows) {
		if(nargs() == 2) {
			return(x[i])
		}
		if(missing(i)) {
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
			return(x[i])
		}
		if(missing(i)) {
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
}

# == title
# Make the UpSet plot
#
UpSet = function(m, set_order = order(set_size(m), decreasing = TRUE), 
	comb_order = order(comb_size(m), decreasing = TRUE), ...) {

	set_on_rows = attr(m, "set_on_rows")
	mode = attr(m, "mode")

	m2 = m
	
	class(m2) = "matrix"

	if(set_on_rows) {
		layer_fun = function(j, i, x, y, w, h, fill) {
			nr = round(1/as.numeric(h[1]))
			nc = round(1/as.numeric(w[1]))
			subm = matrix(pindex(m2, i, j), nrow = nr, byrow = FALSE)
			for(k in seq_len(nr)) {
				if(k %% 2) {
					grid.rect(y = k/nr, height = 1/nr, just = "top", gp = gpar(fill = "#F0F0F0", col = NA))
				}
			}
			grid.points(x, y, size = unit(3, "mm"), pch = 16, gp = gpar(col = ifelse(pindex(m2, i, j), "black", "#CCCCCC")))
			for(k in seq_len(nc)) {
		        if(sum(subm[, k]) >= 2) {
		            i_min = min(which(subm[, k] > 0))
		            i_max = max(which(subm[, k] > 0))
		            grid.lines(c(k - 0.5, k - 0.5)/nc, (nr - c(i_min, i_max) + 0.5)/nr, gp = gpar(col = "black", lwd = 2))
		        }
		    }
		}
		ht = Heatmap(m2, cluster_rows = FALSE, cluster_columns = FALSE, rect_gp = gpar(type = "none"),
			layer_fun = layer_fun, show_heatmap_legend = FALSE,
			top_annotation = HeatmapAnnotation("Combination size" = anno_barplot(comb_size(m), 
					border = FALSE, gp = gpar(fill = "black"), height = unit(2, "cm")), 
				annotation_name_side = "left", annotation_name_rot = 0),
			right_annotation = rowAnnotation("Set size" = anno_barplot(set_size(m), border = FALSE, 
					gp = gpar(fill = "black"), width = unit(3, "cm"))),
			row_names_side = "left",
			row_order = set_order, column_order = comb_order, ...)
	} else {
		layer_fun = function(j, i, x, y, w, h, fill) {
			nr = round(1/as.numeric(h[1]))
			nc = round(1/as.numeric(w[1]))
			subm = matrix(pindex(m2, i, j), nrow = nr, byrow = FALSE)
			for(k in seq_len(nc)) {
				if(k %% 2) {
					grid.rect(x = k/nc, width = 1/nc, just = "right", gp = gpar(fill = "#F0F0F0", col = NA))
				}
			}
			grid.points(x, y, size = unit(3, "mm"), pch = 16, gp = gpar(col = ifelse(pindex(m2, i, j), "black", "#CCCCCC")))
			for(k in seq_len(nr)) {
		        if(sum(subm[k, ]) >= 2) {
		            i_min = min(which(subm[k, ] > 0))
		            i_max = max(which(subm[k, ] > 0))
		            grid.lines((c(i_min, i_max) - 0.5)/nc, (nr - c(k ,k) + 0.5)/nr, gp = gpar(col = "black", lwd = 2))
		        }
		    }
		}
		ht = Heatmap(m2, cluster_rows = FALSE, cluster_columns = FALSE, rect_gp = gpar(type = "none"),
			layer_fun = layer_fun, show_heatmap_legend = FALSE,
			right_annotation = rowAnnotation("Combination size" = anno_barplot(comb_size(m), 
				border = FALSE, gp = gpar(fill = "black"), width = unit(2, "cm"))),
			top_annotation = HeatmapAnnotation("Set size" = anno_barplot(set_size(m), border = FALSE, gp = gpar(fill = "black"),
				height = unit(3, "cm")),
				annotation_name_side = "left", annotation_name_rot = 0),
			row_order = comb_order, column_order = set_order, ...)
	}
	ht
}


binaryToInt = function(x) {
	sum(x * 2^(rev(seq_along(x)) - 1))
}
