
# == title
# Get Row Order from a Heatmap List
#
# == param
# -object A `HeatmapList-class` object.
# -name Name of a specific heatmap.
#
# == value
# The format of the returned object depends on whether rows/columns of the heatmaps are split.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht_list = Heatmap(mat) + Heatmap(mat)
# ht_list = draw(ht_list)
# row_order(ht_list)
# ht_list = Heatmap(mat, row_km = 2) + Heatmap(mat)
# ht_list = draw(ht_list)
# row_order(ht_list)
# ht_list = Heatmap(mat, row_km = 2) \%v\% Heatmap(mat)
# ht_list = draw(ht_list)
# row_order(ht_list)
setMethod(f = "row_order",
	signature = "HeatmapList",
	definition = function(object, name = NULL) {

	if(!object@layout$initialized) {
		warning_wrap("The heatmap list has not been initialized. You might have different results if you repeatedly execute this function, e.g. when row_km/column_km was set. It is more suggested to do as `ht_list = draw(ht_list); row_order(ht_list)`.")
	}

	object = make_layout(object)

	if(!is.null(name)) {
		return(row_order(object@ht_list[[ name[1] ]]))
	}

	n = length(object@ht_list)
	ht_index = which(sapply(seq_along(object@ht_list), function(i) inherits(object@ht_list[[i]], "Heatmap")))
	if(length(ht_index) == 0) {
		return(NULL)
	}

	if(object@direction == "horizontal") {
		lt = object@ht_list[[ ht_index[1] ]]@row_order_list
		if(length(lt) == 1) {
			return(lt[[1]])
		} else {
			return(lt)
		}
	} else {
		lt_rd = list()
		for(i in ht_index) {
	        lt = object@ht_list[[i]]@row_order_list
	        lt_rd = c(lt_rd, list(lt))
	    }
	    names(lt_rd) = names(object@ht_list)[ht_index]
	    proper_format_lt(lt_rd)
	}
})

# == title
# Get Row Order from a Heatmap
#
# == param
# -object A `Heatmap-class` object.
#
# == value
# The format of the returned object depends on whether rows/columns of the heatmaps are split.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht = Heatmap(mat)
# ht = draw(ht)
# row_order(ht)
# ht = Heatmap(mat, row_km = 2)
# ht = draw(ht)
# row_order(ht)
#
setMethod(f = "row_order",
	signature = "Heatmap",
	definition = function(object) {

	if(!object@layout$initialized) {
		warning_wrap("The heatmap has not been initialized. You might have different results if you repeatedly execute this function, e.g. when row_km/column_km was set. It is more suggested to do as `ht = draw(ht); row_order(ht)`.")
	}


	object = prepare(object)

	lt = object@row_order_list
	if(length(lt) == 1) {
		return(lt[[1]])
	} else {
		return(lt)
	}
})

# == title
# Get Column Order from a Heatmap List
#
# == param
# -object A `HeatmapList-class` object.
# -name Name of a specific heatmap.
#
# == value
# The format of the returned object depends on whether rows/columns of the heatmaps are split.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht_list = Heatmap(mat) + Heatmap(mat)
# ht_list = draw(ht_list)
# column_order(ht_list)
# ht_list = Heatmap(mat, column_km = 2) + Heatmap(mat, column_km = 2)
# ht_list = draw(ht_list)
# column_order(ht_list)
# ht_list = Heatmap(mat) \%v\% Heatmap(mat)
# ht_list = draw(ht_list)
# column_order(ht_list)
# ht_list = Heatmap(mat, column_km = 2) \%v\% Heatmap(mat)
# ht_list = draw(ht_list)
# column_order(ht_list)
setMethod(f = "column_order",
	signature = "HeatmapList",
	definition = function(object, name = NULL) {

	if(!object@layout$initialized) {
		warning_wrap("The heatmap list has not been initialized. You might have different results if you repeatedly execute this function, e.g. when row_km/column_km was set. It is more suggested to do as `ht_list = draw(ht_list); column_order(ht_list)`.")
	}

	object = make_layout(object)

	if(!is.null(name)) {
		return(column_order(object@ht_list[[ name[1] ]]))
	}

	n = length(object@ht_list)
	ht_index = which(sapply(seq_along(object@ht_list), function(i) inherits(object@ht_list[[i]], "Heatmap")))
	if(length(ht_index) == 0) {
		return(NULL)
	}

	if(object@direction == "vertical") {
		lt = object@ht_list[[ ht_index[1] ]]@column_order_list
		if(length(lt) == 1) {
			return(lt[[1]])
		} else {
			return(lt)
		}
	} else {
		lt_rd = list()
		for(i in ht_index) {
	        lt = object@ht_list[[i]]@column_order_list
	        lt_rd = c(lt_rd, list(lt))
	    }
	    names(lt_rd) = names(object@ht_list)[ht_index]
	    proper_format_lt(lt_rd)
	}
})

# == title
# Get Column Order from a Aeatmap List
#
# == param
# -object A `Heatmap-class` object.
#
# == value
# The format of the returned object depends on whether rows/columns of the heatmaps are split.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht = Heatmap(mat)
# ht = draw(ht)
# column_order(ht)
# ht = Heatmap(mat, column_km = 2)
# ht = draw(ht)
# column_order(ht)
setMethod(f = "column_order",
	signature = "Heatmap",
	definition = function(object) {

	if(!object@layout$initialized) {
		warning_wrap("The heatmap has not been initialized. You might have different results if you repeatedly execute this function, e.g. when row_km/column_km was set. It is more suggested to do as `ht = draw(ht); column_order(ht)`.")
	}

	object = prepare(object)

	lt = object@column_order_list
	if(length(lt) == 1) {
		return(lt[[1]])
	} else {
		return(lt)
	}
	
})

# == title
# Get Row Dendrograms from a Heatmap List
#
# == param
# -object A `HeatmapList-class` object.
# -name Name of a specific heatmap.
# -on_slice If the value is TRUE, it returns the dendrogram on the slice level.
# 
# == value
# The format of the returned object depends on whether rows/columns of the heatmaps are split.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht_list = Heatmap(mat) + Heatmap(mat)
# ht_list = draw(ht_list)
# row_dend(ht_list)
# ht_list = Heatmap(mat, row_km = 2) + Heatmap(mat)
# ht_list = draw(ht_list)
# row_dend(ht_list)
# row_dend(ht_list, on_slice = TRUE)
# ht_list = Heatmap(mat, row_km = 2) \%v\% Heatmap(mat)
# ht_list = draw(ht_list)
# row_dend(ht_list)
setMethod(f = "row_dend",
	signature = "HeatmapList",
	definition = function(object, name = NULL, on_slice = FALSE) {

	if(!object@layout$initialized) {
		warning_wrap("The heatmap list has not been initialized. You might have different results if you repeatedly execute this function, e.g. when row_km/column_km was set. It is more suggested to do as `ht_list = draw(ht_list); row_dend(ht_list)`.")
	}

	object = make_layout(object)

	if(!is.null(name)) {
		return(row_dend(object@ht_list[[ name[1] ]], on_slice = on_slice))
	}

	n = length(object@ht_list)
	ht_index = which(sapply(seq_along(object@ht_list), function(i) inherits(object@ht_list[[i]], "Heatmap")))
	if(length(ht_index) == 0) {
		return(NULL)
	}

	if(object@direction == "horizontal") {
		if(on_slice) {
			return(object@ht_list[[ ht_index[1] ]]@row_dend_slice)
		}
		lt = object@ht_list[[ ht_index[1] ]]@row_dend_list
		if(length(lt) == 1) {
			return(lt[[1]])
		} else {
			return(lt)
		}
	} else {
		lt_rd = list()
		for(i in ht_index) {
			if(on_slice) {
				lt = object@ht_list[[i]]@row_dend_slice
		        lt_rd = c(lt_rd, list(lt))
			} else {
		        lt = object@ht_list[[i]]@row_dend_list
		        lt_rd = c(lt_rd, list(lt))
		    }
	    }
	    names(lt_rd) = names(object@ht_list)[ht_index]
	    proper_format_lt(lt_rd)
	}
})


# == title
# Get Row Dendrograms from a Heatmap
#
# == param
# -object A `Heatmap-class` object.
# -on_slice If the value is TRUE, it returns the dendrogram on the slice level.
# 
# == value
# The format of the returned object depends on whether rows/columns of the heatmaps are split.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht = Heatmap(mat)
# ht = draw(ht)
# row_dend(ht)
# ht = Heatmap(mat, row_km = 2)
# ht = draw(ht)
# row_dend(ht)
#
setMethod(f = "row_dend",
	signature = "Heatmap",
	definition = function(object, on_slice = FALSE) {

	if(!object@layout$initialized) {
		warning_wrap("The heatmap has not been initialized. You might have different results if you repeatedly execute this function, e.g. when row_km/column_km was set. It is more suggested to do as `ht = draw(ht); row_dend(ht)`.")
	}

	object = prepare(object)	

	if(on_slice) {
		return(object@row_dend_slice)
	}

	lt = object@row_dend_list
	if(length(lt) == 1) {
		return(lt[[1]])
	} else {
		return(lt)
	}
})

# == title
# Get Column Dendrograms from a hHeatmap List
#
# == param
# -object A `HeatmapList-class` object.
# -name Name of a specific heatmap.
# -on_slice If the value is TRUE, it returns the dendrogram on the slice level.
# 
# == value
# The format of the returned object depends on whether rows/columns of the heatmaps are split.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht_list = Heatmap(mat) + Heatmap(mat)
# ht_list = draw(ht_list)
# column_dend(ht_list)
# ht_list = Heatmap(mat, column_km = 2) + Heatmap(mat, column_km = 2)
# ht_list = draw(ht_list)
# column_dend(ht_list)
# column_dend(ht_list, on_slice = TRUE)
# ht_list = Heatmap(mat) \%v\% Heatmap(mat)
# ht_list = draw(ht_list)
# column_dend(ht_list)
# ht_list = Heatmap(mat, column_km = 2) \%v\% Heatmap(mat)
# ht_list = draw(ht_list)
# column_dend(ht_list)
setMethod(f = "column_dend",
	signature = "HeatmapList",
	definition = function(object, name = NULL, on_slice = FALSE) {

	if(!object@layout$initialized) {
		warning_wrap("The heatmap list has not been initialized. You might have different results if you repeatedly execute this function, e.g. when row_km/column_km was set. It is more suggested to do as `ht_list = draw(ht_list); column_dend(ht_list)`.")
	}

	object = make_layout(object)

	if(!is.null(name)) {
		return(column_dend(object@ht_list[[ name[1] ]], on_slice = on_slice))
	}

	n = length(object@ht_list)
	ht_index = which(sapply(seq_along(object@ht_list), function(i) inherits(object@ht_list[[i]], "Heatmap")))
	if(length(ht_index) == 0) {
		return(NULL)
	}

	if(object@direction == "vertical") {
		if(on_slice) {
			return(object@ht_list[[ ht_index[1] ]]@column_dend_slice)
		}

		lt = object@ht_list[[ ht_index[1] ]]@column_dend_list
		if(length(lt) == 1) {
			return(lt[[1]])
		} else {
			return(lt)
		}
	} else {
		lt_rd = list()
		for(i in ht_index) {
			if(on_slice) {
				lt = object@ht_list[[i]]@column_dend_slice
		        lt_rd = c(lt_rd, list(lt))
			} else {
		        lt = object@ht_list[[i]]@column_dend_list
		        lt_rd = c(lt_rd, list(lt))
		    }
	    }
	    names(lt_rd) = names(object@ht_list)[ht_index]
	    proper_format_lt(lt_rd)
	}
})


# == title
# Get Column Dendrograms from a Heatmap
#
# == param
# -object A `Heatmap-class` object.
# -on_slice If the value is TRUE, it returns the dendrogram on the slice level.
# 
# == value
# The format of the returned object depends on whether rows/columns of the heatmaps are split.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht = Heatmap(mat)
# ht = draw(ht)
# column_dend(ht)
# ht = Heatmap(mat, column_km = 2)
# ht = draw(ht)
# column_dend(ht)
#
setMethod(f = "column_dend",
	signature = "Heatmap",
	definition = function(object, on_slice = FALSE) {

	if(!object@layout$initialized) {
		warning_wrap("The heatmap has not been initialized. You might have different results if you repeatedly execute this function, e.g. when row_km/column_km was set. It is more suggested to do as `ht = draw(ht); column_dend(ht)`.")
	}

	object = prepare(object)

	if(on_slice) {
		return(object@column_dend_slice)
	}

	lt = object@column_dend_list
	if(length(lt) == 1) {
		return(lt[[1]])
	} else {
		return(lt)
	}
})



proper_format_lt = function(lt) {	
	n_ht = length(lt)
	
	if(n_ht == 1) {
		if(length(lt[[1]]) == 1) {
			return(lt[[1]][[1]])
		} else {
			return(lt[[1]])
		}
	} else {
		l_empty = sapply(lt, function(x) length(x) == 0)
		lt = lt[!l_empty]
		if(length(lt) == 0) {
			return(NULL)
		}
	
		has_splitting = any(sapply(lt, function(x) length(x) > 1))
		if(has_splitting) {
			return(lt)
		} else {
			return(lapply(lt, function(x) x[[1]]))
		}
	}
}
