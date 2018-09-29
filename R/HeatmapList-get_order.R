
# == title
# Get row order from a heatmap list
#
# == param
# -object a `HeatmapList-class` object
#
# == value
# A list contains row orders which correspond to the original matrix
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht_list = Heatmap(mat) + Heatmap(mat)
# row_order(ht_list)
# ht_list = Heatmap(mat, row_km = 2) + Heatmap(mat)
# row_order(ht_list)
# ht_list = Heatmap(mat, row_km = 2) %v% Heatmap(mat)
# row_order(ht_list)
setMethod(f = "row_order",
	signature = "HeatmapList",
	definition = function(object) {

	object = make_layout(object)

	n = length(object@ht_list)
	ht_index = sapply(seq_along(object@ht_list), function(i) inherits(object@ht_list[[i]], "Heatmap"))
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
	    return(lt_rd)
	}
})

# == title
# Get row order from a heatmap
#
# == param
# -object a `Heatmap-class` object
#
# == value
# A list contains row orders which correspond to the original matrix
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht = Heatmap(mat)
# row_order(ht)
# ht = Heatmap(mat, row_km = 2)
# row_order(ht)
#
setMethod(f = "row_order",
	signature = "Heatmap",
	definition = function(object) {

	object = prepare(object)

	lt = object@row_order_list
	if(length(lt) == 1) {
		return(lt[[1]])
	} else {
		return(lt)
	}
})

# == title
# Get column order from a heatmap list
#
# == param
# -object a `HeatmapList-class` object
#
# == value
# A list contains column orders which correspond every matrix
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht_list = Heatmap(mat) + Heatmap(mat)
# column_order(ht_list)
# ht_list = Heatmap(mat, column_km = 2) + Heatmap(mat, column_km = 2)
# column_order(ht_list)
# ht_list = Heatmap(mat) %v% Heatmap(mat)
# column_order(ht_list)
# ht_list = Heatmap(mat, column_km = 2) %v% Heatmap(mat)
# column_order(ht_list)
setMethod(f = "column_order",
	signature = "HeatmapList",
	definition = function(object) {

	object = make_layout(object)

	n = length(object@ht_list)
	ht_index = sapply(seq_along(object@ht_list), function(i) inherits(object@ht_list[[i]], "Heatmap"))
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
	    if(length(lt_rd) == 1) lt_rd = lt_rd[[1]]
	    return(lt_rd)
	}
})

# == title
# Get column order from a heatmap list
#
# == param
# -object a `Heatmap-class` object
#
# == value
# A vector containing column orders
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht = Heatmap(mat)
# column_order(ht)
# ht = Heatmap(mat, column_km = 2)
# column_order(ht)
setMethod(f = "column_order",
	signature = "Heatmap",
	definition = function(object) {

	object = prepare(object)

	lt = object@column_order_list
	if(length(lt) == 1) {
		return(lt[[1]])
	} else {
		return(lt)
	}
	
})

# == title
# Get row dendrograms from a heatmap list
#
# == param
# -object a `HeatmapList-class` object
# 
# == value
# A list of dendrograms for which each dendrogram corresponds to a row slice
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht_list = Heatmap(mat) + Heatmap(mat)
# row_dend(ht_list)
# ht_list = Heatmap(mat, row_km = 2) + Heatmap(mat)
# row_dend(ht_list)
# ht_list = Heatmap(mat, row_km = 2) %v% Heatmap(mat)
# row_dend(ht_list)
setMethod(f = "row_dend",
	signature = "HeatmapList",
	definition = function(object) {

	object = make_layout(object)

	n = length(object@ht_list)
	ht_index = sapply(seq_along(object@ht_list), function(i) inherits(object@ht_list[[i]], "Heatmap"))
	if(length(ht_index) == 0) {
		return(NULL)
	}

	if(object@direction == "horizontal") {
		lt = object@ht_list[[ ht_index[1] ]]@row_dend_list
		if(length(lt) == 1) {
			return(lt[[1]])
		} else {
			return(lt)
		}
	} else {
		lt_rd = list()
		for(i in ht_index) {
	        lt = object@ht_list[[i]]@row_dend_list
	        lt_rd = c(lt_rd, list(lt))
	    }
	    names(lt_rd) = names(object@ht_list)[ht_index]
	    return(lt_rd)
	}
})


# == title
# Get row dendrograms from a heatmap
#
# == param
# -object a `Heatmap-class` object
# 
# == value
# A list of dendrograms for which each dendrogram corresponds to a row slice
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht = Heatmap(mat)
# row_dend(ht)
# ht = Heatmap(mat, row_km = 2)
# row_dend(ht)
#
setMethod(f = "row_dend",
	signature = "Heatmap",
	definition = function(object) {

	object = prepare(object)

	lt = object@row_dend_list
	if(length(lt) == 1) {
		return(lt[[1]])
	} else {
		return(lt)
	}
})

# == title
# Get column dendrograms from a heatmap list
#
# == param
# -object a `HeatmapList-class` object
# 
# == value
# A list of dendrograms for which dendrogram corresponds to each matrix
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht_list = Heatmap(mat) + Heatmap(mat)
# column_dend(ht_list)
# ht_list = Heatmap(mat, column_km = 2) + Heatmap(mat, column_km = 2)
# column_dend(ht_list)
# ht_list = Heatmap(mat) %v% Heatmap(mat)
# column_dend(ht_list)
# ht_list = Heatmap(mat, column_km = 2) %v% Heatmap(mat)
# column_dend(ht_list)
setMethod(f = "column_dend",
	signature = "HeatmapList",
	definition = function(object) {

	object = make_layout(object)

	n = length(object@ht_list)
	ht_index = sapply(seq_along(object@ht_list), function(i) inherits(object@ht_list[[i]], "Heatmap"))
	if(length(ht_index) == 0) {
		return(NULL)
	}

	if(object@direction == "vertical") {
		lt = object@ht_list[[ ht_index[1] ]]@column_dend_list
		if(length(lt) == 1) {
			return(lt[[1]])
		} else {
			return(lt)
		}
	} else {
		lt_rd = list()
		for(i in ht_index) {
	        lt = object@ht_list[[i]]@column_dend_list
	        lt_rd = c(lt_rd, list(lt))
	    }
	    names(lt_rd) = names(object@ht_list)[ht_index]
	    return(lt_rd)
	}
})


# == title
# Get column dendrograms from a heatmap
#
# == param
# -object a `Heatmap-class` object
# 
# == value
# A dendrogram object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# mat = matrix(rnorm(100), 10)
# ht = Heatmap(mat)
# column_dend(ht)
# ht = Heatmap(mat, column_km = 2)
# column_dend(ht)
#
setMethod(f = "column_dend",
	signature = "Heatmap",
	definition = function(object) {

	object = prepare(object)
	
	lt = object@column_dend_list
	if(length(lt) == 1) {
		return(lt[[1]])
	} else {
		return(lt)
	}
})

