
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
# ht = Heatmap(mat, km = 2) + Heatmap(mat)
# row_order(ht_list)
#
setMethod(f = "row_order",
	signature = "HeatmapList",
	definition = function(object) {

	object = make_layout(object)

	n = length(object@ht_list)
	for(i in seq_len(n)) {
        if(inherits(object@ht_list[[i]], "Heatmap")) {
        	lt = object@ht_list[[i]]@row_order_list
        	return(lt)
        }
    }

    return(NULL)
	
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
# ht = Heatmap(mat, km = 2)
# row_order(ht)
#
setMethod(f = "row_order",
	signature = "Heatmap",
	definition = function(object) {

	object = prepare(object)

	lt = object@row_order_list
	return(lt)
	
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
# ht = Heatmap(mat, km = 2) + Heatmap(mat)
# column_order(ht_list)
#
setMethod(f = "column_order",
	signature = "HeatmapList",
	definition = function(object) {

	object = make_layout(object)

    n = length(object@ht_list)
    order_list = vector("list", n)
    names(order_list) = sapply(object@ht_list, function(ht) ht@name)

    for(i in seq_len(n)) {
        if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
        } else {
            order_list[[i]] = object@ht_list[[i]]@column_order
        }
    }

    return(order_list)

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
# ht = Heatmap(mat, km = 2)
# column_order(ht)
#
setMethod(f = "column_order",
	signature = "Heatmap",
	definition = function(object) {

	object = prepare(object)

	return(object@column_order)
	
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
# ht_list = Heatmap(mat, km = 2) + Heatmap(mat)
# row_dend(ht_list)
#
setMethod(f = "row_dend",
	signature = "HeatmapList",
	definition = function(object) {

	object = make_layout(object)

	n = length(object@ht_list)
    for(i in seq_len(n)) {
        if(inherits(object@ht_list[[i]], "Heatmap")) {
        	lt = object@ht_list[[i]]@row_dend_list
        	return(lt)
        }
    }

    return(NULL)
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
# ht = Heatmap(mat, km = 2)
# row_dend(ht)
#
setMethod(f = "row_dend",
	signature = "Heatmap",
	definition = function(object) {

	object = prepare(object)

	lt = object@row_dend_list
	return(lt)
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
# ht_list = Heatmap(mat, km = 2) + Heatmap(mat)
# column_dend(ht_list)
#
setMethod(f = "column_dend",
	signature = "HeatmapList",
	definition = function(object) {

	object = make_layout(object)

	# return a list of orders
    n = length(object@ht_list)
    dend_list = vector("list", n)
    names(dend_list) = sapply(object@ht_list, function(ht) ht@name)

    for(i in seq_len(n)) {
        if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
        } else {
            dend_list[[i]] = object@ht_list[[i]]@column_dend
        }
    }

    return(dend_list)
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
# ht = Heatmap(mat, km = 2)
# column_dend(ht)
#
setMethod(f = "column_dend",
	signature = "Heatmap",
	definition = function(object) {

	object = prepare(object)

	return(object@column_dend)
})

