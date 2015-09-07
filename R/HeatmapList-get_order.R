
# == title
# Get row order from a heatmap list
#
# == param
# -object a `HeatmapList-class` object
#
# == value
# A list contains row orders which correspond to the original matrix
#
setMethod(f = "row_order",
	signature = "HeatmapList",
	definition = function(object) {

	object = make_layout(object)

	n = length(object@ht_list)
	for(i in seq_len(n)) {
        if(inherits(object@ht_list[[i]], "Heatmap")) {
        	lt = object@ht_list[[i]]@row_order_list
        	if(length(object@ht_list[[i]]@row_title) > 0) {
        		names(lt) = object@ht_list[[i]]@row_title
        	}
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
setMethod(f = "row_order",
	signature = "Heatmap",
	definition = function(object) {

	object = prepare(object)

	lt = object@row_order_list
	if(length(object@row_title) > 0) {
		names(lt) = object@row_title
	}
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
setMethod(f = "row_dend",
	signature = "HeatmapList",
	definition = function(object) {

	object = make_layout(object)

	n = length(object@ht_list)
    for(i in seq_len(n)) {
        if(inherits(object@ht_list[[i]], "Heatmap")) {
        	lt = object@ht_list[[i]]@row_dend_list
        	if(length(object@ht_list[[i]]@row_title) > 0) {
        		names(lt) = object@ht_list[[i]]@row_title
        	}
        	return(lt)
        }
    }

    return(NULL)
})

# == title
# Get row dendrograms from a heatmap
#
# == param
# -object a `HeatmapList` object
# 
# == value
# A list of dendrograms for which each dendrogram corresponds to a row slice
#
setMethod(f = "row_dend",
	signature = "Heatmap",
	definition = function(object) {

	object = prepare(object)

	lt = object@row_dend_list
	if(length(object@row_title) > 0) {
		names(lt) = object@row_title
	}
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
setMethod(f = "column_dend",
	signature = "Heatmap",
	definition = function(object) {

	object = prepare(object)

	return(object@column_dend)
})

