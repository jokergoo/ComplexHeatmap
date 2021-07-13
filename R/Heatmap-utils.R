
# == title
# Subset a Heatmap
#
# == param
# -x A `Heatmap-class` object.
# -i Row indices.
# -j Column indices.
#
# == details
# This functionality is quite experimental. It should be applied before the layout is initialized.
#
# == example
# m = matrix(rnorm(100), nrow = 10)
# rownames(m) = letters[1:10]
# colnames(m) = LETTERS[1:10]
# ht = Heatmap(m)
# ht[1:5, ]
# ht[1:5]
# ht[, 1:5]
# ht[1:5, 1:5]
"[.Heatmap" = function(x, i, j) {

    if(x@layout$initialized) {
        stop_wrap("Subsetting is only allowed on a heatmap where layout is not initialized.")
    }
    
    if(nargs() == 2) {
        subset_heatmap_by_row(x, i)
    } else {
        if(missing(i)) {
            subset_heatmap_by_column(x, j)
        } else if(missing(j)) {
            subset_heatmap_by_row(x, i)
        } else {
            x = subset_heatmap_by_row(x, i)
            subset_heatmap_by_column(x, j)
        }
    }
}


subset_heatmap_by_row = function(ht, ind) {
    if(is.logical(ind)) ind = which(ind)
    if(is.character(ind)) stop_wrap("Indices can only be numeric or logical.")
    ht@row_order = order(intersect(ht@row_order, ind))
    if(!is.null(ht@row_dend_param$obj)) {
        stop_wrap("row dend is specified as a clustering object, cannot do subsetting.")
    }
    ht@matrix = ht@matrix[ind, , drop = FALSE]
    if(!is.null(ht@row_names_param$labels)) {
        ht@row_names_param$labels = ht@row_names_param$labels[ind]
        ht@row_names_param$anno = ht@row_names_param$anno[ind]
    }
    ht@row_names_param$gp = subset_gp(ht@row_names_param$gp, ind)
    if(!is.null(ht@matrix_param$row_split)) {
        ht@matrix_param$row_split = ht@matrix_param$row_split[ind, , drop = FALSE]
    }
    if(length(ht@left_annotation)) {
        ht@left_annotation = ht@left_annotation[ind]
    }
    if(length(ht@right_annotation)) {
        ht@right_annotation = ht@right_annotation[ind]
    }
    return(ht)
}

subset_heatmap_by_column = function(ht, ind) {
    if(is.logical(ind)) ind = which(ind)
    if(is.character(ind)) stop_wrap("Indices can only be numeric or logical.")
    ht@column_order = order(intersect(ht@column_order, ind))
    if(!is.null(ht@column_dend_param$obj)) {
        stop_wrap("column dend is specified as a clustering object, cannot do subsetting.")
    }
    ht@matrix = ht@matrix[, ind, drop = FALSE]
    if(!is.null(ht@column_names_param$labels)) {
        ht@column_names_param$labels = ht@column_names_param$labels[ind]
        ht@column_names_param$anno = ht@column_names_param$anno[ind]
    }
    ht@column_names_param$gp = subset_gp(ht@column_names_param$gp, ind)
    if(!is.null(ht@matrix_param$column_split)) {
        ht@matrix_param$column_split = ht@matrix_param$column_split[, ind, drop = FALSE]
    }
    if(length(ht@top_annotation)) {
        ht@top_annotation = ht@top_annotation[ind]
    }
    if(length(ht@bottom_annotation)) {
        ht@bottom_annotation = ht@bottom_annotation[ind]
    }
    return(ht)
}

# == title
# Dimension of the Heatmap
#
# == param
# -x A `Heatmap-class` object.
#
dim.Heatmap = function(x) {
    dim(x@matrix)
}

# == title
# Number of Rows in the Heatmap
#
# == param
# -x A `Heatmap-class` object.
#
nrow.Heatmap = function(x) {
    nrow(x@matrix)
}

# == title
# Number of Columns in the Heatmap
#
# == param
# -x A `Heatmap-class` object.
#
ncol.Heatmap = function(x) {
    ncol(x@matrix)
}

# == title
# Print the Summary of a Heatmap
#
# == param
# -object A `Heatmap-class` object.
# -... Other arguments.
#
summary.Heatmap = function(object, ...) {
    qqcat("a matrix with @{nrow(object@matrix)} rows and @{ncol(object@matrix)} columns\n")
    qqcat("name: @{object@name}\n")
    qqcat("color mapping is @{object@matrix_color_mapping@type}\n")
    
    if(length(object@column_title)) {
        qqcat("has column title\n")
    } else {
        qqcat("has no column title\n")
    }
    if(length(object@row_title)) {
        qqcat("has row title\n")
    } else {
        qqcat("has no row title\n")
    }

    if(length(object@column_names_param$labels)) {
        qqcat("has column names\n")
    } else {
        qqcat("has no column name\n")
    }
    if(length(object@row_names_param$labels)) {
        qqcat("has row names\n")
    } else {
        qqcat("has no row name\n")
    }

    if(!is.null(object@column_dend_param$obj)) {
        qqcat("column clustering is provided as a clustering object\n")
    } else {
        if(object@column_dend_param$cluster) {
            if(!is.null(object@column_dend_param$fun)) {
                qqcat("column clustering is applied with user-defined function\n")
            } else if(is.function(object@column_dend_param$distance)) {
                qqcat("column clustering is applied with '@{object@column_dend_param$method}' method and user-defined distance function\n")
            } else {
                qqcat("column clustering is applied with '@{object@column_dend_param$method}' method and '@{object@column_dend_param$distance}' distance\n")
            }
        } else {
            qqcat("no column clustering\n")
        }
    }
    if(object@matrix_param$column_km > 1) {
        qqcat("columns are split by k-means with @{object@matrix_param$column_km} groups\n")
    }
    if(!is.null(object@matrix_param$column_split)) {
        qqcat("columns are split by a categorical data frame\n")
    }
    if(!is.null(object@row_dend_param$obj)) {
        qqcat("row clustering is provided as a clustering object\n")
    } else {
        if(object@row_dend_param$cluster) {
            if(!is.null(object@row_dend_param$fun)) {
                qqcat("row clustering is applied with user-defined function\n")
            } else if(is.function(object@row_dend_param$distance)) {
                qqcat("row clustering is applied with '@{object@row_dend_param$method}' method and user-defined distance function\n")
            } else {
                qqcat("row clustering is applied with '@{object@row_dend_param$method}' method and '@{object@row_dend_param$distance}' distance\n")
            }
        } else {
            qqcat("no row clustering\n")
        }
    }
    if(object@matrix_param$row_km > 1) {
        qqcat("rows are split by k-means with @{object@matrix_param$row_km} groups\n")
    }
    if(!is.null(object@matrix_param$row_split)) {
        qqcat("rows are split by a categorical data frame\n")
    }

    if(length(object@top_annotation)) {
        qqcat("has @{length(object@top_annotation)} top annotationa:\n")
        qqcat("=======================================\n")
        show(object@top_annotation)
        qqcat("=======================================\n")
    } else {
        qqcat("has no top annotation\n")
    }
    if(length(object@bottom_annotation)) {
        qqcat("has @{length(object@bottom_annotation)} bottom annotation:\n")
        qqcat("=======================================\n")
        show(object@bottom_annotation)
        qqcat("=======================================\n")
    } else {
        qqcat("has no bottom annotation\n")
    }
    if(length(object@left_annotation)) {
        qqcat("has @{length(object@left_annotation)} left annotationa:\n")
        qqcat("=======================================\n")
        show(object@left_annotation)
        qqcat("=======================================\n")
    } else {
        qqcat("has no left annotation\n")
    }
    if(length(object@right_annotation)) {
        qqcat("has @{length(object@right_annotation)} right annotationa:\n")
        qqcat("=======================================\n")
        show(object@right_annotation)
        qqcat("=======================================\n")
    } else {
        qqcat("has no right annotation\n")
    }
}


validate_anno_names_with_matrix = function(m, ha, which) {
    if(!ht_opt$validate_names) {
        return(NULL)
    }
    if(which == "column") {
        if(is.null(colnames(m))) {
            return(NULL)
        }
        cn = colnames(m)
        for(i in seq_along(ha@anno_list)) {
            if(setequal(cn, names(ha@anno_list[[i]]))) {
                if(!identical(cn, names(ha@anno_list[[i]]))) {
                    warning_wrap(qq("Values in column annotation '@{ha@anno_list[[i]]@name}' have a different order of names from the matrix column names. It may lead to wrong conclusion of your data. Please double check."))
                }
            }
        }
    }
    if(which == "row") {
        if(is.null(rownames(m))) {
            return(NULL)
        }
        rn = rownames(m)
        for(i in seq_along(ha@anno_list)) {
            if(setequal(rn, names(ha@anno_list[[i]]))) {
                if(!identical(rn, names(ha@anno_list[[i]]))) {
                    warning_wrap(qq("Values in row annotation '@{ha@anno_list[[i]]@name}' have a different order of names from the matrix row names. It may lead to wrong conclusion of your data. Please double check."))
                }
            }
        }
    }
}