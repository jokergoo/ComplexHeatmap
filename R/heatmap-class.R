
###############################
# class for single heatmap
#


# the layout of the heatmap is 7 x 9

# == title
# Class for a single heatmap
#
# == details
# The components for a single heamtap are placed into a 9 x 7 layout:
#
#          +------+
#          +------+
#          +------+
#          +------+
#    +-+-+-+------+-+-+-+
#    | | | |      | | | |
#    +-+-+-+------+-+-+-+
#          +------+
#          +------+
#          +------+
#          +------+
#
# From top to bottom in column 4, the regions are:
#
# - title put on column, graphics are drawn by `draw_title,Heatmap-method`.
# - column cluster, graphics are drawn by `draw_hclust,Heatmap-method`.
# - column annotation, graphics are drawn by `draw_annotation,Heatmap-method`.
# - column names, graphics are drawn by `draw_dimnames,Heatmap-method`.
# - heatmap body, graphics are drawn by `draw_heatmap_body,Heatmap-method`.
# - column names, graphics are drawn by `draw_dimnames,Heatmap-method`.
# - column annotation, graphics are drawn by `draw_annotation,Heatmap-method`.
# - column cluster, graphics are drawn by `draw_hclust,Heatmap-method`.
# - title put on column, graphics are drawn by `draw_title,Heatmap-method`.
# 
# From left to right in row 5, the regions are:
#
# - title put on row, graphics are drawn by `draw_title,Heatmap-method`.
# - row cluster, graphics are drawn by `draw_hclust,Heatmap-method`.
# - row names, graphics are drawn by `draw_dimnames,Heatmap-method`.
# - heatmap body
# - row names, graphics are drawn by `draw_dimnames,Heatmap-method`.
# - row cluster, graphics are drawn by `draw_hclust,Heatmap-method`.
# - title put on row, graphics are drawn by `draw_title,Heatmap-method`.
#
# The `Heatmap` class is not resposible for heatmap legend. The `draw,Heatmap-method` method
# will construct a `HeatmapList` class which only contains one single heatmap
# and call `draw,HeatmapList-method` to make a complete heatmap.
#
# == methods
# The `Heatmap` class provides following methods:
#
# - `initialize,Heatmap-method`: contructor method.
# - `draw,Heatmap-method`: draw a single heatmap.
# - `add_heatmap,Heatmap-method` add heatmaps to a list of heatmaps.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
Heatmap = setClass("Heatmap",
    slots = list(
        name = "character",
        matrix = "matrix",  # one or more matrix which are spliced by rows
        matrix_param = "list",
        matrix_color_mapping = "ANY",

        row_title = "character",
        row_title_param = "list",
        column_title = "character",
        column_title_param = "list",

        row_hclust_list = "list", # one or more row clusters
        row_hclust_param = "list", # parameters for row cluster
        row_order_list = "list",

        column_hclust = "ANY",
        column_hclust_param = "list", # parameters for column cluster
        column_order = "numeric",

        row_names_param = "list",
        column_names_param = "list",

        top_annotation = "ANY",
        top_annotation_param = "list",

        bottom_annotation = "ANY",
        bottom_annotation_param = "list",

        heatmap_param = "list",

        layout = "environment"
    )
)



# == title
# Constructor method of Heatmap class
#
# == param
# -.Object private object.
# -matrix matrix. Either numeric or character. If it is a simple vector, it will be
#         converted to a one-column matrix.
# -col a vector of colors if the matrix is character or a color mapping 
#      function if the matrix is numeric. Pass to `initialize,ColorMapping-method`.
# -name name of the heatmap.
# -rect_gp graphic parameters for drawing rectangles (for heatmap body).
# -row_title title on row.
# -row_title_side will the title be put on the left or right of the heatmap.
# -row_title_gp graphic parameters for drawing text.
# -column_title title on column.
# -column_title_side will the title be put on the top or bottom of the heatmap.
# -column_title_gp graphic parameters for drawing text.
# -cluster_rows whether make cluster on rows.
# -clustering_distance_rows it can be a pre-defined character which is in 
#                ("euclidean", "maximum", "manhattan", "canberra", "binary", 
#                "minkowski", "pearson", "spearman", "kendall"). It can also be a function.
#                If the function has one argument, the input argument should be a matrix and 
#                the returned value should be a `stats::dist` object. If the function has two arguments,
#                the input arguments are two vectors and the function calcualtes distance between these
#                two vectors.
# -clustering_method_rows method to make cluster, pass to `stats::hclust`.
# -row_hclust_side should the row cluster be put on the left or right of the heatmap.
# -row_hclust_width width of the row cluster, should be a `grid::unit` object.
# -show_row_hclust whether show row clusters.
# -row_hclust_gp graphics parameters for drawing lines.
# -cluster_columns whether make cluster on columns.
# -clustering_distance_columns same setting as ``clustering_distance_rows``.
# -clustering_method_columns method to make cluster, pass to `stats::hclust`.
# -column_hclust_side should the column cluster be put on the top or bottom of the heatmap.
# -column_hclust_height height of the column cluster, should be a `grid::unit` object.
# -show_column_hclust whether show column clusters.
# -column_hclust_gp graphic parameters for drawling lines.
# -row_names_side should the row names be put on the left or right of the heatmap.
# -show_row_names whether show row names.
# -row_names_gp graphic parameters for drawing text.
# -column_names_side should the column names be put on the top or bottom of the heatmap.
# -show_column_names whether show column names.
# -column_names_gp graphic parameters for drawing text.
# -top_annotation
# -top_annotation_height
# -bottom_annotation
# -bottom_annotation_height
# -km whether do k-means clustering on rows. 
# -gap gap between row-slice, should be `grid::unit` object
# -split a vector or a data frame by which the rows are splitted 
# -width the width of the single heatmap.
#
# == details
# The initialization function only applies parameter checking. Clustering
# on rows can be applied by `make_row_cluster,Heatmap-method`;
# clustering on columns can be applied by `make_column_cluster,Heatmap-method`
# and layout can be constructed by `make_layout,Heatmap-method`. Basically,
# these three methods will be called when calling `draw,Heatmap-method` or `draw,HeatmapList-method`.
#
# If ``km`` or/and ``split`` are set, the clustering inside each row slice uses ``clustering_method_rows``
# and ``clustering_method_rows`` as input parameters.
# 
# Following methods can be applied on the `Heatmap` object:
#
# - `show,Heatmap-method`: drwa a single heatmap with default parameters
# - `draw,Heatmap-method`: draw a single heatmap.
# - `add_heatmap,Heatmap-method` add heatmaps to a list of heatmaps.
#
# == value
# A `Heatmap` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "initialize",
    signature = "Heatmap",
    definition = function(.Object, matrix, col, name, rect_gp = gpar(col = NA),
    row_title = character(0), row_title_side = c("left", "right"), 
    row_title_gp = gpar(fontsize = 14), column_title = character(0), 
    column_title_side = c("top", "bottom"), column_title_gp = gpar(fontsize = 14),
    cluster_rows = TRUE, clustering_distance_rows = "euclidean", 
    clustering_method_rows = "complete", row_hclust_side = c("left", "right"), 
    row_hclust_width = unit(10, "mm"), show_row_hclust = TRUE, 
    row_hclust_gp = gpar(), cluster_columns = TRUE, 
    clustering_distance_columns = "euclidean", clustering_method_columns = "complete",
    column_hclust_side = c("top", "bottom"), column_hclust_height = unit(10, "mm"), 
    show_column_hclust = TRUE, column_hclust_gp = gpar(), 
    row_names_side = c("right", "left"), show_row_names = TRUE, 
    row_names_gp = gpar(fontsize = 12), column_names_side = c("bottom", "top"), 
    show_column_names = TRUE, column_names_gp = gpar(fontsize = 12),
    top_annotation = NULL, top_annotation_height = unit(1, "cm"),
    bottom_annotation = NULL, bottom_annotation_height = unit(1, "cm"),
    km = 1, gap = unit(1, "mm"), split = NULL, width = NULL) {

    if(!is.matrix(matrix)) {
    	matrix = matrix(matrix, ncol = 1)
    }
    .Object@matrix = matrix
    .Object@matrix_param$km = km
    .Object@matrix_param$gap = gap
    if(!is.null(split)) {
        if(!is.data.frame(split)) split = data.frame(split)
    }
    .Object@matrix_param$split = split
    .Object@matrix_param$gp = rect_gp
    
    if(missing(name)) {
        name = paste0("matrix", get_heatmap_index() + 1)
        increase_heatmap_index()
    }
    .Object@name = name

    # color for main matrix
    if(missing(col)) {
        col = default_col(matrix, main_matrix = TRUE)
    }
    if(is.function(col)) {
        .Object@matrix_color_mapping = ColorMapping(col_fun = col, name = name)
    } else {
        .Object@matrix_color_mapping = ColorMapping(colors = col, name = name)
    }
    
    if(length(row_title) == 0) {
        row_title = character(0)
    } else if(is.na(row_title)) {
        row_title = character(0)
    } else if(row_title == "") {
        row_title = character(0)
    }
    .Object@row_title = row_title
    .Object@row_title_param$side = match.arg(row_title_side)[1]
    .Object@row_title_param$gp = row_title_gp

    if(length(column_title) == 0) {
        column_title = character(0)
    } else if(is.na(column_title)) {
        column_title = character(0)
    } else if(column_title == "") {
        column_title = character(0)
    }
    .Object@column_title = column_title
    .Object@column_title_param$side = match.arg(column_title_side)[1]
    .Object@column_title_param$gp = column_title_gp

    if(is.null(rownames(matrix))) {
        show_row_names = FALSE
    }
    .Object@row_names_param$side = match.arg(row_names_side)[1]
    .Object@row_names_param$show = show_row_names
    .Object@row_names_param$gp = row_names_gp

    if(is.null(colnames(matrix))) {
        show_column_names = FALSE
    }
    .Object@column_names_param$side = match.arg(column_names_side)[1]
    .Object@column_names_param$show = show_column_names
    .Object@column_names_param$gp = column_names_gp

    if(!cluster_rows) {
        row_hclust_width = unit(0, "null")
        show_row_hclust = FALSE
    }
    .Object@row_hclust_list = list()
    .Object@row_hclust_param$cluster = cluster_rows
    .Object@row_hclust_param$distance = clustering_distance_rows
    .Object@row_hclust_param$method = clustering_method_rows
    .Object@row_hclust_param$side = match.arg(row_hclust_side)[1]
    .Object@row_hclust_param$width = row_hclust_width
    .Object@row_hclust_param$show = show_row_hclust
    .Object@row_hclust_param$gp = row_hclust_gp
    .Object@row_order_list = list(seq_len(nrow(matrix)))

    if(!cluster_columns) {
        column_hclust_height = unit(0, "null")
        show_column_hclust = FALSE
    }
    .Object@column_hclust = NULL
    .Object@column_hclust_param$cluster = cluster_columns
    .Object@column_hclust_param$distance = clustering_distance_columns
    .Object@column_hclust_param$method = clustering_method_columns
    .Object@column_hclust_param$side = match.arg(column_hclust_side)[1]
    .Object@column_hclust_param$height = column_hclust_height
    .Object@column_hclust_param$show = show_column_hclust
    .Object@column_hclust_param$gp = column_hclust_gp
    .Object@column_order = seq_len(ncol(matrix))

    .Object@top_annotation = top_annotation
    .Object@top_annotation_param$height = top_annotation_height
    
    .Object@bottom_annotation = bottom_annotation
    .Object@bottom_annotation_param$height = bottom_annotation_height

    .Object@layout = as.environment(list(
        layout_column_title_top_height = unit(0, "null"),
        layout_column_hclust_top_height = unit(0, "null"),
        layout_column_anno_top_height = unit(0, "null"),
        layout_column_names_top_height = unit(0, "null"),
        layout_column_title_bottom_height = unit(0, "null"),
        layout_column_hclust_bottom_height = unit(0, "null"),
        layout_column_anno_bottom_height = unit(0, "null"),
        layout_column_names_bottom_height = unit(0, "null"),

        layout_row_title_left_width = unit(0, "null"),
        layout_row_hclust_left_width = unit(0, "null"),
        layout_row_names_left_width = unit(0, "null"),
        layout_row_hclust_right_width = unit(0, "null"),
        layout_row_names_right_width = unit(0, "null"),
        layout_row_title_right_width = unit(0, "null"),

        layout_heatmap_width = width,

        layout_index = matrix(nrow = 0, ncol = 2),
        graphic_fun_list = list()
    ))

    return(.Object)

})

# == title
# Make cluster on columns
#
# == param
# -object a `Heatmap` object.
# -order a single string ``hclust`` means the cluster is performed by `stats::hclust`. The value
#        can also be a pre-defined order.
#
# == details
# The function will fill or adjust ``column_hclust`` and ``column_order`` slots.
#
# This function is only for internal use.
#
# == value
# A `Heatmap` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "make_column_cluster",
    signature = "Heatmap",
    definition = function(object, order = "hclust") {
    
    mat = object@matrix
    distance = object@column_hclust_param$distance
    method = object@column_hclust_param$method

    if(length(order) > 1) {
        column_order = order
    } else if(order == "hclust") {
        object@column_hclust = hclust(get_dist(t(mat), distance), method = method)
        column_order = object@column_hclust$order
    }

    object@column_order = column_order
    return(object)
})


# == title
# Make cluster on rows
#
# == param
# -object a `Heatmap` object.
# -order a single string ``hclust`` means the cluster is performed by `stats::hclust`. The value
#        can also be a pre-defined order.
# -km if apply k-means clustering on rows, number of clusters.
# -split a vector or a data frame by which the rows are be splitted.
#
# == details
# The function will fill or adjust ``row_hclust_list``, ``row_order_list``, ``row_title`` and ``matrix_param`` slots.
#
# This function is only for internal use.
#
# == value
# A `Heatmap` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "make_row_cluster",
    signature = "Heatmap",
    definition = function(object, order = "hclust", km = object@matrix_param$km, 
        split = object@matrix_param$split) {

    mat = object@matrix
    distance = object@row_hclust_param$distance
    method = object@row_hclust_param$method

    if(length(order) > 1) {
        row_order = order
    } else {
        row_order = seq_len(nrow(mat))  # default row order
    }

    if(km > 1) {
        km.fit = kmeans(mat, centers = km, iter.max = 50, nstart = round(nrow(mat)*0.1))
        cluster = km.fit$cluster
        meanmat = lapply(unique(cluster), function(i) {
            colMeans(mat[cluster == i, , drop = FALSE])
        })
        meanmat = as.matrix(as.data.frame(meanmat))
        hc = hclust(dist(t(meanmat)))
        cluster2 = numeric(length(cluster))
        for(i in seq_along(hc$order)) {
            cluster2[cluster == hc$order[i]] = i
        }
        cluster2 = paste0("cluster", cluster2)
        split = cbind(split, cluster2)
    }

    row_order_list = list()
    if(is.null(split)) {
        row_order_list[[1]] = row_order
    } else {
        if(is.null(ncol(split))) split = data.frame(split)
        for(i in seq_len(ncol(split))) split[[i]] = as.character(split[[i]])
        # convert the data frame into a vector
        if(ncol(split) == 1) {
            split = split[, 1]
        } else {
            split = do.call("paste", c(split, sep = "/"))
        }
        
        row_levels = unique(split)
        for(i in seq_along(row_levels)) {
            l = split == row_levels[i]
            row_order_list[[i]] = row_order[l]
        }
        object@row_title = row_levels
    }

    if(length(order) == 1) {
        row_hclust_list = rep(list(NULL), length(row_order_list))
        for(i in seq_along(row_order_list)) {
            submat = mat[ row_order_list[[i]], , drop = FALSE]
            if(nrow(submat) > 1) {
                row_hclust_list[[i]] = hclust(get_dist(submat, distance), method = method)
                row_order_list[[i]] = row_order_list[[i]][row_hclust_list[[i]]$order]
            }
        }
        object@row_hclust_list = row_hclust_list
    }
    object@row_order_list = row_order_list
    object@matrix_param$split = split

    return(object)

})

# == title
# Make the layout of a single heatmap
#
# == param
# -object a `Heatmap` object.
# 
# == detail
# The layout of the single heatmap will be established by setting the size of each heatmap components.
# Also functions that make graphics for heatmap components will be recorded.
#
# Whether apply row clustering or column clustering affects the layout, so clustering should be applied 
# first before making the layout.
#
# This function is only for internal use.
#
# == value
# A `Heatmap` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "make_layout",
    signature = "Heatmap",
    definition = function(object) {

    # position of each row-slice
    gap = object@matrix_param$gap
    n_slice = length(object@row_order_list)
    snr = sapply(object@row_order_list, length)
    slice_height = (unit(1, "npc") - gap*(n_slice-1))*(snr/sum(snr))
    for(i in seq_len(n_slice)) {
        if(i == 1) {
            slice_y = unit(1, "npc")
        } else {
            slice_y = unit.c(slice_y, unit(1, "npc") - sum(slice_height[seq_len(i-1)]) - gap*(i-1))
        }
    }

    ###########################################
    ## heatmap body
    object@layout$layout_index = rbind(c(5, 4))
    object@layout$graphic_fun_list = list(function(object) {
        for(i in seq_len(n_slice)) {
            draw_heatmap_body(object, k = i, y = slice_y[i], height = slice_height[i], just = c("center", "top"))
        }
    })

    ############################################
    ## title on top or bottom
    column_title = object@column_title
    column_title_side = object@column_title_param$side
    column_title_gp = object@column_title_param$gp
    if(length(column_title) > 0) {
        if(column_title_side == "top") {
            object@layout$layout_column_title_top_height = grobHeight(textGrob(column_title, gp = column_title_gp))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(1, 4))
        } else {
            object@layout$layout_column_title_bottom_height = grobHeight(textGrob(column_title, gp = column_title_gp))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(9, 4))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_title(object, which = "column"))
    }

    ############################################
    ## title on left or right
    row_title = object@row_title
    row_title_side = object@row_title_param$side
    row_title_gp = object@row_title_param$gp
    if(length(row_title) > 0) {
        if(row_title_side == "left") {
            object@layout$layout_row_title_left_width = max(grobHeight(textGrob(row_title, gp = row_title_gp)))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(5, 1))
        } else {
            object@layout$layout_row_title_right_width = max(grobHeight(textGrob(row_title, gp = row_title_gp)))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(5, 7))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
            for(i in seq_len(n_slice)) {
                draw_title(object, k = i, which = "row", y = slice_y[i], height = slice_height[i], just = c("center", "top"))
            }
        })
    }

    ##########################################
    ## hclust on left or right
    show_row_hclust = object@row_hclust_param$show
    row_hclust_side = object@row_hclust_param$side
    row_hclust_width = object@row_hclust_param$width
    if(show_row_hclust) {
        if(row_hclust_side == "left") {
            object@layout$layout_row_hclust_left_width = row_hclust_width
            object@layout$layout_index = rbind(object@layout$layout_index, c(5, 2))
        } else {
            object@layout$layout_row_hclust_right_width = row_hclust_width
            object@layout$layout_index = rbind(object@layout$layout_index, c(5, 6))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
            for(i in seq_len(n_slice)) {
                draw_hclust(object, k = i, which = "row", y = slice_y[i], height = slice_height[i], just = c("center", "top"))
            }
        })
    }

    ##########################################
    ## hclust on top or bottom
    show_column_hclust = object@column_hclust_param$show
    column_hclust_side = object@column_hclust_param$side
    column_hclust_height = object@column_hclust_param$height
    if(show_column_hclust) {
        if(column_hclust_side == "top") {
            object@layout$layout_column_hclust_top_height = column_hclust_height
            object@layout$layout_index = rbind(object@layout$layout_index, c(2, 4))
        } else {
            object@layout$layout_column_hclust_bottom_height = column_hclust_height
            object@layout$layout_index = rbind(.object@layout$layout_index, c(8, 4))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_hclust(object, which = "column"))
    }
    
    #######################################
    ## row_names on left or right
    row_names_side = object@row_names_param$side
    show_row_names = object@row_names_param$show
    row_names = rownames(object@matrix)
    row_names_gp = object@row_names_param$gp
    if(show_row_names) {
        row_names_width = max(do.call("unit.c", lapply(row_names, function(x) {
            grobWidth(textGrob(x, gp = row_names_gp))
        }))) + unit(2, "mm")
        if(row_names_side == "left") {
            object@layout$layout_row_names_left_width = row_names_width
            object@layout$layout_index = rbind(object@layout$layout_index, c(5, 3))
        } else {
            object@layout$layout_row_names_right_width = row_names_width
            object@layout$layout_index = rbind(object@layout$layout_index, c(5, 5))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
            for(i in seq_len(n_slice)) {
                draw_dimnames(object, k = i, which = "row", x = unit(2, "mm"), y = slice_y[i], height = slice_height[i], just = c("left", "top"))
            }
        })
    }

    #########################################
    ## column_names on top or bottom
    column_names_side = object@column_names_param$side
    show_column_names = object@column_names_param$show
    column_names = colnames(object@matrix)
    column_names_gp = object@column_names_param$gp
    if(show_column_names) {
        column_names_height = max(do.call("unit.c", lapply(column_names, function(x) {
            grobWidth(textGrob(x, gp = column_names_gp))
        }))) + unit(2, "mm")
        if(column_names_side == "top") {
            object@layout$layout_column_names_top_height = column_names_height
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 4))
        } else {
            object@layout$layout_column_names_bottom_height = column_names_height
            object@layout$layout_index = rbind(object@layout$layout_index, c(6, 4))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_dimnames(object, which = "column", y = unit(1, "npc") - unit(2, "mm"), just = c("center", "top")))
    }
    
    ##########################################
    ## annotation on top
    annotation = object@top_annotation
    annotation_height = object@top_annotation_param$height
    if(!is.null(annotation)) {
        object@layout$layout_column_anno_top_height = annotation_height
        object@layout$layout_index = rbind(object@layout$layout_index, c(3, 4))
        
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_annotation(object, which = "top"))
    }

    ##########################################
    ## annotation on bottom
    annotation = object@bottom_annotation
    annotation_height = object@bottom_annotation_param$height
    if(!is.null(annotation)) {
        object@layout$layout_column_anno_bottom_height = annotation_height
        object@layout$layout_index = rbind(object@layout$layout_index, c(7, 4))
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_annotation(object, which = "bottom"))
    }

    return(object)
})

# == title
# Draw the single heatmap with default parameters
#
# == param
# -object a `Heatmap` object.
#
# == details
# Actually it calls `draw,Heatmap-method`, but only with default parameters. If users want to customize the heatmap,
# they can pass parameters directly to `draw,Heatmap-method`.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "show",
    signature = "Heatmap",
    definition = function(object) {

    # cat("A Heatmap object:\n")
    # cat("name:", object@name, "\n")
    # cat("dim:", nrow(object@matrix), "x", ncol(object@matrix), "\n")
    draw(object)
})

# == title
# Add two heatmaps as a heatmap list
#
# == param
# -object a `Heatmap` object.
# -ht a `Heatmap` object or a `HeatmapList` object.
#
# == details
# There is a shortcut function ``+.Heatmap``.
#
# == value
# A `HeatmapList` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "add_heatmap",
    signature = "Heatmap",
    definition = function(object, x) {

    ht_list = new("HeatmapList")
    ht_list = add_heatmap(ht_list, object)
    ht_list = add_heatmap(ht_list, x)
    return(ht_list)

})

# == title
# Draw the heatmap body
#
# == param
# -object a `Heatmap` object.
# -k a matrix may be splitted by rows, the value identifies which row-slice.
# -... pass to `grid::viewport`, basically for defining the position of the viewport.
#
# == details
# The matrix can be splitted into several parts by rows if ``km`` or ``split`` is 
# specified when initializing the `Heatmap` object. If the matrix is splitted, 
# there will be gaps between rows to identify differnet row-slice.
#
# A viewport is created which contains grids.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_heatmap_body",
    signature = "Heatmap",
    definition = function(object, k = 1, ...) {

    row_order = object@row_order_list[[k]]
    column_order = object@column_order

    gp = object@matrix_param$gp

    pushViewport(viewport(name = paste(object@name, "heatmap_body", k, sep = "-"), ...))

    mat = object@matrix[row_order, column_order, drop = FALSE]
    col_matrix = map(object@matrix_color_mapping, mat)

    nc = ncol(mat)
    nr = nrow(mat)
    x = (seq_len(nc) - 0.5) / nc
    y = (rev(seq_len(nr)) - 0.5) / nr
    expand_index = expand.grid(seq_len(nr), seq_len(nc))
    grid.rect(x[expand_index[[2]]], y[expand_index[[1]]], width = 1/nc, height = 1/nr, gp = do.call("gpar", c(list(fill = col_matrix), gp)))
    upViewport()

})

# == title
# Draw dendrogram on row or column
#
# == param
# -object a `Heatmap` object.
# -which dendrogram on the row or on the column of the heatmap
# -k a matrix may be splitted by rows, the value identifies which row-slice.
# -max_height maximum height of the dendrograms.
# -... pass to `grid::viewport`, basically for defining the position of the viewport.
#
# == details
# If the matrix is splitted into several row slices, a list of dendrograms wil be drawn by 
# the heatmap that each dendrogram corresponds to its row slices.
#
# A viewport is created which contains dendrograms.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_hclust",
    signature = "Heatmap",
    definition = function(object,
    which = c("row", "column"), k = 1, max_height = NULL, ...) {

    which = match.arg(which)[1]

    side = switch(which,
        "row" = object@row_hclust_param$side,
        "column" = object@column_hclust_param$side)
    
    hc = switch(which,
        "row" = object@row_hclust_list[[k]],
        "column" = object@column_hclust)

    gp = switch(which,
        "row" = object@row_hclust_param$gp,
        "column" = object@column_hclust_param$gp)

    if(length(hc) == 0) {
        return(invisible(NULL))
    }

    if(!is.null(max_height)) {
        h = hc$height / max_height
    } else {
        h = hc$height / max(hc$height)
    }
    m = hc$merge
    o = hc$order
    n = length(o)

    m[m > 0] = n + m[m > 0] 
    m[m < 0] = abs(m[m < 0])

    dist = matrix(0, nrow = 2 * n - 1, ncol = 2, dimnames = list(NULL, c("x", "y"))) 
    dist[1:n, 1] = 1 / n / 2 + (1 / n) * (match(1:n, o) - 1)

    for(i in 1:nrow(m)){
        dist[n + i, 1] = (dist[m[i, 1], 1] + dist[m[i, 2], 1]) / 2
        dist[n + i, 2] = h[i]
    }
    
    draw_connection = function(x1, x2, y1, y2, y, horizontal = FALSE, gp = gpar()){
        
        if(horizontal) {
            grid.lines(y = c(x1, x1), x = c(y1, y), gp = gp)
            grid.lines(y = c(x2, x2), x = c(y2, y), gp = gp)
            grid.lines(y = c(x1, x2), x = c(y, y), gp = gp)
        } else {
            grid.lines(x = c(x1, x1), y = c(y1, y), gp = gp)
            grid.lines(x = c(x2, x2), y = c(y2, y), gp = gp)
            grid.lines(x = c(x1, x2), y = c(y, y), gp = gp)
        }
    }
    
    if(which == "row" && side == "right") {
        #dist[, 1] = 1 - dist[, 1]
    } else if(which == "row" && side == "left") {
        #dist[, 1] = 1 - dist[, 1]
        dist[, 2] = 1 - dist[, 2]
        h = 1 - h
    } else if(which == "column" && side == "bottom") {
        dist[, 2] = 1 - dist[, 2]
        h = 1 - h
    }

    if(which == "row") {
        pushViewport(viewport(name = paste(object@name, "hclust_row", k, sep = "-"), ...))
        for(i in 1:nrow(m)){
            draw_connection(dist[m[i, 1], 1], dist[m[i, 2], 1], dist[m[i, 1], 2], dist[m[i, 2], 2], h[i], horizontal = TRUE, gp = gp)
        }
    } else {
        pushViewport(viewport(name = paste(object@name, "hclust_col", sep = "-"), ...))
        for(i in 1:nrow(m)){
            draw_connection(dist[m[i, 1], 1], dist[m[i, 2], 1], dist[m[i, 1], 2], dist[m[i, 2], 2], h[i], horizontal = FALSE, gp = gp)
        }
    }

    upViewport()
})

# == title
# Draw row names or column names
#
# == param
# -object a `Heatmap` object.
# -which names on the row or on the column of the heatmap
# -k a matrix may be splitted by rows, the value identifies which row-slice.
# -... pass to `grid::viewport`, basically for defining the position of the viewport.
#
# == details
# A viewport is created which contains row names or column names.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_dimnames",
    signature = "Heatmap",
    definition = function(object,
    which = c("row", "column"), k = 1, ...) {

    which = match.arg(which)[1]

    side = switch(which,
        "row" = object@row_names_param$side,
        "column" = object@column_names_param$side)

    nm = switch(which,
        "row" = rownames(object@matrix)[ object@row_order_list[[k]] ],
        "column" = colnames(object@matrix)[ object@column_order ])
    
    gp = switch(which,
        "row" = object@row_names_param$gp,
        "column" = object@column_names_param$gp)

    if(is.null(nm)) {
        return(invisible(NULL))
    }

    n = length(nm)
    
    if(which == "row") {
        pushViewport(viewport(name = paste(object@name, "row_names", k, sep = "-"), ...))
        if(side == "left") {
            x = unit(1, "npc")
            just = c("right", "center")
        } else {
            x = unit(0, "npc")
            just = c("left", "center")
        }
        y = (rev(seq_len(n)) - 0.5) / n
        grid.text(nm, x, y, just = just, gp = gp)
    } else {
        pushViewport(viewport(name = paste(object@name, "column_names", sep = "-"), ...))
        x = (seq_len(n) - 0.5) / n
        if(side == "top") {
            y = unit(0, "npc")
            just = c("left", "center")
        } else {
            y = unit(1, "npc")
            just = c("right", "center")
        }
        grid.text(nm, x, y, rot = 90, just = just, gp = gp)
    }

    upViewport()
})

# == title
# Draw heatmap title
#
# == param
# -object a `Heatmap` object.
# -which title on the row or on the column of the heatmap
# -k a matrix may be splitted by rows, the value identifies which row-slice.
# -... pass to `grid::viewport`, basically for defining the position of the viewport.
#
# == details
# A viewport is created which contains heatmap title.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_title",
    signature = "Heatmap",
    definition = function(object,
    which = c("row", "column"), k = 1, ...) {

    which = match.arg(which)[1]

    side = switch(which,
        "row" = object@row_hclust_param$side,
        "column" = object@column_hclust_param$side)

    gp = switch(which,
        "row" = object@row_title_param$gp,
        "column" = object@column_title_param$gp)

    title = switch(which,
        "row" = object@row_title[k],
        "column" = object@column_title)

    if(which == "row") {
        rot = switch(side,
            "left" = 90,
            "right" = 270)

        pushViewport(viewport(name = paste(object@name, "row_title", k, sep = "-"), clip = FALSE, ...))
        grid.text(title, rot = rot, gp = gp)
        upViewport()
    } else {
        pushViewport(viewport(name = paste(object@name, "column_title", sep = "-"), clip = FALSE, ...))
        grid.text(title, gp = gp)
        upViewport()
    }
})

# == title
# Draw column annotations
#
# == param
# -object a `Heatmap` object.
#
# == details
# A viewport is created which contains column annotations.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_annotation",
    signature = "Heatmap",
    definition = function(object, which = c("top", "bottom")) {
    
    which = match.arg(which)[1]

    annotation = switch(which,
        top = object@top_annotation,
        bottom = object@bottom_annotation)

    # if there is no annotation, draw nothing
    if(is.null(annotation)) {
        return(invisible(NULL))
    }

    draw(annotation, index = object@column_order)
})

# == title
# Width of each heatmap component
#
# == param
# -object a `Heatmap` object.
# -k which components, see `Heatmap-class`.
#
# == detials
#
# This function is only for internal use.
#
# == value
# A `grid::unit` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "component_width",
    signature = "Heatmap",
    definition = function(object, k = 1:7) {

    .single_unit = function(k) {
        if(k == 1) {
            object@layout$layout_row_title_left_width
        } else if(k == 2) {
            object@layout$layout_row_hclust_left_width
        } else if(k == 3) {
            object@layout$layout_row_names_left_width
        } else if(k == 4) {
            unit(1, "null")
        } else if(k == 5) {
            object@layout$layout_row_names_right_width
        } else if(k == 6) {
            object@layout$layout_row_hclust_right_width
        } else if(k == 7) {
            object@layout$layout_row_title_right_width
        } else {
            stop("wrong 'k'")
        }
    }

    do.call("unit.c", lapply(k, function(i) .single_unit(i)))
})

# == title
# Height of each heatmap component
#
# == param
# -object a `Heatmap` object.
# -k which components, see `Heatmap-class`.
#
# == detail
#
# This function is only for internal use.
#
# == value
# A `grid::unit` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "component_height",
    signature = "Heatmap",
    definition = function(object, k = 1:9) {

    .single_unit = function(k) {
        if(k == 1) {
            object@layout$layout_column_title_top_height
        } else if(k == 2) {
            object@layout$layout_column_hclust_top_height
        } else if(k == 3) {
            object@layout$layout_column_anno_top_height
        } else if(k == 4) {
            object@layout$layout_column_names_top_height
        } else if(k == 5) {
            unit(1, "null")
        } else if(k == 6) {
            object@layout$layout_column_names_bottom_height
        } else if(k == 7) {
            object@layout$layout_column_anno_bottom_height
        } else if(k == 8) {
            object@layout$layout_column_hclust_bottom_height
        } else if(k == 9) {
            object@layout$layout_column_title_bottom_height
        } else {
            stop("wrong 'k'")
        }
    }

    do.call("unit.c", lapply(k, function(i) .single_unit(i)))
})

# == title
# Set height of each heatmap component
#
# == param
# -object a `Heatmap` object.
# -k which components, see `Heatmap-class`.
# -v height of the component, a `grid::unit` object.
#
# == detail
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "set_component_height",
    signature = "Heatmap",
    definition = function(object, k, v) {

    if(k == 1) {
        object@layout$layout_title_top_height = v
    } else if(k == 2) {
        object@layout$layout_column_hclust_top_height = v
    } else if(k == 3) {
        object@layout$layout_column_anno_top_height = v
    } else if(k == 4) {
        object@layout$layout_column_names_top_height = v
    } else if(k == 6) {
        object@layout$layout_column_names_bottom_height = v
    } else if(k == 7) {
        object@layout$layout_column_anno_bottom_height = v
    } else if(k == 8) {
        object@layout$layout_column_hclust_bottom_height = v
    } else if(k == 9) {
        object@layout$layout_title_bottom_height = v
    } else {
        stop("wrong 'k'")
    }
})

# == title
# Draw a single heatmap
#
# == param
# -object a `Heatmap` object.
# -internal only for internal use.
# -test only for testing
# -... pass to `draw,HeatmapList-method`.
#
# == detail
# The function creates a `HeatmapList` object, add a single heatmap
# and call `draw,HeatmapList-method` to make the final heatmap.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw",
    signature = "Heatmap",
    definition = function(object, internal = FALSE, test = FALSE, ...) {

    if(test) {
        object = prepare(object)
        grid.newpage()
        draw(object, internal = TRUE)
    } else {
        if(internal) {  # a heatmap without legend
            layout = grid.layout(nrow = 9, ncol = 7, widths = component_width(object, 1:7), 
                heights = component_height(object, 1:9))
            pushViewport(viewport(layout = layout, name = "main_heatmap_list"))
            
            ht_layout_index = object@layout$layout_index
            ht_graphic_fun_list = object@layout$graphic_fun_list
            
            for(j in seq_len(nrow(ht_layout_index))) {
                pushViewport(viewport(layout.pos.row = ht_layout_index[j, 1], layout.pos.col = ht_layout_index[j, 2]))
                ht_graphic_fun_list[[j]](object)
                upViewport()
            }

            upViewport()
        } else {
            ht_list = new("HeatmapList")
            ht_list = add_heatmap(ht_list, object)
            draw(ht_list, ...)
        }
    }
})

# == title
# Prepare the heatmap
#
# == param
# -object a `Heatmap` object.
# -row_order orders of rows, pass to `make_row_cluster,Heatmap-method`.
# -split how to split rows in the matrix, passing to `make_row_cluster,Heatmap-method`.
# -show_row_title whether show row titles
#
# == detail
# The preparation of the heatmap includes following steps:
#
# - making clustering on rows if specified
# - making clustering on columns if specified
# - set row title to a empty string if specified
# - makeing the layout of the heatmap
#
# This function is only for internal use.
#
# == value
# A `Heatmap` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "prepare",
    signature = "Heatmap",
    definition = function(object, row_order = "hclust", split = object@matrix_param$split, show_row_title = TRUE) {
    if(object@row_hclust_param$cluster) object = make_row_cluster(object, order = row_order, split = split)
    if(object@column_hclust_param$cluster) object = make_column_cluster(object)
    if(!show_row_title) object@row_title = character(0)
    object = make_layout(object)
    return(object)
})

# == title
# Add two heatmaps as a heatmap list
#
# == param
# -x a `Heatmap` object.
# -y a `Heatmap` object or a `HeatmapList` object.
#
# == detail
# It is only a shortcut function. It actually calls `add_heatmap,Heatmap-method`.
#
# == value
# a `HeatmapList` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
"+.Heatmap" = function(x, y) {
    add_heatmap(x, y)
}


# == title
# Calculate distance from a matrix
#
# == param
# -mat a matrix. The distance is calculated by rows.
# -pairwise_fun a function which calculates distance between two vectors.
# -... pass to `stats::dist`.
#
# == detail
# You can construct any type of distance measurements by defining a pair-wise distance function.
#
# == value
# A `stats::dist` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
dist2 = function(mat, pairwise_fun = function(x, y) sqrt(sum((x - y)^2)), ...) {

    if(!is.matrix(mat)) {
        stop("`mat` should be a matrix.")
    }

    if(nrow(mat) < 2) {
        stop("`mat` should have at least two rows.")
    }

    nr = nrow(mat)
    mat2 = matrix(NA, nrow = nr, ncol = nr)
    rownames(mat2) = colnames(mat2) = rownames(mat)

    for(i in 2:nr) {
        for(j in 1:(nr-1)) {
            mat2[i, j] = pairwise_fun(mat[i, ], mat[j, ])
        }
    }

    as.dist(mat2, ...)
}


get_dist = function(matrix, method) {
    if(is.function(method)) {
        nargs = length(as.list(args(method)))
        if(nargs == 2) { # a distance function
            dst = method(matrix)
        } else if(nargs == 3) {
            dst = dist2(matrix, method)
        } else {
            stop("Since your distance method is a funciton, it can only accept one or two arguments.")
        }
    } else if(method %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) {
        dst = dist(matrix, method = method)
    } else if(method %in% c("pearson", "spearman", "kendall")) {
        dst = switch(method,
                     pearson = as.dist(1 - cor(t(matrix), method = "pearson")),
                     spearman = as.dist(1 - cor(t(matrix), method = "spearman")),
                     kendall = as.dist(1 - cor(t(matrix), method = "kendall")))
    }
    return(dst)
}
