
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
#          +------+ (1)
#          +------+ (2)
#          +------+ (3)
#          +------+ (4)
#    +-+-+-+------+-+-+-+
#    |1|2|3| 4(5) |5|6|7|
#    +-+-+-+------+-+-+-+
#          +------+ (6)
#          +------+ (7)
#          +------+ (8)
#          +------+ (9)
#
# From top to bottom in column 4, the regions are:
#
# - title which is put on the top of the heatmap, graphics are drawn by `draw_title,Heatmap-method`.
# - column cluster on the top, graphics are drawn by `draw_hclust,Heatmap-method`.
# - column annotation on the top, graphics are drawn by `draw_annotation,Heatmap-method`.
# - column names on the top, graphics are drawn by `draw_dimnames,Heatmap-method`.
# - heatmap body, graphics are drawn by `draw_heatmap_body,Heatmap-method`.
# - column names on the bottom, graphics are drawn by `draw_dimnames,Heatmap-method`.
# - column annotation on the bottom, graphics are drawn by `draw_annotation,Heatmap-method`.
# - column cluster on the bottom, graphics are drawn by `draw_hclust,Heatmap-method`.
# - title on the bottom, graphics are drawn by `draw_title,Heatmap-method`.
# 
# From left to right in row 5, the regions are:
#
# - title which is put in the left of the heatmap, graphics are drawn by `draw_title,Heatmap-method`.
# - row cluster on the left, graphics are drawn by `draw_hclust,Heatmap-method`.
# - row names on the left, graphics are drawn by `draw_dimnames,Heatmap-method`.
# - heatmap body
# - row names on the right, graphics are drawn by `draw_dimnames,Heatmap-method`.
# - row cluster on the right, graphics are drawn by `draw_hclust,Heatmap-method`.
# - title on the right, graphics are drawn by `draw_title,Heatmap-method`.
#
# The `Heatmap-class` is not responsible for heatmap legend and annotation legends. The `draw,Heatmap-method` method
# will construct a `HeatmapList-class` object which only contains one single heatmap
# and call `draw,HeatmapList-method` to make a complete heatmap.
#
# == methods
# The `Heatmap-class` provides following methods:
#
# - `Heatmap`: constructor method.
# - `draw,Heatmap-method`: draw a single heatmap.
# - `add_heatmap,Heatmap-method` append heatmaps and row annotations to a list of heatmaps.
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

        top_annotation = "ANY", # NULL or a `HeatmapAnnotation` object
        top_annotation_param = "list",

        bottom_annotation = "ANY",
        bottom_annotation_param = "list",

        heatmap_param = "list",

        layout = "list"
    ),
    contains = "AdditiveUnit"
)



# == title
# Constructor method for Heatmap class
#
# == param
# -matrix a matrix. Either numeric or character. If it is a simple vector, it will be
#         converted to a one-column matrix.
# -col a vector of colors if the color mapping is discrete or a color mapping 
#      function if the matrix is continuous numbers. Pass to `ColorMapping`.
# -name name of the heatmap. The name is used as the title of the heatmap legend.
# -rect_gp graphic parameters for drawing rectangles (for heatmap body).
# -cell_fun self-defined function to add graphics on each cell. Six parameters will be passed into 
#           this function: ``i``, ``j``, ``x``, ``y``, ``width``, ``height`` which are row index,
#           column index in ``matrix``, coordinate of the middle points in the heatmap body viewport,
#           and the width and height of the cell. 
# -row_title title on row.
# -row_title_side will the title be put on the left or right of the heatmap?
# -row_title_gp graphic parameters for drawing text.
# -column_title title on column.
# -column_title_side will the title be put on the top or bottom of the heatmap?
# -column_title_gp graphic parameters for drawing text.
# -cluster_rows If the value is a logical, it means whether make cluster on rows. The value can also
#               be a `stats::hclust` or a `stats::dendrogram` that already contains clustering information.
#               This means you can use any type of clustering methods and render the `stats::dendrogram`
#               object with self-defined graphic settings.
# -clustering_distance_rows it can be a pre-defined character which is in 
#                ("euclidean", "maximum", "manhattan", "canberra", "binary", 
#                "minkowski", "pearson", "spearman", "kendall"). It can also be a function.
#                If the function has one argument, the input argument should be a matrix and 
#                the returned value should be a `stats::dist` object. If the function has two arguments,
#                the input arguments are two vectors and the function calculates distance between these
#                two vectors.
# -clustering_method_rows method to make cluster, pass to `stats::hclust`.
# -row_hclust_side should the row cluster be put on the left or right of the heatmap?
# -row_hclust_width width of the row cluster, should be a `grid::unit` object.
# -show_row_hclust whether show row clusters. 
# -row_hclust_gp graphics parameters for drawing lines. If users already provide a `stats::dendrogram`
#                object with edges rendered, this argument will be ignored.
# -cluster_columns whether make cluster on columns. Same settings as ``cluster_rows``.
# -clustering_distance_columns same setting as ``clustering_distance_rows``.
# -clustering_method_columns method to make cluster, pass to `stats::hclust`.
# -column_hclust_side should the column cluster be put on the top or bottom of the heatmap?
# -column_hclust_height height of the column cluster, should be a `grid::unit` object.
# -show_column_hclust whether show column clusters.
# -column_hclust_gp graphic parameters for drawling lines. Same settings as ``row_hclust_gp``.
# -row_names_side should the row names be put on the left or right of the heatmap?
# -show_row_names whether show row names.
# -row_names_max_width maximum width of row names viewport. Because some times row names can be very long, it is not reasonable
#                      to show them all.
# -row_names_gp graphic parameters for drawing text.
# -column_names_side should the column names be put on the top or bottom of the heatmap?
# -column_names_max_height maximum height of column names viewport.
# -show_column_names whether show column names.
# -column_names_gp graphic parameters for drawing text.
# -top_annotation a `HeatmapAnnotation` object which contains a list of annotations.
# -top_annotation_height total height of the column annotations on the top.
# -bottom_annotation a `HeatmapAnnotation` object.
# -bottom_annotation_height total height of the column annotations on the bottom.
# -km do k-means clustering on rows. If the value is larger than 1, the heatmap will be split by rows according to the k-means clustering.
#     For each row-clusters, hierarchical clustering is still applied with parameters above.
# -split a vector or a data frame by which the rows are split.
# -gap gap between row-slices if the heatmap is split by rows, should be `grid::unit` object.
# -combined_name_fun if the heatmap is split by rows, how to make a combined row title for each slice?
#                 The input parameter for this function is a vector which contains level names under each column in ``split``.
# -width the width of the single heatmap, should be a fixed `grid::unit` object. It is used for the layout when the heatmap
#        is appended to a list of heatmaps.
# -show_heatmap_legend whether show heatmap legend?
#
# == details
# The initialization function only applies parameter checking and fill values to each slot with proper ones.
# Then it will be ready for clustering and layout.
# 
# Following methods can be applied on the `Heatmap-class` object:
#
# - `show,Heatmap-method`: draw a single heatmap with default parameters
# - `draw,Heatmap-method`: draw a single heatmap.
# - `add_heatmap,Heatmap-method` append heatmaps and row annotations to a list of heatmaps.
#
# The constructor function pretends to be a high-level graphic function because the ``show`` method
# of the `Heatmap-class` object actually plots the graphics.
#
# == value
# A `Heatmap-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
Heatmap = function(matrix, col, name, rect_gp = gpar(col = NA), 
    cell_fun = function(j, i, x, y, width, height, fill) NULL,
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
    row_names_max_width = unit(4, "cm"), row_names_gp = gpar(fontsize = 12), 
    column_names_side = c("bottom", "top"), 
    show_column_names = TRUE, column_names_max_height = unit(4, "cm"), 
    column_names_gp = gpar(fontsize = 12),
    top_annotation = new("HeatmapAnnotation"),
    top_annotation_height = unit(5*length(top_annotation@anno_list), "mm"),
    bottom_annotation = new("HeatmapAnnotation"),
    bottom_annotation_height = unit(5*length(bottom_annotation@anno_list), "mm"),
    km = 1, split = NULL, gap = unit(1, "mm"), 
    combined_name_fun = function(x) paste(x, collapse = "/"),
    width = NULL, show_heatmap_legend = TRUE) {

    .Object = new("Heatmap")

    .Object@heatmap_param$width = width
    .Object@heatmap_param$show_heatmap_legend = show_heatmap_legend

    if(is.data.frame(matrix)) {
        matrix = as.matrix(matrix)
    }
    if(!is.matrix(matrix)) {
        if(is.atomic(matrix)) {
           matrix = matrix(matrix, ncol = 1)
        } else {
            stop("If data is not a matrix, it should be a simple vector.")
        }
    }

    if(ncol(matrix) == 0) {
        .Object@heatmap_param$show_heatmap_legend = FALSE
    }

    if(is.character(matrix) || ncol(matrix) <= 1) {
        cluster_rows = FALSE
        cluster_columns = FALSE
        show_row_hclust = FALSE
        show_column_hclust = FALSE
        km = 1
    }
    .Object@matrix = matrix
    .Object@matrix_param$km = km
    .Object@matrix_param$gap = gap
    if(!is.null(split)) {
        if(!is.data.frame(split)) split = data.frame(split)
        if(nrow(split) != nrow(matrix)) {
            stop("Length or number of rows of `split` should be same as rows in `matrix`.")
        }
    }
    .Object@matrix_param$split = split
    .Object@matrix_param$gp =check_gp(rect_gp)
    .Object@matrix_param$cell_fun = cell_fun
    
    if(missing(name)) {
        name = paste0("matrix_", get_heatmap_index() + 1)
        increase_heatmap_index()
    }
    .Object@name = name

    if(ncol(matrix) == 1 && is.null(colnames(matrix))) {
        colnames(matrix) = name
        .Object@matrix = matrix
    }

    # color for main matrix
    if(ncol(matrix) > 0) {
        if(missing(col)) {
            col = default_col(matrix, main_matrix = TRUE)
        }
        if(is.function(col)) {
            .Object@matrix_color_mapping = ColorMapping(col_fun = col, name = name)
        } else {
            if(is.null(names(col))) {
                if(length(col) == length(unique(matrix))) {
                    names(col) = unique(matrix)
                } else {
                    stop("`col` should have names to map to values in `mat`.")
                }
            }
            .Object@matrix_color_mapping = ColorMapping(colors = col, name = name)
        }
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
    .Object@row_title_param$gp = check_gp(row_title_gp)
    .Object@row_title_param$combined_name_fun = combined_name_fun

    if(length(column_title) == 0) {
        column_title = character(0)
    } else if(is.na(column_title)) {
        column_title = character(0)
    } else if(column_title == "") {
        column_title = character(0)
    }
    .Object@column_title = column_title
    .Object@column_title_param$side = match.arg(column_title_side)[1]
    .Object@column_title_param$gp = check_gp(column_title_gp)

    if(is.null(rownames(matrix))) {
        show_row_names = FALSE
    }
    .Object@row_names_param$side = match.arg(row_names_side)[1]
    .Object@row_names_param$show = show_row_names
    .Object@row_names_param$gp = recycle_gp(check_gp(row_names_gp), nrow(matrix))
    .Object@row_names_param$max_width = row_names_max_width + unit(2, "mm")

    if(is.null(colnames(matrix))) {
        show_column_names = FALSE
    }
    .Object@column_names_param$side = match.arg(column_names_side)[1]
    .Object@column_names_param$show = show_column_names
    .Object@column_names_param$gp = recycle_gp(check_gp(column_names_gp), ncol(matrix))
    .Object@column_names_param$max_height = column_names_max_height + unit(2, "mm")

    if(inherits(cluster_rows, "dendrogram") || inherits(cluster_rows, "hclust")) {
        .Object@row_hclust_param$obj = cluster_rows
        .Object@row_hclust_param$cluster = TRUE
    } else if(inherits(cluster_rows, "function")) {
        .Object@row_hclust_param$fun = cluster_rows
        .Object@row_hclust_param$cluster = TRUE
    } else {
        .Object@row_hclust_param$cluster = cluster_rows
        if(!cluster_rows) {
            row_hclust_width = unit(0, "null")
            show_row_hclust = FALSE
        }
    }
    if(!show_row_hclust) {
        row_hclust_width = unit(0, "null")
    }
    .Object@row_hclust_list = list()
    .Object@row_hclust_param$distance = clustering_distance_rows
    .Object@row_hclust_param$method = clustering_method_rows
    .Object@row_hclust_param$side = match.arg(row_hclust_side)[1]
    .Object@row_hclust_param$width = row_hclust_width
    .Object@row_hclust_param$show = show_row_hclust
    .Object@row_hclust_param$gp = check_gp(row_hclust_gp)
    .Object@row_order_list = list(seq_len(nrow(matrix))) # default order

    if(inherits(cluster_columns, "dendrogram") || inherits(cluster_columns, "hclust")) {
        .Object@column_hclust_param$obj = cluster_columns
        .Object@column_hclust_param$cluster = TRUE
    } else if(inherits(cluster_columns, "function")) {
        .Object@column_hclust_param$fun = cluster_columns
        .Object@column_hclust_param$cluster = TRUE
    } else {
        .Object@column_hclust_param$cluster = cluster_columns
        if(!cluster_columns) {
            column_hclust_height = unit(0, "null")
            show_column_hclust = FALSE
        }
    }
    if(!show_column_hclust) {
        column_hclust_height = unit(0, "null")
    }
    .Object@column_hclust = NULL
    .Object@column_hclust_param$distance = clustering_distance_columns
    .Object@column_hclust_param$method = clustering_method_columns
    .Object@column_hclust_param$side = match.arg(column_hclust_side)[1]
    .Object@column_hclust_param$height = column_hclust_height
    .Object@column_hclust_param$show = show_column_hclust
    .Object@column_hclust_param$gp = check_gp(column_hclust_gp)
    .Object@column_order = seq_len(ncol(matrix))

    .Object@top_annotation = top_annotation # a `HeatmapAnnotation` object
    if(is.null(top_annotation)) {
        .Object@top_annotation_param$height = unit(0, "null")    
    } else {
        .Object@top_annotation_param$height = top_annotation_height
    }
    if(!is.null(top_annotation)) {
        if(length(top_annotation@anno_list) > 0) {
            if(!.Object@top_annotation@which == "column") {
                stop("`which` in `top_annotation` should only be `column`.")
            }
        }
    }
    
    .Object@bottom_annotation = bottom_annotation # a `HeatmapAnnotation` object
    if(is.null(bottom_annotation)) {
        .Object@bottom_annotation_param$height = unit(0, "null")
    } else {
        .Object@bottom_annotation_param$height = bottom_annotation_height
    }
    if(!is.null(bottom_annotation)) {
        if(length(bottom_annotation@anno_list) > 0) {
            if(!.Object@bottom_annotation@which == "column") {
                stop("`which` in `bottom_annotation` should only be `column`.")
            }
        }
    }

    .Object@layout = list(
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

        layout_heatmap_width = width, # for the layout of heatmap list

        layout_index = matrix(nrow = 0, ncol = 2),
        graphic_fun_list = list()
    )

    return(.Object)

}

# == title
# Make cluster on columns
#
# == param
# -object a `Heatmap-class` object.
# -order a pre-defined order.
#
# == details
# The function will fill or adjust ``column_hclust`` and ``column_order`` slots.
#
# This function is only for internal use.
#
# == value
# A `Heatmap-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "make_column_cluster",
    signature = "Heatmap",
    definition = function(object, order = NULL) {
    
    mat = object@matrix
    distance = object@column_hclust_param$distance
    method = object@column_hclust_param$method

    if(!object@column_hclust_param$cluster && is.null(order)) {
        order = seq_len(ncol(mat))
    }

    if(is.null(order)) {
        if(!is.null(object@column_hclust_param$obj)) {
            object@column_hclust = object@column_hclust_param$obj
        } else if(!is.null(object@column_hclust_param$fun)) {
            object@column_hclust = object@column_hclust_param$fun(t(mat))
        } else {
            object@column_hclust = hclust(get_dist(t(mat), distance), method = method)
        }
        column_order = get_hclust_order(object@column_hclust)
    } else {
        column_order = order
    }

    object@column_order = column_order

    return(object)
})


# == title
# Make cluster on rows
#
# == param
# -object a `Heatmap-class` object.
# -order a pre-defined order.
# -km if apply k-means clustering on rows, number of clusters.
# -split a vector or a data frame by which the rows are be split.
#
# == details
# The function will fill or adjust ``row_hclust_list``, ``row_order_list``, ``row_title`` and ``matrix_param`` slots.
#
# If ``order`` is defined, no clustering will be applied.
#
# This function is only for internal use.
#
# == value
# A `Heatmap-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "make_row_cluster",
    signature = "Heatmap",
    definition = function(object, order = NULL, km = object@matrix_param$km, 
    split = object@matrix_param$split) {

    mat = object@matrix
    distance = object@row_hclust_param$distance
    method = object@row_hclust_param$method

    if(!object@row_hclust_param$cluster && is.null(order)) {
        order = seq_len(nrow(mat))
    }

    if(is.null(order)) {

        if(!is.null(object@row_hclust_param$obj)) {
            if(km > 1) {
                stop("You can not make k-means clustering since you have already specified a clustering object.")
            }
            if(!is.null(split)) {
                stop("You can not split by rows since you have already specified a clustering object.")
            }
            object@row_hclust_list = list(object@row_hclust_param$obj)
            object@row_order_list = list(get_hclust_order(object@row_hclust_param$obj))
            return(object)
        }

        row_order = seq_len(nrow(mat))  # default row order
    } else {
        row_order = order
    }

    # make k-means clustering to add a split column
    if(km > 1 && is.numeric(mat)) {
        km.fit = kmeans(mat, centers = km)
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

    # split the original order into a list according to split
    row_order_list = list()
    if(is.null(split)) {
        row_order_list[[1]] = row_order
    } else {
        if(is.null(ncol(split))) split = data.frame(split)
        for(i in seq_len(ncol(split))) split[[i]] = as.character(split[[i]])
        # convert the data frame into a vector
        if(ncol(split) == 1) {
            split = split[, 1]
            split_name = split
        } else {
            combined_name_fun = object@row_title_param$combined_name_fun
            if(!is.null(combined_name_fun)) {
                split_name = apply(as.matrix(split), 1, combined_name_fun)
            }
            split = apply(as.matrix(split), 1, paste, collapse = "/")
        }
        
        row_levels = unique(split)
        for(i in seq_along(row_levels)) {
            l = split == row_levels[i]
            row_order_list[[i]] = row_order[l]
        }

        if(!is.null(object@row_title_param$combined_name_fun)) {
            object@row_title = unique(split_name)
        }
    }

    # make hclust in each slice
    if(is.null(order)) {
        row_hclust_list = rep(list(NULL), length(row_order_list))
        for(i in seq_along(row_order_list)) {
            submat = mat[ row_order_list[[i]], , drop = FALSE]
            if(nrow(submat) > 1) {
                if(!is.null(object@row_hclust_param$fun)) {
                    row_hclust_list[[i]] = object@row_hclust_param$fun(mat)
                    row_order_list[[i]] = row_order_list[[i]][ get_hclust_order(row_hclust_list[[i]]) ]
                } else {
                    if(is.numeric(mat)) {
                        row_hclust_list[[i]] = hclust(get_dist(submat, distance), method = method)
                        row_order_list[[i]] = row_order_list[[i]][ get_hclust_order(row_hclust_list[[i]]) ]
                    }
                }
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
# -object a `Heatmap-class` object.
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
# A `Heatmap-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "make_layout",
    signature = "Heatmap",
    definition = function(object) {

    # for components which are placed by rows, they will be splitted into parts
    # and slice_y controls the y-coordinates of each part

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
        #max_hclust_height = max(sapply(object@row_hclust_list, function(hc) attr(as.dendrogram(hc), "height")))
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
            object@layout$layout_index = rbind(object@layout$layout_index, c(8, 4))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_hclust(object, which = "column"))
    }
    
    #######################################
    ## row_names on left or right
    row_names_side = object@row_names_param$side
    show_row_names = object@row_names_param$show
    row_names = rownames(object@matrix)
    row_names_gp = object@row_names_param$gp;
    if(show_row_names) {
        row_names_width = max(do.call("unit.c", lapply(seq_along(row_names), function(x) {
            cgp = subset_gp(row_names_gp, x)
            grobWidth(textGrob(row_names[x], gp = cgp))
        }))) + unit(2, "mm")
        row_names_width = min(row_names_width, object@row_names_param$max_width)
        if(row_names_side == "left") {
            object@layout$layout_row_names_left_width = row_names_width
            object@layout$layout_index = rbind(object@layout$layout_index, c(5, 3))
        } else {
            object@layout$layout_row_names_right_width = row_names_width
            object@layout$layout_index = rbind(object@layout$layout_index, c(5, 5))
        }
        if(row_names_side == "right") {
            object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
                for(i in seq_len(n_slice)) {
                    draw_dimnames(object, k = i, which = "row", x = unit(2, "mm"), y = slice_y[i], height = slice_height[i], just = c("left", "top"))
                }
            })
        } else {
            object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
                for(i in seq_len(n_slice)) {
                    draw_dimnames(object, k = i, which = "row", x = unit(0, "npc"), y = slice_y[i], height = slice_height[i], width = unit(1, "npc") - unit(2, "mm"), just = c("left", "top"))
                }
            })
        }
    }

    #########################################
    ## column_names on top or bottom
    column_names_side = object@column_names_param$side
    show_column_names = object@column_names_param$show
    column_names = colnames(object@matrix)
    column_names_gp = object@column_names_param$gp
    if(show_column_names) {
        column_names_height = max(do.call("unit.c", lapply(seq_along(column_names), function(x) {
            cgp = subset_gp(column_names_gp, x)
            grobWidth(textGrob(column_names[x], gp = cgp))
        }))) + unit(2, "mm")
        column_names_height = min(column_names_height, object@column_names_param$max_height)
        if(column_names_side == "top") {
            object@layout$layout_column_names_top_height = column_names_height
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 4))
        } else {
            object@layout$layout_column_names_bottom_height = column_names_height
            object@layout$layout_index = rbind(object@layout$layout_index, c(6, 4))
        }
        if(column_names_side == "top") {
            object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_dimnames(object, which = "column", y = unit(1, "npc"), height = unit(1, "npc") - unit(2, "mm"), just = c("center", "top")))
        } else {
            object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_dimnames(object, which = "column", y = unit(1, "npc") - unit(2, "mm"), just = c("center", "top")))
        }
    }
    
    ##########################################
    ## annotation on top
    annotation = object@top_annotation
    annotation_height = object@top_annotation_param$height
    if(!is.null(annotation)) {
        if(length(annotation@anno_list) > 0) {
            object@layout$layout_column_anno_top_height = annotation_height
            object@layout$layout_index = rbind(object@layout$layout_index, c(3, 4))
            
            object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_annotation(object, which = "top"))
        }
    }

    ##########################################
    ## annotation on bottom
    annotation = object@bottom_annotation
    annotation_height = object@bottom_annotation_param$height
    if(!is.null(annotation)) {
        if(length(annotation@anno_list) > 0) {
            object@layout$layout_column_anno_bottom_height = annotation_height
            object@layout$layout_index = rbind(object@layout$layout_index, c(7, 4))
            object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_annotation(object, which = "bottom"))
        }
    }

    return(object)
})

# == title
# Draw the single heatmap with default parameters
#
# == param
# -object a `Heatmap-class` object.
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
# Add heatmaps or row annotations as a heatmap list
#
# == param
# -object a `Heatmap-class` object.
# -x a `Heatmap-class` object, a `HeatmapAnnotation-class` object or a `HeatmapList-class` object.
#
# == details
# There is a shortcut function ``+.AdditiveUnit``.
#
# == value
# A `HeatmapList-class` object.
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
# -object a `Heatmap-class` object.
# -k a matrix may be split by rows, the value identifies which row-slice.
# -... pass to `grid::viewport`, basically for defining the position of the viewport.
#
# == details
# The matrix can be split into several parts by rows if ``km`` or ``split`` is 
# specified when initializing the `Heatmap` object. If the matrix is split, 
# there will be gaps between rows to identify different row-slice.
#
# A viewport is created which contains subset rows of the heatmap.
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

    if(ncol(object@matrix) == 0) {
        return(invisible(NULL))
    }

    row_order = object@row_order_list[[k]]
    column_order = object@column_order

    gp = object@matrix_param$gp

    pushViewport(viewport(name = paste(object@name, "heatmap_body", k, sep = "_"), ...))

    mat = object@matrix[row_order, column_order, drop = FALSE]
    col_matrix = map_to_colors(object@matrix_color_mapping, mat)

    nc = ncol(mat)
    nr = nrow(mat)
    x = (seq_len(nc) - 0.5) / nc
    y = (rev(seq_len(nr)) - 0.5) / nr
    expand_index = expand.grid(seq_len(nr), seq_len(nc))
    if(any(names(gp) %in% c("type"))) {
        if(gp$type == "none") {
        } else {
            grid.rect(x[expand_index[[2]]], y[expand_index[[1]]], width = unit(1/nc, "npc"), height = unit(1/nr, "npc"), gp = do.call("gpar", c(list(fill = col_matrix), gp)))
        }
    } else {
        grid.rect(x[expand_index[[2]]], y[expand_index[[1]]], width = unit(1/nc, "npc"), height = unit(1/nr, "npc"), gp = do.call("gpar", c(list(fill = col_matrix), gp)))
    }
    
    cell_fun = object@matrix_param$cell_fun
    for(i in seq_along(row_order)) {
        for(j in seq_along(column_order)) {
            cell_fun(column_order[j], row_order[i], x[j], y[i], unit(1/nc, "npc"), unit(1/nr, "npc"), col_matrix[i, j])
        }
    }

    upViewport()

})

# == title
# Draw dendrogram on row or column
#
# == param
# -object a `Heatmap-class` object.
# -which is dendrogram put on the row or on the column of the heatmap?
# -k a matrix may be splitted by rows, the value identifies which row-slice.
# -max_height maximum height of the dendrograms.
# -... pass to `grid::viewport`, basically for defining the position of the viewport.
#
# == details
# If the matrix is split into several row slices, a list of dendrograms will be drawn by 
# the heatmap that each dendrogram corresponds to its row slices.
#
# A viewport is created which contains dendrograms.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == seealso
# `grid.dendrogram`
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

    dend = as.dendrogram(hc)
    n = length(labels(dend))

    pushViewport(viewport(name = paste(object@name, which, "cluster", sep = "_"), ...))

    if(side == "left") {
        grid.dendrogram(dend, name = paste(object@name, "hclust_row", k, sep = "_"), max_height = max_height, facing = "right", order = "reverse", x = unit(1, "mm"), width = unit(1, "npc") - unit(1, "mm"), just = "left")
    } else if(side == "right") {
        grid.dendrogram(dend, name = paste(object@name, "hclust_row", k, sep = "_"), max_height = max_height, facing = "left", order = "reverse", x = unit(0, "null"), width = unit(1, "npc") - unit(1, "mm"), just = "left")
    } else if(side == "top") {
        grid.dendrogram(dend, name = paste(object@name, "hclust_column", sep = "_"), max_height = max_height, facing = "bottom", y = unit(0, "null"), height = unit(1, "npc") - unit(1, "mm"), just = "bottom")
    } else if(side == "bottom") {
        grid.dendrogram(dend, name = paste(object@name, "hclust_column", sep = "_"), max_height = max_height, facing = "top", y = unit(1, "mm"), height = unit(1, "npc") - unit(1, "mm"), just = "bottom")
    } 

    upViewport()

})

# == title
# Draw row names or column names
#
# == param
# -object a `Heatmap-class` object.
# -which are names put on the row or on the column of the heatmap?
# -k a matrix may be split by rows, the value identifies which row-slice.
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

    if(which == "row") {
        gp = subset_gp(gp, object@row_order_list[[k]])
    }

    if(is.null(nm)) {
        return(invisible(NULL))
    }

    n = length(nm)
    
    if(which == "row") {
        pushViewport(viewport(name = paste(object@name, "row_names", k, sep = "_"), ...))
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
        pushViewport(viewport(name = paste(object@name, "column_names", sep = "_"), ...))
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
# -object a `Heatmap-class` object.
# -which is title put on the row or on the column of the heatmap?
# -k a matrix may be split by rows, the value identifies which row-slice.
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

        pushViewport(viewport(name = paste(object@name, "row_title", k, sep = "_"), clip = FALSE, ...))
        grid.text(title, rot = rot, gp = gp)
        upViewport()
    } else {
        pushViewport(viewport(name = paste(object@name, "column_title", sep = "_"), clip = FALSE, ...))
        grid.text(title, gp = gp)
        upViewport()
    }
})

# == title
# Draw column annotations
#
# == param
# -object a `Heatmap-class` object.
# -which are the annotations put on the top or bottom of the heatmap?
#
# == details
# A viewport is created which contains column annotations.
#
# Since the column annotations is a `HeatmapAnnotation-class` object, the function
# calls `draw,HeatmapAnnotation-method` to draw the annotations.
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
# -object a `Heatmap-class` object.
# -k which component in the heatmap, see `Heatmap-class`.
#
# == details
#
# This function is only for internal use.
#
# == value
# A `grid::unit` object.
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
            if(ncol(object@matrix) == 0) {
                unit(0, "null")
            } else {
                unit(1, "null")
            }
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
# -object a `Heatmap-class` object.
# -k which component in the heatmap, see `Heatmap-class`.
#
# == detail
#
# This function is only for internal use.
#
# == value
# A `grid::unit` object.
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
# -object a `Heatmap-class` object.
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
        object@layout$layout_column_title_top_height = v
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
        object@layout$layout_column_title_bottom_height = v
    } else {
        stop("wrong 'k'")
    }

    return(object)
})

# == title
# Draw a single heatmap
#
# == param
# -object a `Heatmap-class` object.
# -internal only used inside the calling of `draw,HeatmapList-method`. Only heatmap without legends will be drawn.
# -test only for testing
# -... pass to `draw,HeatmapList-method`.
#
# == detail
# The function creates a `HeatmapList-class` object which only contains a single heatmap
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
            pushViewport(viewport(layout = layout))

            ht_layout_index = object@layout$layout_index
            ht_graphic_fun_list = object@layout$graphic_fun_list
            
            for(j in seq_len(nrow(ht_layout_index))) {
                pushViewport(viewport(layout.pos.row = ht_layout_index[j, 1], layout.pos.col = ht_layout_index[j, 2]))
                ht_graphic_fun_list[[j]](object)
                upViewport()
            }
            upViewport()
        } else {
            if(ncol(object@matrix) == 0) {
                stop("Single heatmap should contains a matrix with at least one column.\nZero-column matrix can only be appended to the heatmap list.")
            }
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
# -object a `Heatmap-class` object.
# -row_order orders of rows, pass to `make_row_cluster,Heatmap-method`. Because if more than one heatmaps
#            are drawn by columns, the order of some heatmap will be adjusted by one certain heatmap, this
#            argument is used to pass a pre-defined row order.
# -split how to split rows in the matrix, passing to `make_row_cluster,Heatmap-method`.
#
# == detail
# The preparation of the heatmap includes following steps:
#
# - making clustering on rows if specified (by calling `make_row_cluster,Heatmap-method`)
# - making clustering on columns if specified (by calling `make_column_cluster,Heatmap-method`)
# - making the layout of the heatmap (by calling `make_layout,Heatmap-method`)
#
# This function is only for internal use.
#
# == value
# A `Heatmap-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "prepare",
    signature = "Heatmap",
    definition = function(object, row_order = NULL, split = object@matrix_param$split) {

    if(object@row_hclust_param$cluster || !is.null(split)) {
        object = make_row_cluster(object, order = row_order, split = split)
    }
    if(object@column_hclust_param$cluster) object = make_column_cluster(object)

    object = make_layout(object)
    return(object)

})
