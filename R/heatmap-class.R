
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
        matrix = "matrix",  # original order
        row_hclust = "ANY",
        column_hclust = "ANY",
        column_anno = "ANY", # annotation data frame, order of columns are same as matrix
        column_anno_type = "character",
        column_anno_height = "numeric",
        column_anno_color_mapping = "list", # a list of ColorMapping objects
        matrix_color_mapping = "ANY",

        layout = "environment",
        gp_list = "list"
    )
)

# default colors for matrix or annotations
# this function should be improved later
default_col = function(x, main_matrix = FALSE) {

    if(is.factor(x)) {
        x = as.character(x)
    }

    if(length(unique(x)) == 1) {
        x = as.character(x)
    }

    attributes(x) = NULL

    if(is.character(x)) {  # discrete
        levels = unique(x)
        colors = rand_color(length(levels))
        names(colors) = levels
        return(colors)
    } else if(is.numeric(x)) {
        if(main_matrix) {
            col_fun = colorRamp2(seq(min(x), max(x), length.out = 11), 
                rev(brewer.pal(11, "RdYlBu")))
        } else {
            col_fun = colorRamp2(range(min(x), max(x)), c("white", rand_color(1)))
        }
        return(col_fun)
    }
}

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
# -rownames_side should the row names be put on the left or right of the heatmap.
# -show_rownames whether show row names.
# -rownames_gp graphic parameters for drawing text.
# -colnames_side should the column names be put on the top or bottom of the heatmap.
# -show_colnames whether show column names.
# -colnames_gp graphic parameters for drawing text.
# -annotation column annotation. The value should be a data frame for which rownames correspond to
#             column names of the matrix and columns correspond to different annotations.
# -annotation_color colors for the annotations. The value is a list in which each element corresponds
#             to each annotation and the value for each element is a vector or a color mapping function
#             which depends on the annotation is discrete or continuous.
# -annotation_side should the annotaitons be put on the top or bottom of the heatmap.
# -annotation_height height of the annotations, should be a `grid::unit` object.
# -annotation_gp graphic parameters for drawing rectangles.
#
# == details
# Following methods can be applied on the `Heatmap` object:
#
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
    rownames_side = c("right", "left"), show_rownames = TRUE, 
    rownames_gp = gpar(fontsize = 12), colnames_side = c("bottom", "top"), 
    show_colnames = TRUE, colnames_gp = gpar(fontsize = 12),
    annotation = NULL, annotation_color = NULL, annotation_side = c("top", "bottom"),
    annotation_height = if(is.null(annotation)) unit(0, "null") else ncol(annotation)*unit(4, "mm"), 
    annotation_gp = gpar(col = NA)
    ) {

    .Object@gp_list = list(rect_gp = rect_gp,
                     row_title_gp = row_title_gp,
                     column_title_gp = column_title_gp,
                     row_hclust_gp = rownames_gp,
                     column_hclust_gp = column_hclust_gp,
                     rownames_gp = rownames_gp,
                     colnames_gp = colnames_gp,
                     colnames_annotation_gp = annotation_gp)

    if(!is.matrix(matrix)) {
    	matrix = matrix(matrix, ncol = 1)
    }

    if(missing(col)) {
        col = default_col(matrix, main_matrix = TRUE)
    }

    if(missing(name)) {
        name = paste0("matrix", get_heatmap_index() + 1)
        increase_heatmap_index()
    }

    if(is.function(col)) {
        .Object@matrix_color_mapping = ColorMapping(col_fun = col, name = name)
    } else {
        .Object@matrix_color_mapping = ColorMapping(colors = col, name = name)
    }
    .Object@name = name

    if(cluster_rows) {
        .Object@row_hclust = hclust(get_dist(matrix, clustering_distance_rows), method = clustering_method_rows)
        row_order = .Object@row_hclust$order
    } else {
        row_hclust_width = unit(0, "null")
        .Object@row_hclust = NULL
        row_order = seq_len(nrow(matrix))
    }

    if(cluster_columns) {
        .Object@column_hclust = hclust(get_dist(t(matrix), clustering_distance_columns), method = clustering_method_columns)
        column_order = .Object@column_hclust$order
    } else {
        column_hclust_height = unit(0, "null")
        .Object@column_hclust = NULL
        column_order = seq_len(ncol(matrix))
    }

    .Object@matrix = matrix[row_order, column_order, drop = FALSE]

    if(is.null(annotation)) {
        # don't need to consider annotation_color
    } else if(is.data.frame(annotation)) {
        
        for(i in seq_len(ncol(annotation))) {
            if(is.factor(annotation[[i]])) {
                annotation[[i]] = as.character(annotation[[i]])
            }
        }

        # if there is rownames
        if(is.null(rownames(annotation))) {
            .Object@column_anno = annotation[column_order, , drop = FALSE]
        } else {
            .Object@column_anno = annotation[colnames(matrix), , drop = FALSE]
        }

        if(is.null(colnames(annotation))) {
            stop("`annotation` should have colnames.")
        }

        if(is.null(annotation_color)) {
            annotation_color = lapply(annotation, default_col)
        }

        if(is.null(names(annotation_color))) {
            stop("`annotation_color` should have names to map to `annotation`.")
        }

        if(!setequal(colnames(annotation), names(annotation_color))) {
            stop("You should provide colors for all annotations.")
        } else {
            annotation_color = annotation_color[colnames(annotation)]
            annotation_name = names(annotation_color)
            .Object@column_anno_color_mapping = list()

            for(i in seq_along(annotation_color)) {
                if(is.atomic(annotation_color[[i]])) {
                    .Object@column_anno_color_mapping[[i]] = ColorMapping(name = annotation_name[i],
                                                             colors = annotation_color[[i]])
                } else if(is.function(annotation_color[[i]])) {
                    .Object@column_anno_color_mapping[[i]] = ColorMapping(name = annotation_name[i],
                                                                  col_fun = annotation_color[[i]])
                }
            }
        }
    } else {
        stop("`annotation` should be a data frame.")
    }

    .Object@column_anno_type = rep("heatmap", ncols(annotation))
    .Object@column_hclust_height = rep(1, ncols(annotation))

    # settings for positin of each component
    .Object@layout = as.environment(list(
        layout_column_title_top_height = NULL,
        layout_column_hclust_top_height = NULL,
        layout_column_anno_top_height = NULL,
        layout_colnames_top_height = NULL,
        layout_column_title_bottom_height = NULL,
        layout_column_hclust_bottom_height = NULL,
        layout_column_anno_bottom_height = NULL,
        layout_colnames_bottom_height = NULL,

        layout_row_title_left_width = NULL,
        layout_row_hclust_left_width = NULL,
        layout_rownames_left_width = NULL,
        layout_row_hclust_right_width = NULL,
        layout_rownames_right_width = NULL,
        layout_row_title_right_width = NULL,

        layout_index = matrix(nrow = 0, ncol = 2),
        graphic_fun_list = list()
    ))

    .Object@layout$layout_index = rbind(c(5, 4))
    .Object@layout$graphic_fun_list = list(function(object) draw_heatmap_body(object))

    ############################################
    ## title on top or bottom
    column_title_side = match.arg(column_title_side)[1]
    if(length(column_title) == 0) {
        column_title = character(0)
    } else if(is.na(column_title)) {
        column_title = character(0)
    } else if(column_title == "") {
        column_title = character(0)
    }
    if(length(column_title) > 0) {
        column_title = column_title
        if(column_title_side == "top") {
            .Object@layout$layout_column_title_top_height = grobHeight(textGrob(column_title, gp = column_title_gp))*2
            .Object@layout$layout_column_title_bottom_height = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(1, 4))
        } else {
            .Object@layout$layout_column_title_bottom_height = grobHeight(textGrob(column_title, gp = column_title_gp))*2
            .Object@layout$layout_column_title_top_height = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(9, 4))
        }
        .Object@layout$graphic_fun_list = c(.Object@layout$graphic_fun_list, function(object) draw_title(object, column_title, which = "column", side = column_title_side))
    } else {
        .Object@layout$layout_column_title_top_height = unit(0, "null")
        .Object@layout$layout_column_title_bottom_height = unit(0, "null")
    }

    ############################################
    ## title on left or right
    row_title_side = match.arg(row_title_side)[1]
    if(length(row_title) == 0) {
        row_title = character(0)
    } else if(is.na(row_title)) {
        row_title = character(0)
    } else if(row_title == "") {
        row_title = character(0)
    }
    if(length(row_title) > 0) {
        row_title = row_title
        if(row_title_side == "left") {
            .Object@layout$layout_row_title_left_width = grobHeight(textGrob(row_title, gp = row_title_gp))*2
            .Object@layout$layout_row_title_right_width = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(5, 1))
        } else {
            .Object@layout$layout_row_title_right_width = grobHeight(textGrob(row_title, gp = row_title_gp))*2
            .Object@layout$layout_row_title_left_width = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(5, 7))
        }
        .Object@layout$graphic_fun_list = c(.Object@layout$graphic_fun_list, function(object) draw_title(object, row_title, which = "row", side = row_title_side))
    } else {
        .Object@layout$layout_row_title_left_width = unit(0, "null")
        .Object@layout$layout_row_title_right_width = unit(0, "null")
    }

    ##########################################
    ## hclust on left or right
    row_hclust_side = match.arg(row_hclust_side)[1]
    if(show_row_hclust) {
        if(row_hclust_side == "left") {
            .Object@layout$layout_row_hclust_left_width = row_hclust_width
            .Object@layout$layout_row_hclust_right_width = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(5, 2))
        } else {
            .Object@layout$layout_row_hclust_right_width = row_hclust_width
            .Object@layout$layout_row_hclust_left_width = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(5, 6))
        }
        .Object@layout$graphic_fun_list = c(.Object@layout$graphic_fun_list, function(object) draw_hclust(object, which = "row", side = row_hclust_side))
    } else {
        .Object@layout$layout_row_hclust_right_width = unit(0, "null")
        .Object@layout$layout_row_hclust_left_width = unit(0, "null")    
    }

    ##########################################
    ## hclust on top or bottom
    column_hclust_side = match.arg(column_hclust_side)[1]
    if(show_column_hclust) {
        if(column_hclust_side == "top") {
            .Object@layout$layout_column_hclust_top_height = column_hclust_height
            .Object@layout$layout_column_hclust_bottom_height = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(2, 4))
        } else {
            .Object@layout$layout_column_hclust_bottom_height = column_hclust_height
            .Object@layout$layout_column_hclust_top_height = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(8, 4))
        }
        .Object@layout$graphic_fun_list = c(.Object@layout$graphic_fun_list, function(object) draw_hclust(object, which = "column", side = column_hclust_side))
    } else {
        .Object@layout$layout_column_hclust_top_height = unit(0, "null")
        .Object@layout$layout_column_hclust_bottom_height = unit(0, "null")    
    }
    
    #######################################
    ## rownames on left or right
    rownames_side = match.arg(rownames_side)[1]
    if(is.null(rownames(matrix))) {
        show_rownames = FALSE
    }
    if(show_rownames) {
        rownames_width = max(do.call("unit.c", lapply(rownames(matrix), function(x) {
            grobWidth(textGrob(x, gp = rownames_gp))
        }))) + unit(2, "mm")
        if(rownames_side == "left") {
            .Object@layout$layout_rownames_left_width = rownames_width
            .Object@layout$layout_rownames_right_width = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(5, 3))
        } else {
            .Object@layout$layout_rownames_right_width = rownames_width
            .Object@layout$layout_rownames_left_width = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(5, 5))
        }
        .Object@layout$graphic_fun_list = c(.Object@layout$graphic_fun_list, function(object) draw_dimnames(object, which = "row", side = rownames_side))
    } else {
        .Object@layout$layout_rownames_left_width = unit(0, "null")
        .Object@layout$layout_rownames_right_width = unit(0, "null")
    }

    #########################################
    ## colnames on top or bottom
    colnames_side = match.arg(colnames_side)[1]
    if(is.null(colnames(matrix))) {
        show_colnames = FALSE
    }
    if(show_colnames) {
        colnames_height = max(do.call("unit.c", lapply(colnames(matrix), function(x) {
            grobWidth(textGrob(x, gp = colnames_gp))
        }))) + unit(2, "mm")
        if(colnames_side == "top") {
            .Object@layout$layout_colnames_top_height = colnames_height
            .Object@layout$layout_colnames_bottom_height = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(4, 4))
        } else {
            .Object@layout$layout_colnames_bottom_height = colnames_height
            .Object@layout$layout_colnames_top_height = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(6, 4))
        }
        .Object@layout$graphic_fun_list = c(.Object@layout$graphic_fun_list, function(object) draw_dimnames(object, which = "column", side = colnames_side))
    } else {
        .Object@layout$layout_colnames_top_height = unit(0, "null")
        .Object@layout$layout_colnames_bottom_height = unit(0, "null")
    }
    
    ##########################################
    ## annotation on top or bottom
    column_anno_side = match.arg(annotation_side)[1]
    if(is.null(annotation)) {
        .Object@layout$layout_column_anno_top_height = unit(0, "null")
        .Object@layout$layout_column_anno_bottom_height = unit(0, "null")
    } else {
        if(column_anno_side == "top") {
            .Object@layout$layout_column_anno_top_height = annotation_height
            .Object@layout$layout_column_anno_bottom_height = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(3, 4))
        } else {
            .Object@layout$layout_column_anno_bottom_height = annotation_height
            .Object@layout$layout_column_anno_top_height = unit(0, "null")
            .Object@layout$layout_index = rbind(.Object@layout$layout_index, c(7, 4))
        }
        .Object@layout$graphic_fun_list = c(.Object@layout$graphic_fun_list, function(object) draw_annotation(object))
    }

    return(.Object)
})

# show method is in fact a plot method
setMethod(f = "show",
    signature = "Heatmap",
    definition = function(object) {

    cat("A Heatmap object:\n")
    cat("name:", object@name, "\n")
    cat("dim:", nrow(object@matrix), "x", ncol(object@matrix), "\n")

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
    definition = function(object, ht) {

    ht_list = new("HeatmapList")
    ht_list = add_heatmap(ht_list, object)
    ht_list = add_heatmap(ht_list, ht)
    return(ht_list)

})

# == title
# Draw the heatmap body
#
# == param
# -object a `Heatmap` object.
# -gp graphic parameters for drawing rectangles.
#
# == details
# A viewport is created which contains grids.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_heatmap_body",
    signature = "Heatmap",
    definition = function(object, gp = object@gp_list$rect_gp) {

    pushViewport(viewport(name = paste(object@name, "heatmap_body", sep = "-")))
    col_matrix = map(object@matrix_color_mapping, object@matrix)

    nc = ncol(object@matrix)
    nr = nrow(object@matrix)
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
# -which should the dendrogram be drawn on row or column.
# -side side of the dendrogram.
# -gp graphic parameters for drawing lines.
#
# == details
# A viewport is created which contains dendrogram.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_hclust",
    signature = "Heatmap",
    definition = function(object, which = c("row", "column"), 
    side = ifelse(which == "row", "left", "top"), gp = NULL) {

    which = match.arg(which)[1]

    side = side[1]
    if(which == "row" && side %in% c("top", "bottom")) {
        stop("`side` can only be set to 'left' or 'right' if `which` is 'row'.")
    }

    if(which == "column" && side %in% c("left", "right")) {
        stop("`side` can only be set to 'top' or 'bottom' if `which` is 'column'.")
    }

    hc = switch(which,
        "row" = object@row_hclust,
        "column" = object@column_hclust)

    if(is.null(gp)) {
        gp = switch(which,
            "row" = object@gp_list$row_hclust_gp,
            "column" = object@gp_list$column_hclust_gp)
    }

    if(is.null(hc)) {
        return(invisible(NULL))
    }

    h = hc$height / max(hc$height)
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
        pushViewport(viewport(name = paste(object@name, "hclust_row", sep = "-") ))
        for(i in 1:nrow(m)){
            draw_connection(dist[m[i, 1], 1], dist[m[i, 2], 1], dist[m[i, 1], 2], dist[m[i, 2], 2], h[i], horizontal = TRUE, gp = gp)
        }
    } else {
        pushViewport(viewport(name = paste(object@name, "hclust_col", sep = "-") ))
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
# -which draw row names or column names.
# -side side of dimension names.
# -gp graphc parameters for drawing text.
#
# == details
# A viewport is created which contains row names or column names.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_dimnames",
    signature = "Heatmap",
    definition = function(object, which = c("row", "column"),
    side = ifelse(which == "row", "right", "bottom"), gp = NULL) {

    which = match.arg(which)[1]

    side = side[1]
    if(which == "row" && side %in% c("bottom", "top")) {
        stop("`side` can only be set to 'left' or 'right' if `which` is 'row'.")
    }

    if(which == "column" && side %in% c("left", "right")) {
        stop("`side` can only be set to 'top' or 'bottom' if `which` is 'column'.")
    }

    nm = switch(which,
        "row" = rownames(object@matrix),
        "column" = colnames(object@matrix))
    
    if(is.null(gp)) {
        gp = switch(which,
            "row" = object@gp_list$row_hclust_gp,
            "column" = object@gp_list$column_hclust_gp)
    }

    if(is.null(nm)) {
        return(invisible(NULL))
    }

    n = length(nm)
    
    if(which == "row") {
        pushViewport(viewport(name = paste(object@name, "rownames", sep = "-") ))
        if(side == "left") {
            x = unit(1, "npc") - unit(2, "mm")
            just = c("right", "center")
        } else {
            x = unit(0, "npc") + unit(2, "mm")
            just = c("left", "center")
        }
        y = (rev(seq_len(n)) - 0.5) / n
        grid.text(nm, x, y, just = just, gp = gp)
    } else {
        pushViewport(viewport(name = paste(object@name, "colnames", sep = "-") ))
        x = (seq_len(n) - 0.5) / n
        if(side == "top") {
            y = unit(0, "npc") + unit(2, "mm")
            just = c("left", "center")
        } else {
            y = unit(1, "npc") - unit(2, "mm")
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
# -title title.
# -which the title should be plotted on rows or columns.
# -side side of heatmap title.
# -gp graphic paramter for drawing text.
#
# == details
# A viewport is created which contains heatmap title.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_title",
    signature = "Heatmap",
    definition = function(object, title, which = c("row", "column"),
    side = ifelse(which == "row", "right", "bottom"), gp = NULL) {

    which = match.arg(which)[1]

    side = side[1]
    if(which == "row" && side %in% c("bottom", "top")) {
        stop("`side` can only be set to 'left' or 'right' if `which` is 'row'.")
    }

    if(which == "column" && side %in% c("left", "right")) {
        stop("`side` can only be set to 'top' or 'bottom' if `which` is 'column'.")
    }

    if(is.null(gp)) {
        gp = switch(which,
            "row" = object@gp_list$row_title_gp,
            "column" = object@gp_list$column_title_gp)
    }

    if(which == "row") {
        rot = switch(side,
            "left" = 90,
            "right" = 270)

        pushViewport(viewport(name = paste(object@name, "row_title", sep = "-"), clip = FALSE))
        grid.text(title, rot = rot, gp = gp)
        upViewport()
    } else {
        pushViewport(viewport(name = paste(object@name, "column_title", sep = "-"), clip = FALSE))
        grid.text(title, gp = gp)
        upViewport()
    }
})

# == title
# Draw column annotations
#
# == param
# -object a `Heatmap` object.
# -gp graphic parameters for drawing rectangles.
#
# == details
# A viewport is created which contains column annotations.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_annotation",
    signature = "Heatmap",
    definition = function(object, type, height, gp = object@gp_list$colnames_annotation_gp) {
    
    # if there is no annotation, draw nothing
    if(is.null(object@column_anno)) {
        return(invisible(NULL))
    }

    n = ncol(object@column_anno)
    nc = nrow(object@column_anno)  # number of columns which correspond to the matrix
    pushViewport(viewport(name = paste(object@name, "annotation", sep = "-")))
    
    if(missing(type)) {
        type = rep("heatmap", n)
    }
    if(missing(height)) {
        height = rep(1, n)
    }
    height = height / sum(height)

    for(i in seq_len(n)) {
        x = unit(0, "npc")
        y = unit(1, "npc")*sum(height(seq_len(i))
        pushViewport(viewport(x = x, y = y, just = c("left", "top"))) 

        x = (seq_len(nc) - 0.5) / nc
        if(type[i] %in% "heatmap") {
            cm = object@column_anno_color_mapping[[i]]
            fill = map(cm, object@column_anno[[i]])
            grid.rect(x, y = 0.5, width = 1/nc, height = 1, gp = do.call("gpar", c(list(fill = fill), gp)))
        }
        if(type[i] %in% c("p", "point")) {
            y = object@column_anno[[i]]
            if(length(unique(y)) == 1) {
                grid.points(x, 0.5, default.units = "npc", gp = gp)
            } else {
                # normalize to [0, 1]
                y = (y - mean(y))*0.9  # zoom
                y = (y - min(y)) / (max(y) - min(y))
                grid.points(x, y, default.units = "npc", gp = gp)
            }
        }
        if(type[i] %in% c("l", "line")) {
            y = object@column_anno[[i]]
            if(length(unique(y)) == 1) {
                grid.lines(x, 0.5, default.units = "npc", gp = gp)
            } else {
                # normalize to [0, 1]
                y = (y - mean(y))*0.9  # zoom
                y = (y - min(y)) / (max(y) - min(y))
                grid.lines(x, y, default.units = "npc", gp = gp)
            }   
        }
        if(type[i] %in% c("bar", "histogram")) {
            y = object@column_anno[[i]]
            if(length(unique(y)) == 1) {
                grid.rect(x, 0.5, width = 1/nc, default.units = "npc", gp = gp)
            } else {
                # normalize to [0, 1]
                y = (y - mean(y))*0.9  # zoom
                y = (y - min(y)) / (max(y) - min(y))
                grid.rect(x, y = 0, width = 1/nc, height = y, default.units = "npc", just = c("center", "bottom"), gp = gp)
            }   
        }
        if(type[i] %in% c("boxplot")) {
            boxplot_stats = apply(object@matrix, 2, function(x) boxplot.stats(x)$stats)
            boxplot_stats = (boxplot_stats - min(boxplot_stats)) / (max(boxplot_stats) - min(boxplot_stats))
            grid.rect(x, boxplot_stats[2, ], width = 1/nc*0.8, height = boxplot_stats[4, ] - boxplot_stats[2, ], default.unit = "npc", just = c("center", "bottom"), gp = gp)
            grid.points(x, boxplot_stats[3, ], pch = 16, gp = gp)
            grid.segments(x - 1/nc*0.4, boxplot_stats[1, ],
                          x + 1/nc*0.4, boxplot_stats[5, ])
            grid.segments(x, boxplot_stats[1, ],
                          x, boxplot_stats[2, ])
            grid.segments(x, boxplot_stats[4, ],
                          x, boxplot_stats[5, ])
            
        }
        upViewport()
    }
    upViewport()
})

# == title
# Width of each heatmap component
#
# == param
# -object a `Heatmap` object.
# -k which components, see `Heatmap-class`.
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
            object@layout$layout_rownames_left_width
        } else if(k == 4) {
            unit(1, "null")
        } else if(k == 5) {
            object@layout$layout_rownames_right_width
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
            object@layout$layout_colnames_top_height
        } else if(k == 5) {
            unit(1, "null")
        } else if(k == 6) {
            object@layout$layout_colnames_bottom_height
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
        object@layout$layout_colnames_top_height = v
    } else if(k == 6) {
        object@layout$layout_colnames_bottom_height = v
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
# -... pass to `HeatmapList`.
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
    definition = function(object, internal = FALSE, ...) {

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
})

# == title
# Add two heatmaps as a heatmap list
#
# == param
# -ht1 a `Heatmap` object.
# -ht2 a `Heatmap` object or a `HeatmapList` object.
#
# == value
# a `HeatmapList` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
"+.Heatmap" = function(ht1, ht2) {
    add_heatmap(ht1, ht2)
}


# == title
# Calculate distance from a matrix
#
# == param
# -mat a matrix. The distance is calculated by rows.
# -pairwise_fun a function which calculates distance between two vectors.
# -... pass to `stats::dist`.
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
