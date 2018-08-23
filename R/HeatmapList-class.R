
# == title
# Class for a list of heatmaps
#
# == details
# A heatmap list is defined as a list of heatmaps and row annotations.
#
# The components for the heamtap list are placed into a 7 x 7 layout:
#
#          +------+(1)
#          +------+(2)
#          +------+(3)
#    +-+-+-+------+-+-+-+
#    |1|2|3| 4(4) |5|6|7|
#    +-+-+-+------+-+-+-+
#          +------+(5)
#          +------+(6)
#          +------+(7)
# 
# From top to bottom in column 4, the regions are:
#
# - annotation legend on the top, graphics are drawn by `draw_annotation_legend,HeatmapList-method`.
# - heatmap legend on the top, graphics are drawn by `draw_heatmap_legend,HeatmapList-method`.
# - title for the heatmap list which is put on the top, graphics are drawn by `draw_title,HeatmapList-method`.
# - the list of heatmaps and row annotations
# - title for the heatmap list which is put on the bottom, graphics are drawn by `draw_title,HeatmapList-method`.
# - heatmap legend on the bottom, graphics are drawn by `draw_heatmap_legend,HeatmapList-method`.
# - annotation legend on the bottom, graphics are drawn by `draw_annotation_legend,HeatmapList-method`.
# 
# From left to right in row 4, the regions are:
#
# - annotation legend on the left, graphics are drawn by `draw_annotation_legend,HeatmapList-method`.
# - heatmap legend on the left, graphics are drawn by `draw_heatmap_legend,HeatmapList-method`.
# - title for the heatmap list which is put on the left, graphics are drawn by `draw_title,HeatmapList-method`.
# - the list of heatmaps and row annotations
# - title for the heatmap list which is put on the right, graphics are drawn by `draw_title,HeatmapList-method`.
# - heatmap legend on the right, graphics are drawn by `draw_heatmap_legend,HeatmapList-method`.
# - annotation legend on the right, graphics are drawn by `draw_annotation_legend,HeatmapList-method`.
#
# For the list of heatmaps which are placed at (5, 5) in the layout, the heatmaps and row annotations
# are placed one after the other.
#
# == methods
# The `HeatmapList-class` provides following methods:
#
# - `draw,HeatmapList-method`: draw the list of heatmaps and row annotations.
# - `add_heatmap,HeatmapList-method`: add heatmaps to the list of heatmaps.
# - `row_order,HeatmapList-method`: get order of rows
# - `column_order,HeatmapList-method`: get order of columns
# - `row_dend,HeatmapList-method`: get row dendrograms
# - `column_dend,HeatmapList-method`: get column dendrograms
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
HeatmapList = setClass("HeatmapList",
    slots = list(
        ht_list = "list",
        ht_list_param = "list",

        row_title = "ANY",
        row_title_param = "list",
        column_title = "ANY",
        column_title_param = "list",

        annotation_legend_param = "list",
        heatmap_legend_param = "list",

        layout = "list"
    ),
    prototype = list(
        layout = list(
            layout_annotation_legend_left_width = unit(0, "mm"),
            layout_heatmap_legend_left_width = unit(0, "mm"),
            layout_row_title_left_width = unit(0, "mm"),
            layout_row_title_right_width = unit(0, "mm"),
            layout_heatmap_legend_right_width = unit(0, "mm"),
            layout_annotation_legend_right_width = unit(0, "mm"),

            layout_annotation_legend_top_height = unit(0, "mm"),
            layout_heatmap_legend_top_height = unit(0, "mm"),
            layout_column_title_top_height = unit(0, "mm"),
            layout_column_title_bottom_height = unit(0, "mm"),
            layout_heatmap_legend_bottom_height = unit(0, "mm"),
            layout_annotation_legend_bottom_height = unit(0, "mm"),
            
            layout_index = matrix(nrow = 0, ncol = 2),
            graphic_fun_list = list(),

            initialized = FALSE
        )
    ),
    contains = "AdditiveUnit"
)

# == title
# Constructor method for HeatmapList class
#
# == param
# -... arguments
#
# == details
# There is no public constructor method for the `HeatmapList-class`.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
HeatmapList = function(...) {
    new("HeatmapList", ...)
}

# == title
# Add heatmaps and row annotations to the heatmap list
#
# == param
# -object a `HeatmapList-class` object.
# -x a `Heatmap-class` object or a `HeatmapAnnotation-class` object or a `HeatmapList-class` object.
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
    signature = "HeatmapList",
    definition = function(object, x) {
    
    # check settings of this new heatmap
    if(inherits(x, "Heatmap")) {
        ht_name = x@name
        x = list(x)
        names(x) = ht_name
        object@ht_list = c(object@ht_list, x)
    } else if(inherits(x, "HeatmapAnnotation")) {
        if(x@which == "row") {
            ht_name = x@name
            x = list(x)
            names(x) = ht_name
            object@ht_list = c(object@ht_list, x)
        } else {
            stop("You should specify `which` to `row` in you add a HeatmapAnnotation which shows row annotations.")    
        }
    } else if(inherits(x, "HeatmapList")) {
        ht_name = names(x@ht_list)
        object@ht_list = c(object@ht_list, x@ht_list)
    }

    ht_name = names(object@ht_list)
    which_duplicated = duplicated(ht_name)
    if(any(which_duplicated)) {
        warning(paste0("Heatmap/row annotation names are duplicated: ", paste(ht_name[which_duplicated], collapse = ", ")))
    }

    l = which(sapply(object@ht_list, inherits, "Heatmap"))
    nr = sapply(object@ht_list[l], function(ht) nrow(ht@matrix))

    if(length(unique(nr)) > 1) {
        cat("`nrow` of all heatmaps:\n")
        print(nr)
        stop("`nrow` of all heatmaps should be the same.")
        for(i in l) {
            cat(object@ht_list[[i]]@name, ":", nrow(object@ht_list[[i]]@matrix), "\n")
        }
        cat("\n")
    }

    return(object)
})

# == title
# Make layout for the complete plot
#
# == param
# -object a `HeatmapList-class` object.
# -row_title title on the row.
# -row_title_side will the title be put on the left or right of the heatmap.
# -row_title_gp graphic parameters for drawing text.
# -column_title title on the column.
# -column_title_side will the title be put on the top or bottom of the heatmap.
# -column_title_gp graphic parameters for drawing text.
# -heatmap_legend_side side of the heatmap legend.
# -merge_legends whether put heatmap legends and annotation legends in a same column
# -show_heatmap_legend whether show heatmap legend.
# -heatmap_legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -annotation_legend_side side of annotation legend.
# -show_annotation_legend whether show annotation legend.
# -annotation_legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -gap gap between heatmaps, should be a `grid::unit` object.
# -row_gap gap between row clusters if rows are split
# -main_heatmap name or index for the main heatmap
# -row_dend_side if auto adjust, where to put the row dendrograms for the main heatmap
# -row_hclust_side deprecated, use ``row_dend_side`` instead
# -row_sub_title_side if auto adjust, where to put sub row titles for the main heatmap
# -cluster_rows same setting as in `Heatmap`, if it is specified, ``cluster_rows`` in main heatmap is ignored.
# -clustering_distance_rows same setting as in `Heatmap`, if it is specified, ``clustering_distance_rows`` in main heatmap is ignored.
# -clustering_method_rows same setting as in `Heatmap`, if it is specified, ``clustering_method_rows`` in main heatmap is ignored.
# -row_dend_width same setting as in `Heatmap`, if it is specified, ``row_dend_width`` in main heatmap is ignored.
# -show_row_dend same setting as in `Heatmap`, if it is specified, ``show_row_dend`` in main heatmap is ignored.
# -row_dend_reorder same setting as in `Heatmap`, if it is specified, ``row_dend_reorder`` in main heatmap is ignored.
# -row_dend_gp same setting as in `Heatmap`, if it is specified, ``row_dend_gp`` in main heatmap is ignored.
# -row_order same setting as in `Heatmap`, if it is specified, ``row_order`` in main heatmap is ignored.
# -km same setting as in `Heatmap`, if it is specified, ``km`` in main heatmap is ignored.
# -split same setting as in `Heatmap`, if it is specified, ``split`` in main heatmap is ignored.
# -combined_name_fun same setting as in `Heatmap`, if it is specified, ``combined_name_fun`` in main heatmap is ignored.
#
# == detail
# It sets the size of each component of the heatmap list and adjusts graphic parameters for each heatmap if necessary.
#
# The layout for the heatmap list and layout for each heatmap are calculated when drawing the heatmap list.
#
# This function is only for internal use.
#
# == value
# A `HeatmapList-class` object in which settings for each heatmap are adjusted.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "make_layout",
    signature = "HeatmapList",
    definition = function(object, 

    row_title = character(0), 
    row_title_side = c("left", "right"), 
    row_title_gp = gpar(fontsize = 14),
    column_title = character(0), 
    column_title_side = c("top", "bottom"), 
    column_title_gp = gpar(fontsize = 14), 

    heatmap_legend_side = c("right", "left", "bottom", "top"), 
    merge_legends = FALSE,
    show_heatmap_legend = TRUE, 
    heatmap_legend_list = list(),
    annotation_legend_side = c("right", "left", "bottom", "top"), 
    show_annotation_legend = TRUE, 
    annotation_legend_list = list(),

    gap = unit(2, "mm"), 
    ht_gap = gap, 

    row_dend_side = c("original", "left", "right"),
    row_sub_title_side = c("original", "left", "right"),

    padding = NULL,
    
    main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
    row_gap = NULL,
    cluster_rows = NULL,
    clustering_distance_rows = NULL,
    clustering_method_rows = NULL,
    row_dend_width = NULL, 
    show_row_dend = NULL, 
    row_dend_reorder = NULL,
    row_dend_gp = NULL,
    row_order = NULL,
    km = NULL,
    split = NULL,
    row_km = km,
    row_split = split,
    heatmap_body_height = NULL) {

    verbose = ht_global_opt("verbose")

    if(object@layout$initialized) {
        if(verbose) qqcat("heatmap list is already initialized\n")
        return(object)
    }

    n_ht = length(object@ht_list)
    i_main = main_heatmap[1]

    # i_main is aleays numeric index
    if(is.character(i_main)) {
        i_main = which(names(object@ht_list) == i_main)[1]
    }

    if(verbose) qqcat("@{n_ht} heatmaps/annotations, main heatmap: @{i_main}th\n")

    if(inherits(object@ht_list[[i_main]], "HeatmapAnnotation")) {
        stop("the main heatmap can only be the heatmap.")
    }

    nr = nrow(object@ht_list[[i_main]]@matrix)

    if(n_ht > 1) {
        if(length(ht_gap) == 1) {
            if(inherits(ht_gap, "unit")) {
                ht_gap = rep(ht_gap, n_ht)
            }
        } else if(length(ht_gap) == n_ht - 1) {
            ht_gap = unit.c(ht_gap, unit(0, "mm"))
        } else if(length(ht_gap) > n_ht) {
            stop(paste0("length of `ht_gap` can only be 1 or ", n_ht-1, "."))
        }
    } else {
        if(!is.unit(ht_gap)) {
            warning("`ht_gap` should be a unit object, reset it to unit(0, 'mm').")
            ht_gap = unit(rep(0, n_ht), "mm")    
        }
    }
    object@ht_list_param$ht_gap = ht_gap

    for(i in seq_len(n_ht)) {
        # if the zero-column matrix is the first one
        if(inherits(object@ht_list[[i]], "Heatmap")) {
            if(i == 1 && ncol(object@ht_list[[1]]@matrix) == 0) {
                ht_gap[1] = unit(0, "mm")
                if(verbose) qqcat("The first heatmap has zero columns, set the first gap to unit(0, 'mm')\n")
            } else if(i == n_ht && ncol(object@ht_list[[n_ht]]@matrix) == 0) {
                ht_gap[n_ht - 1] = unit(0, "mm")
                if(verbose) qqcat("The last heatmap has zero columns, set the last gap to unit(0, 'mm')\n")
            } else if(ncol(object@ht_list[[i]]@matrix) == 0) {
                ht_gap[i] = unit(0, "mm")
                if(verbose) qqcat("The @{i}th heatmap has zero columns, set the @{i}th gap to unit(0, 'mm')\n")
            }
        }
    }

    ### update some values for the main heatmap
    if(!is.null(row_split)) {
        object@ht_list[[i_main]]@matrix_param$row_split = row_split
        if(verbose) qqcat("set row_split to main heatmap\n")
    }
    if(!is.null(row_km)) {
        object@ht_list[[i_main]]@matrix_param$row_km = row_km
        if(verbose) qqcat("set row_km to main heatmap\n")
    }

    if(!is.null(row_gap)) {
        object@ht_list[[i_main]]@matrix_param$row_gap = row_gap
        if(verbose) qqcat("set row_gap to main heatmap\n")
    }

    if(!is.null(cluster_rows)) {

        if(is.null(show_row_dend) && identical(cluster_rows, TRUE)) {
            show_row_dend = TRUE
        }

        if(inherits(cluster_rows, c("dendrogram", "hclust"))) {
            object@ht_list[[i_main]]@row_dend_param$obj = cluster_rows
            object@ht_list[[i_main]]@row_dend_param$cluster = TRUE
            if(verbose) qqcat("set cluster_rows to main heatmap\n")
        } else if(inherits(cluster_rows, "function")) {
            object@ht_list[[i_main]]@row_dend_param$fun = cluster_rows
            object@ht_list[[i_main]]@row_dend_param$cluster = TRUE
            if(verbose) qqcat("set cluster_rows to main heatmap\n")
        } else {
            object@ht_list[[i_main]]@row_dend_param$cluster = cluster_rows
            if(verbose) qqcat("set cluster_rows to main heatmap\n")
            if(!cluster_rows) {
                row_dend_width = unit(0, "mm")
                show_row_dend = FALSE
            } else {
                row_dend_width = unit(10, "mm")
            }
        }
    }

    if(!is.null(show_row_dend)) {
        if(!show_row_dend) {
            row_dend_width = unit(0, "mm")
        }
    }
    if(!is.null(clustering_distance_rows)) {
        object@ht_list[[i_main]]@row_dend_param$distance = clustering_distance_rows
        if(verbose) qqcat("set clustering_distance_rows to main heatmap\n")
    }
    if(!is.null(clustering_method_rows)) {
        object@ht_list[[i_main]]@row_dend_param$method = clustering_method_rows
        if(verbose) qqcat("set clustering_method_rows to main heatmap\n")
    }
    if(!is.null(row_dend_width)) {
        if(row_dend_width[[1]] == 0) {
            object@ht_list[[i_main]]@row_dend_param$width = unit(0, "mm")
        } else {
            object@ht_list[[i_main]]@row_dend_param$width = row_dend_width + DENDROGRAM_PADDING  # append the gap
        }
    }
    if(!is.null(show_row_dend)) {
        object@ht_list[[i_main]]@row_dend_param$show = show_row_dend
    }
    if(!is.null(row_dend_gp)) {
        object@ht_list[[i_main]]@row_dend_param$gp = check_gp(row_dend_gp)
        if(verbose) qqcat("set row_dend_gp to main heatmap\n")
    }
    if(!is.null(row_dend_reorder)) {
        object@ht_list[[i_main]]@row_dend_param$reorder = row_dend_reorder
        if(verbose) qqcat("set row_dend_reorder to main heatmap\n")
    }
    if(!is.null(row_order)) {
        if(is.character(row_order)) {
            row_order = structure(seq_len(nrow(object@ht_list[[i_main]]@matrix)), names = rownames(object@ht_list[[i_main]]@matrix))[row_order]
        }
        object@ht_list[[i_main]]@row_order = row_order
        if(verbose) qqcat("set row_order to main heatmap\n")
    }

    if(verbose) qqcat("auto adjust all heatmap/annotations by the main heatmap\n")

    ######## auto adjust ##########
    ht_main = object@ht_list[[i_main]]
    ht_main = make_row_cluster(ht_main)  # with pre-defined order
    if(verbose) qqcat("perform row clustering on the main heatmap\n")
    object@ht_list[[i_main]] = ht_main

    row_dend_side = match.arg(row_dend_side)[1]
    row_sub_title_side = match.arg(row_sub_title_side)[1]

    if(row_dend_side == "left" || row_sub_title_side == "left") {
        # if the first one is a HeatmapAnnotation object
        # add a heatmap with zero column so that we can put titles and dend on the most left
        if(inherits(object@ht_list[[1]], "HeatmapAnnotation")) {
            object = Heatmap(matrix(nrow = nr, ncol = 0)) + object
            gap = unit.c(unit(0, "mm"), gap)
            i_main = i_main + 1
            if(verbose) qqcat("add a zero-column heatmap for row dend/title on the very left\n")
        }
            
    }

    if(row_dend_side == "right" || row_sub_title_side == "right") {
        # if the last one is a HeatmapAnnotation object
        if(inherits(object@ht_list[[ length(object@ht_list) ]], "HeatmapAnnotation")) {
            object = object + Heatmap(matrix(nrow = nr, ncol = 0))
            gap = unit.c(gap, unit(0, "mm"))
            if(verbose) qqcat("add a zero-column heatmap for row dend/title on the very right\n")
        }
    }

    object@ht_list_param$main_heatmap = i_main
    object@ht_list_param$merge_legends = merge_legends
    if(!is.null(padding)) {
        if(length(padding) == 1) {
            padding = rep(padding, 4)
        } else if(length(padding) == 2) {
            padding = rep(padding, 2)
        } else if(length(padding) != 4) {
            stop("`padding` can only have length of 1, 2, 4")
        }
    }
    object@ht_list_param$padding = padding

    ## orders of other heatmaps should be changed
    for(i in seq_len(n_ht)) {
        if(inherits(object@ht_list[[i]], "Heatmap") & i != i_main) {
            object@ht_list[[i]]@row_order_list = ht_main@row_order_list
            object@ht_list[[i]]@row_order = ht_main@row_order
            object@ht_list[[i]]@row_dend_param$show = FALSE
            object@ht_list[[i]]@row_dend_param$cluster = FALSE  # don't do clustering because cluster was already done
        }
    }
    if(verbose) qqcat("adjust row order for all other heatmaps\n")

    # update other heatmaps' row titles
    for(i in seq_len(n_ht)) {
        if(inherits(object@ht_list[[i]], "Heatmap") && i != i_main) {
            object@ht_list[[i]]@row_title = character(0)
        }
    }
    if(verbose) qqcat("remove row titles for all other heatmaps\n")

    if(row_dend_side == "left") {
        # move dend to the first one
        object@ht_list[[i_main]]@row_dend_param$show = FALSE
        object@ht_list[[1]]@row_dend_list = ht_main@row_dend_list
        object@ht_list[[1]]@row_dend_param = ht_main@row_dend_param
        object@ht_list[[1]]@row_dend_param$side = "left"
        if(verbose) qqcat("add dendrogram of the main heatmap to the left of the first heatmap\n")
    } else if(row_dend_side == "right") {
        object@ht_list[[i_main]]@row_dend_param$show = FALSE
        object@ht_list[[n_ht]]@row_dend_list = ht_main@row_dend_list
        object@ht_list[[n_ht]]@row_dend_param = ht_main@row_dend_param
        object@ht_list[[n_ht]]@row_dend_param$side = "right"
        if(verbose) qqcat("add dendrogram of the main heatmap to the right of the last heatmap\n")
    }

    if(row_sub_title_side == "left") {
        object@ht_list[[i_main]]@row_title = character(0)
        object@ht_list[[1]]@row_title = ht_main@row_title
        object@ht_list[[1]]@row_title_param = ht_main@row_title_param
        object@ht_list[[1]]@row_title_param$side = "left"
        object@ht_list[[1]]@row_title_param$just = get_text_just(ht_main@row_title_param$rot, "left")
        if(verbose) qqcat("add row title of the main heatmap to the left of the first heatmap\n")
    } else if(row_sub_title_side == "right") {
        object@ht_list[[i_main]]@row_title = character(0)
        object@ht_list[[n_ht]]@row_title = ht_main@row_title
        object@ht_list[[n_ht]]@row_title_param = ht_main@row_title_param
        object@ht_list[[n_ht]]@row_title_param$side = "right"
        object@ht_list[[n_ht]]@row_title_param$just = get_text_just(ht_main@row_title_param$rot, "right")
        if(verbose) qqcat("add row title of the main heatmap to the right of the last heatmap\n")
    }

    # gap 
    for(i in seq_len(n_ht)) {
        if(inherits(object@ht_list[[i]], "Heatmap")) {
            object@ht_list[[i]]@matrix_param$row_gap = ht_main@matrix_param$row_gap
        }
    }
    if(verbose) qqcat("adjust row_gap for all other heatmaps\n")

    for(i in seq_len(n_ht)) {
        # supress row clustering because all rows in all heatmaps are adjusted
        if(inherits(object@ht_list[[i]], "Heatmap")) {
            if(verbose) qqcat("prepare layout for heatmap: @{object@ht_list[[i]]@name}\n")
            object@ht_list[[i]] = prepare(object@ht_list[[i]], process_rows = FALSE)
        }
    }

    object = adjust_heatmap_list(object)
    object@layout$layout_index = rbind(c(4, 4))
    object@layout$graphic_fun_list = list(function(object) draw_heatmap_list(object))

    ############################################
    ## title on top or bottom
    column_title_side = match.arg(column_title_side)[1]
    if(length(column_title) == 0) {
        column_title = character(0)
    } else if(!inherits(column_title, c("expression", "call"))) {
        if(is.na(column_title)) {
            column_title = character(0)
        } else if(column_title == "") {
            column_title = character(0)
        }   
    }
    object@column_title = column_title
    object@column_title_param$gp = check_gp(column_title_gp)
    object@column_title_param$side = column_title_side
    if(length(column_title) > 0) {
        if(column_title_side == "top") {
            object@layout$layout_column_title_top_height = grobHeight(textGrob(column_title, gp = column_title_gp)) + TITLE_PADDING*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(3, 4))
        } else {
            object@layout$layout_column_title_bottom_height = grobHeight(textGrob(column_title, gp = column_title_gp)) + TITLE_PADDING*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(5, 4))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_title(object, which = "column"))
    }

    ############################################
    ## title on left or right
    row_title_side = match.arg(row_title_side)[1]
    if(length(row_title) == 0) {
        row_title = character(0)
    } else if(!inherits(row_title, c("expression", "call"))) { 
        if(is.na(row_title)) {
            row_title = character(0)
        } else if(row_title == "") {
            row_title = character(0)
        }
    }
    object@row_title = row_title
    object@row_title_param$gp = check_gp(row_title_gp)
    object@row_title_param$side = row_title_side
    if(length(row_title) > 0) {
        if(row_title_side == "left") {
            object@layout$layout_row_title_left_width = grobHeight(textGrob(row_title, gp = row_title_gp)) + TITLE_PADDING*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 3))
        } else {
            object@layout$layout_row_title_right_width = grobHeight(textGrob(row_title, gp = row_title_gp)) + TITLE_PADDING*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 5))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_title(object, which = "row"))
    }

    #################################################
    ## heatmap legend to top, bottom, left and right
    # default values
    ColorMappingList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(inherits(ht, "Heatmap")) {
            if(merge_legends && !is.null(ht@top_annotation)) {
                ColorMappingList = c(ColorMappingList, get_color_mapping_list(ht@top_annotation))
            }
            if(ht@heatmap_param$show_heatmap_legend) {
                ColorMappingList = c(ColorMappingList, ht@matrix_color_mapping)
            }
            if(merge_legends && !is.null(ht@bottom_annotation)) {
                ColorMappingList = c(ColorMappingList, get_color_mapping_list(ht@bottom_annotation))
            }
        }
        if(inherits(ht, "HeatmapAnnotation")) {
            ColorMappingList = c(ColorMappingList, get_color_mapping_list(ht))
        }
    }
    if(length(ColorMappingList) == 0 && length(heatmap_legend_list) == 0) {
        show_heatmap_legend = FALSE
    }

    # ### proper max_height/max_width for legends #####
    # ht_main = object@ht_list[[i_main]]
    # max_legend_height = unit(dev.size("cm")[2], "cm") - object@ht_list_param$padding[1] -object@ht_list_param$padding[3] -
    #     ht_main@layout$layout_size$column_title_top_height - ht_main@layout$layout_size$column_dend_top_height - 
    #     ht_main@layout$layout_size$column_title_bottom_height - ht_main@layout$layout_size$column_dend_bottom_height
    # if(heatmap_legend_side %in% c("top", "bottom") && annotation_legend_side %in% c("left", "right") || 
    #    heatmap_legend_side %in% c("left", "right") && annotation_legend_side %in% c("top", "bottom")) {
    #     max_legend_width = unit(dev.size("cm")[1], "cm")
    # }


    object@heatmap_legend_param$show = show_heatmap_legend
    heatmap_legend_side = match.arg(heatmap_legend_side)[1]
    object@heatmap_legend_param$side = heatmap_legend_side   
    if(show_heatmap_legend) {
        if(heatmap_legend_side == "top") {
            object@heatmap_legend_param$padding = unit(c(2, 0, 0, 0), "mm")
            size = heatmap_legend_size(object, legend_list = heatmap_legend_list)
            object@heatmap_legend_param$size = size
            object@layout$layout_heatmap_legend_top_height = size[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(2, 4))
        } else if(heatmap_legend_side == "bottom") {
            object@heatmap_legend_param$padding = unit(c(0, 0, 2, 0), "mm")
            size = heatmap_legend_size(object, legend_list = heatmap_legend_list)
            object@heatmap_legend_param$size = size
            object@layout$layout_heatmap_legend_bottom_height = size[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(6, 4))
        } else if(heatmap_legend_side == "left") {
            object@heatmap_legend_param$padding = unit(c(0, 0, 0, 2), "mm")
            size = heatmap_legend_size(object, legend_list = heatmap_legend_list)
            object@heatmap_legend_param$size = size
            object@layout$layout_heatmap_legend_left_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 2))
        } else if(heatmap_legend_side == "right") {
            object@heatmap_legend_param$padding = unit(c(0, 2, 0, 0), "mm")
            size = heatmap_legend_size(object, legend_list = heatmap_legend_list)
            object@heatmap_legend_param$size = size
            object@layout$layout_heatmap_legend_right_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 6))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_heatmap_legend(object, legend_list = heatmap_legend_list))
    } else {
        object@heatmap_legend_param$size = unit(c(0, 0), "mm")
    }

    #################################################
    ## annotation legend to top, bottom, left and right
    # default values
    ColorMappingList = list()
    if(!merge_legends) {
        for(i in seq_along(object@ht_list)) {
            ht = object@ht_list[[i]]
            if(inherits(ht, "Heatmap")) {
                if(!is.null(ht@top_annotation)) {
                    ColorMappingList = c(ColorMappingList, get_color_mapping_list(ht@top_annotation))
                }
                if(!is.null(ht@bottom_annotation)) {
                    ColorMappingList = c(ColorMappingList, get_color_mapping_list(ht@bottom_annotation))
                }
            }
        }
    } else {
        annotation_legend_list = list()
    }
    if(length(ColorMappingList) == 0 && length(annotation_legend_list) == 0) {
        show_annotation_legend = FALSE
    }
    object@annotation_legend_param$show = show_annotation_legend
    annotation_legend_side = match.arg(annotation_legend_side)[1]
    object@annotation_legend_param$side = annotation_legend_side
    if(show_annotation_legend) {
        if(annotation_legend_side == "top") {
            object@annotation_legend_param$padding = unit(c(2, 0, 0, 0), "mm")
            size = annotation_legend_size(object, legend_list = annotation_legend_list)
            object@annotation_legend_param$size = size
            object@layout$layout_annotation_legend_top_height = size[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(1, 4))
        } else if(annotation_legend_side == "bottom") {
            object@annotation_legend_param$padding = unit(c(0, 0, 2, 0), "mm")
            size = annotation_legend_size(object, legend_list = annotation_legend_list)
            object@annotation_legend_param$size = size
            object@layout$layout_annotation_legend_bottom_height = size[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(7, 4))
        } else if(annotation_legend_side == "left") {
            object@annotation_legend_param$padding = unit(c(0, 0, 0, 2), "mm")
            size = annotation_legend_size(object, legend_list = annotation_legend_list)
            object@annotation_legend_param$size = size
            object@layout$layout_annotation_legend_left_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 1))
        } else if(annotation_legend_side == "right") {
            object@annotation_legend_param$padding = unit(c(0, 2, 0, 0), "mm")
            size = annotation_legend_size(object, legend_list = annotation_legend_list)
            object@annotation_legend_param$size = size
            object@layout$layout_annotation_legend_right_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 7))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_annotation_legend(object, legend_list = annotation_legend_list))
    } else {
        object@annotation_legend_param$size = unit(c(0, 0), "null")
    }

    main_matrix_rn = rownames(object@ht_list[[i_main]]@matrix)
    if(!is.null(main_matrix_rn)) {
        for(i in seq_len(n_ht)) {
            if(i == i_main) next
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                matrix_rn = rownames(object@ht_list[[i]]@matrix)
                if(!is.null(matrix_rn)) {
                    # if same set but different order
                    if(setequal(main_matrix_rn, matrix_rn)) {
                        if(!identical(main_matrix_rn, matrix_rn)) {
                            warning("Row names of heatmap ", i, " is not consistent as the main heatmap (", i_main, ")", sep = "")
                        }
                    }
                }
            }
        }
    }

    object@layout$initialized = TRUE

    return(object)
})

# == title
# Draw a list of heatmaps
#
# == param
# -object a `HeatmapList-class` object
# -padding padding of the plot. Elements correspond to bottom, left, top, right paddings.
# -newpage whether create a new page for the graphics.
# -row_title title on the row.
# -row_title_side will the title be put on the left or right of the heatmap.
# -row_title_gp graphic parameters for drawing text.
# -column_title title on the column.
# -column_title_side will the title be put on the top or bottom of the heatmap.
# -column_title_gp graphic parameters for drawing text.
# -heatmap_legend_side side of the heatmap legend.
# -show_heatmap_legend whether show heatmap legend.
# -heatmap_legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -annotation_legend_side side of annotation legend.
# -show_annotation_legend whether show annotation legend.
# -annotation_legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -gap gap between heatmaps, should be a `grid::unit` object.
# -main_heatmap name or index for the main heatmap
# -row_dend_side if auto adjust, where to put the row dendrograms for the main heatmap
# -row_sub_title_side if auto adjust, where to put sub row titles for the main heatmap
# -... pass to `make_layout,HeatmapList-method`
#
# == detail
# The function first calls `make_layout,HeatmapList-method` to calculate
# the layout of the heatmap list and the layout of every single heatmap,
# then makes the plot by re-calling the graphic functions which are already recorded
# in the layout.
#
# == value
# This function returns a list of row dendrograms and column dendrogram.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw",
    signature = "HeatmapList",
    definition = function(object, 
        padding = NULL, 
        newpage = TRUE,
        row_title = character(0), 
        row_title_side = c("left", "right"), 
        row_title_gp = gpar(fontsize = 14),
        column_title = character(0), 
        column_title_side = c("top", "bottom"), 
        column_title_gp = gpar(fontsize = 14), 
        heatmap_legend_side = c("right", "left", "bottom", "top"), 
        heatmap_legend_offset = unit(0, "mm"),
        show_heatmap_legend = TRUE, 
        heatmap_legend_list = list(),
        annotation_legend_side = c("right", "left", "bottom", "top"), 
        annotation_legend_offset = unit(0, "mm"),
        show_annotation_legend = TRUE, 
        annotation_legend_list = list(),
        gap = unit(2, "mm"), 
        main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
        row_dend_side = c("original", "left", "right"),
        row_sub_title_side = c("original", "left", "right"), ...) {

    l = sapply(object@ht_list, inherits, "Heatmap")
    if(! any(l)) {
        stop("There should be at least one Heatmap in the heatmap list. You can add a matrix with zero column to the list.")
    }
    if(nrow(object@ht_list[[ which(l)[1] ]]@matrix) == 0 && length(l) > 1) {
        stop("Since you have a zeor-row matrix, only one heatmap (no row annotation) is allowed.")
    }

    if(newpage) {
        grid.newpage()
    }

    if(!missing(heatmap_legend_offset) && missing(annotation_legend_offset)) {
        annotation_legend_offset = heatmap_legend_offset
    } else if(missing(heatmap_legend_offset) && !missing(annotation_legend_offset)) {
        heatmap_legend_offset = annotation_legend_offset
    }

    object@heatmap_legend_param$offset = heatmap_legend_offset
    object@annotation_legend_param$offset = annotation_legend_offset

    object = make_layout(
        object, 
        row_title = row_title, 
        row_title_gp = row_title_gp, 
        row_title_side = row_title_side,
        column_title = column_title, 
        column_title_side = column_title_side, 
        column_title_gp = column_title_gp,
        heatmap_legend_side = heatmap_legend_side, 
        show_heatmap_legend = show_heatmap_legend, 
        heatmap_legend_list = heatmap_legend_list,
        annotation_legend_side = annotation_legend_side, 
        show_annotation_legend = show_annotation_legend,
        annotation_legend_list = annotation_legend_list, 
        gap = gap, 
        main_heatmap = main_heatmap, 
        row_dend_side = row_dend_side,
        row_sub_title_side = row_sub_title_side, 
        padding = padding, 
        ...
    )

    
    padding = object@ht_list_param$padding
    layout = grid.layout(nrow = 7, ncol = 7, widths = component_width(object, 1:7), heights = component_height(object, 1:7))
    ht_list_width = sum(component_width(object, 1:7)) + padding[2] + padding[4]
    ht_list_height = sum(component_height(object, 1:7)) + padding[1] + padding[3]

    if(is_abs_unit(ht_list_width)) {
        ht_list_width = unit(round(convertWidth(ht_list_width, "mm", valueOnly = TRUE)), "mm")
        qqcat("Since all heatmaps/annotations have absolute units, the total width of the plot is @{ht_list_width} mm.\n")
        w = ht_list_width
    } else {
        w = unit(1, "npc")
    }
    if(is_abs_unit(ht_list_height)) {
        ht_list_height = unit(round(convertHeight(ht_list_height, "mm", valueOnly = TRUE)), "mm")
        qqcat("Since all heatmaps/annotations have absolute units, the total height of the plot is @{ht_list_height} mm.\n")
        h = ht_list_height
    } else {
        h = unit(1, "npc")
    }

    pushViewport(viewport(name = "global", width = w, height = h))
    pushViewport(viewport(layout = layout, name = "global_layout", x = padding[2], y = padding[1], width = unit(1, "npc") - padding[2] - padding[4],
        height = unit(1, "npc") - padding[1] - padding[3], just = c("left", "bottom")))
    ht_layout_index = object@layout$layout_index
    ht_graphic_fun_list = object@layout$graphic_fun_list
    
    for(j in seq_len(nrow(ht_layout_index))) {
        pushViewport(viewport(layout.pos.row = ht_layout_index[j, 1], layout.pos.col = ht_layout_index[j, 2]))
        ht_graphic_fun_list[[j]](object)
        upViewport()
    }

    upViewport()
    upViewport()

    

    object@ht_list_param$width = ht_list_width
    object@ht_list_param$height = ht_list_height

    return(invisible(object))
})


# == title
# Width of each heatmap list component
#
# == param
# -object a `HeatmapList-class` object.
# -k which component in the heatmap list, see `HeatmapList-class`.
#
# == detail
# This function is only for internal use.
#
# == value
# A `grid::unit` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "component_width",
    signature = "HeatmapList",
    definition = function(object, k = 1:7) {

    .single_unit = function(k) {
        if(k == 1) {
            object@layout$layout_annotation_legend_left_width
        } else if(k == 2) {
            object@layout$layout_heatmap_legend_left_width
        } else if(k == 3) {
            object@layout$layout_row_title_left_width
        } else if(k == 4) {
            width = sum(do.call("unit.c", lapply(object@ht_list, function(ht) {
                    if(inherits(ht, "Heatmap")) {
                        ht@heatmap_param$width
                    } else {
                        ht@size
                    }
                })))
            if(is_abs_unit(width)) {
                width + sum(object@ht_list_param$ht_gap)
            } else {
                unit(1, "null") 
            }
        } else if(k == 5) {
            object@layout$layout_row_title_right_width
        } else if(k == 6) {
            object@layout$layout_heatmap_legend_right_width
        } else if(k == 7) {
            object@layout$layout_annotation_legend_right_width
        } else {
            stop("wrong 'k'")
        }
    }

    do.call("unit.c", lapply(k, function(i) .single_unit(i)))
})

# == title
# Height of each heatmap list component
#
# == param
# -object a `HeatmapList-class` object.
# -k which component in the heatmap list, see `HeatmapList-class`.
#
# == value
# A `grid::unit` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "component_height",
    signature = "HeatmapList",
    definition = function(object, k = 1:7) {

    ht_index = which(sapply(object@ht_list, inherits, "Heatmap"))
    n = length(object@ht_list)

    .single_unit = function(k) {
        if(k == 1) {
            object@layout$layout_annotation_legend_top_height
        } else if(k == 2) {
            object@layout$layout_heatmap_legend_top_height
        } else if(k == 3) {
            object@layout$layout_column_title_top_height
        } else if(k == 4) {
            height = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
                    if(inherits(ht, "Heatmap")) {
                        ht@heatmap_param$height
                    } else {
                        ht@size
                    }
                })))
            if(is_abs_unit(height)) {
                height
            } else {
                unit(1, "null") 
            }
        } else if(k == 5) {
            object@layout$layout_column_title_bottom_height
        } else if(k == 6) {
            object@layout$layout_heatmap_legend_bottom_height
        } else if(k == 7) {
            object@layout$layout_annotation_legend_bottom_height
        } else {
            stop("wrong 'k'")
        }
    }

    do.call("unit.c", lapply(k, function(i) .single_unit(i)))
})

setMethod(f = "adjust_heatmap_list",
    signature = "HeatmapList",
    definition = function(object) {
    
    verbose = ht_global_opt("verbose")

    ## this function does mainly two things
    ## 1. calculate viewport/layout for individual heatmaps/annotations
    ## 2. adjust non heatmap body to fill additional space

    ht_gap = object@ht_list_param$ht_gap
    ht_index = which(sapply(object@ht_list, inherits, "Heatmap"))
    n = length(object@ht_list)

    # adjust top anntatation, top annotation of all heatmaps should be aligned
    max_top_anno_height = max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 4))))
    max_top_anno_height = convertHeight(max_top_anno_height, "mm")
    max_bottom_anno_height = max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 6))))
    max_bottom_anno_height = convertHeight(max_bottom_anno_height, "mm")
    for(i in ht_index) {
        if(has_component(object@ht_list[[i]], "top_annotation")) {
            if(verbose) qqcat("adjust height of top annotation of heamtap @{object@ht_list[[i]]@name}\n")
            object@ht_list[[i]]@top_annotation = resize(object@ht_list[[i]]@top_annotation, height = max_top_anno_height)
            object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 4, object@ht_list[[i]]@top_annotation@size)
        }
        if(has_component(object@ht_list[[i]], "bottom_annotation")) {   
            if(verbose) qqcat("adjust height of bottom annotation of heamtap @{object@ht_list[[i]]@name}\n")
            object@ht_list[[i]]@bottom_annotation = resize(object@ht_list[[i]]@bottom_annotation, height = max_bottom_anno_height)
            object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 6, object@ht_list[[i]]@bottom_annotation@size)
        }
    }

    # since each heatmap actually has nine rows, calculate the maximum height of corresponding rows in all heatmap 
    max_title_component_height = unit.c(
        max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 1)))),
        max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 9))))
    )
    max_title_component_height = convertHeight(max_title_component_height, "mm")

    max_top_component_height = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
        if(inherits(ht, "Heatmap")) {
            sum(component_height(ht, k = 2:4))
        } else {
            ht@extended[3]
        }
    })))
    max_top_component_height = convertHeight(max_top_component_height, "mm")
    max_bottom_component_height = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
        if(inherits(ht, "Heatmap")) {
            sum(component_height(ht, k = 6:8))
        } else {
            ht@extended[1]
        }
    })))
    max_bottom_component_height = convertHeight(max_bottom_component_height, "mm")

    # adjust height 
    if(verbose) qqcat("adjust title/dend height of all heatmaps\n")
    for(i in ht_index) {
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 1, max_title_component_height[1])
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 9, max_title_component_height[2])

        ht = object@ht_list[[i]]
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 2, max_top_component_height - sum(component_height(object@ht_list[[i]], k = 3:4)))
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 8, max_bottom_component_height - sum(component_height(object@ht_list[[i]], k = 6:7)))
    }

    if(verbose) qqcat("adjust height to the main heatmap\n")
    i_main = object@ht_list_param$main_heatmap
    for(i in ht_index) {
        if(i != i_main) {
            object@ht_list[[i]]@matrix_param$height = object@ht_list[[i_main]]@matrix_param$height
            object@ht_list[[i]]@heatmap_param$height = object@ht_list[[i_main]]@heatmap_param$height
        }
    }

    ## only some of the heatmaps have null unit and the number is the number of columns
    total_fixed_width = unit(0, "mm")
    total_null_units_lt = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(inherits(ht, "Heatmap")) {
            total_fixed_width = total_fixed_width + sum(component_width(ht, c(1:3, 5:7)))
            if(is_abs_unit(ht@matrix_param$width)) {
                total_fixed_width = total_fixed_width + ht@matrix_param$width
            } else {
                total_null_units_lt = c(total_null_units_lt, list(ht@matrix_param$width))
            }
        } else {
            total_fixed_width = total_fixed_width + ht@size
        }
    }
    if(n > 1) {
        total_fixed_width = total_fixed_width + sum(ht_gap[seq_len(n-1)])
    }
    if(!all(sapply(total_null_units_lt, is.unit))) {
        warning("Since some of the heatmap_body_width is numeric, all heatmaps should explicitly specify heatmap_body_width as null units or numeric, or else the numeric width is treated as number of columns and will be normalized to other heatmaps.")
    }
    total_null_units = sum(unlist(total_null_units_lt))
    if(total_null_units == 0) {
        heatmap_width = lapply(object@ht_list, function(ht) {
            if(inherits(ht, "Heatmap")) {
                ht@heatmap_param$width
            } else {
                ht@size
            }
        })
    } else {
        unit_per_null = 1/total_null_units*(unit(1, "npc") - convertWidth(total_fixed_width, "mm"))

        heatmap_width = lapply(object@ht_list, function(ht) {
            if(inherits(ht, "Heatmap")) {
                if(is_abs_unit(ht@matrix_param$width)) {
                    ht@heatmap_param$width
                } else {
                    sum(component_width(ht, c(1:3, 5:7))) + ht@matrix_param$width[[1]]*unit_per_null
                }
            } else {
                ht@size
            }
        })
    }

    heatmap_width = do.call("unit.c", heatmap_width)

    object@layout$heatmap_width = heatmap_width
    object@layout$max_top_component_height = max_top_component_height
    object@layout$max_bottom_component_height = max_bottom_component_height
    
    # for annotation, check whether extension exceed plotting regions
    # and calcualte proper paddings
    if(is.null(object@ht_list_param$padding)) {
        object@ht_list_param$padding = unit(c(2, 2, 2, 2), "mm")
        row_anno_index = which(sapply(object@ht_list, inherits, "HeatmapAnnotation"))
        row_anno_max_top_extended = unit(0, "mm")
        if(length(row_anno_index)) {
            row_anno_max_top_extended = max(do.call("unit.c", lapply(object@ht_list[row_anno_index], function(anno) anno@extended[3])))
            row_anno_max_top_extended = convertHeight(row_anno_max_top_extended, "mm")
        }
        row_anno_max_bottom_extended = unit(0, "mm")
        if(length(row_anno_index)) {
            row_anno_max_bottom_extended = max(do.call("unit.c", lapply(object@ht_list[row_anno_index], function(anno) anno@extended[1])))
            row_anno_max_bottom_extended = convertHeight(row_anno_max_bottom_extended, "mm")
        }
        if(row_anno_max_bottom_extended[[1]] > max_bottom_component_height[[1]]) {
            object@ht_list_param$padding[1] = unit(2, "mm") + row_anno_max_bottom_extended - max_bottom_component_height
        }
    }
    return(object)
})

# == title
# Draw the list of heatmaps
#
# == param
# -object a `HeatmapList-class` object
#
# == details
# A viewport is created which contains heatmaps.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_heatmap_list",
    signature = "HeatmapList",
    definition = function(object) {

    n = length(object@ht_list)
    heatmap_width = object@layout$heatmap_width
    max_bottom_component_height = object@layout$max_bottom_component_height
    max_top_component_height = object@layout$max_top_component_height
    ht_gap = object@ht_list_param$ht_gap

    pushViewport(viewport(name = "main_heatmap_list"))
    
    i_main = object@ht_list_param$main_heatmap
    ht_main = object@ht_list[[i_main]]
    slice_y = ht_main@layout$slice$y
    n_slice = length(slice_y)
    slice_height = ht_main@layout$slice$height
    slice_just = ht_main@layout$slice$just

    for(i in seq_len(n)) {
        ht = object@ht_list[[i]]

        if(i > 1) {
            x = sum(heatmap_width[seq_len(i-1)]) + sum(ht_gap[seq_len(i-1)])
        } else {
            x = unit(0, "npc")
        }
        
        pushViewport(viewport(x = x, y = unit(0, "npc"), width = heatmap_width[i], just = c("left", "bottom"), name = paste0("heatmap_", object@ht_list[[i]]@name)))
        if(inherits(ht, "Heatmap")) {
            draw(ht, internal = TRUE)
        } else if(inherits(ht, "HeatmapAnnotation")) {
            # calcualte the position of the heatmap body
            pushViewport(viewport(y = max_bottom_component_height, height = unit(1, "npc") - max_top_component_height - max_bottom_component_height, just = c("bottom")))
            for(j in seq_len(n_slice)) {
                draw(ht, index = ht_main@row_order_list[[j]], y = slice_y[j], height = slice_height[j], just = slice_just[2], k = j, n = n_slice)
            }
            upViewport()
        }
        upViewport()
    }

    upViewport()

})

# == title
# Draw heatmap list title
#
# == param
# -object a `HeatmapList-class` object
# -which dendrogram on the row or on the column of the heatmap
#
# == details
# A viewport is created which contains heatmap list title.
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
    signature = "HeatmapList",
    definition = function(object,
    which = c("column", "row")) {

    which = match.arg(which)[1]

    side = switch(which,
        "row" = object@row_title_param$side,
        "column" = object@column_title_param$side)

    gp = switch(which,
        "row" = object@row_title_param$gp,
        "column" = object@column_title_param$gp)

    title = switch(which,
        "row" = object@row_title,
        "column" = object@column_title)

    if(which == "row") {
        rot = switch(side,
            "left" = 90,
            "right" = 270)

        pushViewport(viewport(name = "global_row_title"))
        grid.text(title, rot = rot, gp = gp)
        upViewport()
    } else {
        pushViewport(viewport(name = "global_column_title"))
        grid.text(title, gp = gp)
        upViewport()
    }
})

# == title
# Draw legends for all heatmaps
#
# == param
# -object a `HeatmapList-class` object
# -legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -... graphic parameters passed to `color_mapping_legend,ColorMapping-method`.
#
# == details
# A viewport is created which contains heatmap legends.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_heatmap_legend",
    signature = "HeatmapList",
    definition = function(object, legend_list = list(), ...) {

    side = object@heatmap_legend_param$side
    size = object@heatmap_legend_param$size
    padding = object@heatmap_legend_param$padding

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(inherits(object@ht_list[[i]], "Heatmap")) {
            if(object@ht_list_param$merge_legends && !is.null(ht@top_annotation)) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
            }
            if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                ColorMappingParamList = c.list(ColorMappingParamList, object@ht_list[[i]]@matrix_legend_param)
            }
            if(object@ht_list_param$merge_legends && !is.null(ht@bottom_annotation)) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
            }
        } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
            ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(object@ht_list[[i]]))
        }
    }

    annotation_legend_side = object@annotation_legend_param$side
    annotation_legend_size = object@annotation_legend_param$size
    offset = object@heatmap_legend_param$offset

    if(inherits(offset, "character")) {
        if(offset == "annotation_top") {
            i_ht = object@ht_list_param$main_heatmap
            main_ht = object@ht_list[[i_ht]]
            offset = unit(1, "npc") - main_ht@layout$layout_size$column_title_top_height - 
                main_ht@layout$layout_size$column_dend_top_height - 
                (unit(0.5, "npc") + size[2]*0.5)
        } 
    }
    offset = unit(0, "mm")

    if(side != annotation_legend_side) {
        y = unit(0.5, "npc")
        pushViewport(viewport(x = unit(0.5, "npc"), y = y + offset, width = size[1], height = size[2], just = c("center", "center")))
    } else {
        if(side %in% c("left", "right")) {
            y1 = unit(0.5, "npc") + size[2]*0.5  # top of heatmap legend
            y2 = unit(0.5, "npc") + annotation_legend_size[2]*0.5
            y = max(y1, y2)
            pushViewport(viewport(x = unit(0.5, "npc"), y = y + offset, width = size[1], height = size[2], just = c("center", "top")))           
        } else {
            x1 = unit(0.5, "npc") - size[1]*0.5  # top of heatmap legend
            x2 = unit(0.5, "npc") - annotation_legend_size[1]*0.5
            x = min(x1, x2)
            pushViewport(viewport(x = x, y = unit(0.5, "npc") + offset, width = size[1], height = size[2], just = c("left", "center")))           
        }
    }
    draw_legend(ColorMappingList, ColorMappingParamList, side = side, legend_list = legend_list, padding = padding, ...)

    upViewport()
})

# == title
# Draw legends for all column annotations
#
# == param
# -object a `HeatmapList-class` object
# -legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -... graphic parameters passed to `color_mapping_legend,ColorMapping-method`.
#
# == details
# A viewport is created which contains annotation legends.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_annotation_legend",
    signature = "HeatmapList",
    definition = function(object, legend_list = list(), ...) {

    side = object@annotation_legend_param$side
    size = object@annotation_legend_param$size
    padding = object@annotation_legend_param$padding
    offset = object@annotation_legend_param$offset

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(inherits(ht, "Heatmap")) {
            if(!is.null(ht@top_annotation)) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
            }
            if(!is.null(ht@bottom_annotation)) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
            }
        }
    }

    heatmap_legend_side = object@heatmap_legend_param$side
    heatmap_legend_size = object@heatmap_legend_param$size
    if(side != heatmap_legend_side) {
        pushViewport(viewport(name = "annotation_legend", x = unit(0.5, "npc") + offset, y = unit(0.5, "npc"), width = size[1], height = size[2], just = c("center", "center")))
    } else {
        if(side %in% c("left", "right")) {
            y1 = unit(0.5, "npc") + size[2]*0.5  # top of heatmap legend
            y2 = unit(0.5, "npc") + heatmap_legend_size[2]*0.5
            y = max(y1, y2)
            pushViewport(viewport(name = "annotation_legend", x = unit(0.5, "npc") + offset, y = y, width = size[1], height = size[2], just = c("center", "top")))           
        } else {
            x1 = unit(0.5, "npc") - size[1]*0.5  # top of heatmap legend
            x2 = unit(0.5, "npc") - heatmap_legend_size[1]*0.5
            x = min(x1, x2)
            pushViewport(viewport(name = "annotation_legend", x = x + offset, y = unit(0.5, "npc"), width = size[1], height = size[2], just = c("left", "center")))           
        }
    }

    draw_legend(ColorMappingList, ColorMappingParamList, side = side, legend_list = legend_list, padding = padding, ...)
    upViewport()
})

# == title
# Size of the heatmap legend viewport
#
# == param
# -object a `HeatmapList-class` object
# -legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -... graphic parameters passed to `color_mapping_legend,ColorMapping-method`.
#
# == detail
# This function is only for internal use.
#
# == value
# A `grid::unit` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "heatmap_legend_size",
    signature = "HeatmapList",
    definition = function(object, legend_list = list(), ...) {

    side = object@heatmap_legend_param$side
    padding = object@heatmap_legend_param$padding

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(inherits(object@ht_list[[i]], "Heatmap")) {
            if(object@ht_list_param$merge_legends && !is.null(ht@top_annotation)) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
            }
            if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                ColorMappingParamList = c.list(ColorMappingParamList, object@ht_list[[i]]@matrix_legend_param)
            }
            if(object@ht_list_param$merge_legends && !is.null(ht@bottom_annotation)) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
            }
            
        } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
            ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(object@ht_list[[i]]))
        }
    }

    size = draw_legend(ColorMappingList, ColorMappingParamList, side = side, plot = FALSE, legend_list = legend_list, padding = padding, ...)

    return(size)
})

# == title
# Size of the annotation legend viewport
#
# == param
# -object a `HeatmapList-class` object.
# -legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -... graphic parameters passed to `color_mapping_legend,ColorMapping-method`.
#
# == detail
# Legends for all heatmaps or legends for all annotations will be put in one viewport. This function
# calculates the size of such viewport. Note graphic parameters for legends will affect the size.
#
# This function is only for internal use.
#
# == value
# A `grid::unit` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "annotation_legend_size",
    signature = "HeatmapList",
    definition = function(object, legend_list = list(), ...) {

    side = object@annotation_legend_param$side
    padding = object@annotation_legend_param$padding

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(inherits(ht, "Heatmap")) {
            if(!is.null(ht@top_annotation)) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
            }
            if(!is.null(ht@bottom_annotation)) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
            }
        }
    }

    size = draw_legend(ColorMappingList, ColorMappingParamList, side = side, plot = FALSE, legend_list = legend_list, padding = padding, ...)
    
    return(size)
})

# create a viewport which contains legend
# currently, one-row or one-column legend is supported
draw_legend = function(ColorMappingList, ColorMappingParamList, side = c("right", "left", "top", "bottom"), 
    plot = TRUE, gap = unit(2, "mm"), legend_list = list(), padding = unit(c(0, 0, 0, 0), "mm"), 
    max_height = unit(dev.size("cm")[2], "cm"), max_width = unit(dev.size("cm")[1], "cm"), ...) {

    side = match.arg(side)[1]

    # remove legends which are duplicated by testing the names
    legend_names = sapply(ColorMappingList, function(x) x@name)
    l = !duplicated(legend_names)
    ColorMappingList = ColorMappingList[l]
    ColorMappingParamList = ColorMappingParamList[l]

    n = length(ColorMappingList)
    if(n == 0 && length(legend_list) == 0) {
        return(unit(c(0, 0), "mm"))
    } else {
        cm_grob = c(lapply(seq_along(ColorMappingList), function(i) color_mapping_legend(ColorMappingList[[i]], param = ColorMappingParamList[[i]], plot = FALSE, ...)), legend_list)
        if(side %in% c("left", "right")) {
            pk = packLegend(list = cm_grob, gap = gap, direction = "vertical", max_height = max_height)  
        } else {
            pk = packLegend(list = cm_grob, gap = gap, direction = "horizontal", max_width = max_width)
        }
    }

    width = convertWidth(grobWidth(pk), "mm")
    height = convertHeight(grobHeight(pk), "mm")

    if(plot) {
        grid.draw(pk)
    }

    size = unit.c(width, height)

    size = unit.c(size[1] + padding[2] + padding[4], size[2] + padding[1] + padding[3])
    return(size)
}

# == title
# Draw a list of heatmaps with default parameters
#
# == param
# -object a `HeatmapList-class` object.
#
# == details
# Actually it calls `draw,HeatmapList-method`, but only with default parameters. If users want to customize the heatmap,
# they can pass parameters directly to `draw,HeatmapList-method`.
#
# == value
# This function returns no value.
#
setMethod(f = "show",
    signature = "HeatmapList",
    definition = function(object) {

    # cat("A HeatmapList object containing", length(object@ht_list), "heatmaps:\n\n")
    # for(i in seq_along(object@ht_list)) {
    #     cat("[", i, "] ", sep = "")
    #     show(object@ht_list[[i]])
    #     cat("\n")
    # }
    draw(object)
})


"[.HeatmapList" = function(x, i, j) {

    if(!is.null(x@layout$initialized)) {
        if(x@layout$initialized) {
            stop("subsetting on HeatmapList can only be applied before `draw()`.")
        }
    }

    if(nargs() == 2) {
        subset_heatmap_list_by_row(x, i)
    } else {
        if(missing(i)) {
            subset_heatmap_list_by_column(x, j)
        } else if(missing(j)) {
            subset_heatmap_list_by_row(x, i)
        } else {
            x = subset_heatmap_list_by_row(x, i)
            subset_heatmap_list_by_column(x, j)
        }
    }
}

# there is no main heatmap yet
subset_heatmap_list_by_row = function(ht_list, ind) {
    
    for(i in seq_along(ht_list@ht_list)) {
        ht_list@ht_list[[i]] = ht_list@ht_list[[i]][ind]
    }
    return(ht_list)
}

subset_heatmap_list_by_column = function(ht_list, ind) {
    if(is.numeric(ind)) {
        ht_list@ht_list = ht_list@ht_list[ind]
    } else {
        ht_list = NULL
        # also check annotation names
        for(nm in names(ht_list@ht_list)) {
            if(inherits(ht_list@ht_list[[nm]], "Heatmap")) {
                if(nm %in% ind) {
                    ht_list[[nm]] = ht_list@ht_list[[nm]]
                }
            } else {
                anno_nm = names(ht_list@ht_list[[nm]]@anno_list)
                if(anno_nm %in% ind) {
                    ht_list[[nm]] = ht_list@ht_list[[nm]][, intersect(ind, anno_nm)]
                }
            }
        }
        ht_list@ht_list = ht_list
    }
    return(ht_list)
}
