
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

    ht_gap = unit(2, "mm"), 

    main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
    padding = NULL,

    row_dend_side = c("original", "left", "right"),
    row_sub_title_side = c("original", "left", "right"),
    column_dend_side = c("original", "top", "bottom"),
    column_sub_title_side = c("original", "top", "bottom"),
    
    row_gap = NULL,
    cluster_rows = NULL,
    clustering_distance_rows = NULL,
    clustering_method_rows = NULL,
    row_dend_width = NULL, 
    show_row_dend = NULL, 
    row_dend_reorder = NULL,
    row_dend_gp = NULL,
    row_order = NULL,
    row_km = NULL,
    row_split = NULL,
    heatmap_body_height = NULL,

    column_gap = NULL,
    cluster_columns = NULL,
    clustering_distance_columns = NULL,
    clustering_method_columns = NULL,
    column_dend_width = NULL, 
    show_column_dend = NULL, 
    column_dend_reorder = NULL,
    column_dend_gp = NULL,
    column_order = NULL,
    column_km = NULL,
    column_split = NULL,
    heatmap_body_width = NULL) {

    verbose = ht_opt("verbose")

    if(object@layout$initialized) {
        if(verbose) qqcat("heatmap list is already initialized\n")
        return(object)
    }

    n_ht = length(object@ht_list)
    i_main = main_heatmap[1]
    direction = object@direction

    # i_main is aleays numeric index
    if(is.character(i_main)) {
        i_main = which(names(object@ht_list) == i_main)[1]
    }

    if(verbose) qqcat("@{n_ht} heatmaps/annotations, main heatmap: @{i_main}th\n")

    if(inherits(object@ht_list[[i_main]], "HeatmapAnnotation")) {
        stop("the main heatmap can only be the heatmap.")
    }

    nr = nrow(object@ht_list[[i_main]]@matrix)
    nc = ncol(object@ht_list[[i_main]]@matrix)

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

    # adjust gaps if some heatmap has zero row/column
    for(i in seq_len(n_ht)) {
        if(inherits(object@ht_list[[i]], "Heatmap")) {
            if(direction == "horizontal") {
                if(i == 1 && ncol(object@ht_list[[1]]@matrix) == 0) {
                    ht_gap[1] = unit(0, "mm")
                    if(verbose) qqcat("The first heatmap has zero column, set the first gap to unit(0, 'mm')\n")
                } else if(i == n_ht && ncol(object@ht_list[[n_ht]]@matrix) == 0) {
                    ht_gap[n_ht - 1] = unit(0, "mm")
                    if(verbose) qqcat("The last heatmap has zero column, set the last gap to unit(0, 'mm')\n")
                } else if(ncol(object@ht_list[[i]]@matrix) == 0) {
                    ht_gap[i] = unit(0, "mm")
                    if(verbose) qqcat("The @{i}th heatmap has zero column, set the @{i}th gap to unit(0, 'mm')\n")
                }
            } else {
                if(i == 1 && nrow(object@ht_list[[1]]@matrix) == 0) {
                    ht_gap[1] = unit(0, "mm")
                    if(verbose) qqcat("The first heatmap has zero row, set the first gap to unit(0, 'mm')\n")
                } else if(i == n_ht && nrow(object@ht_list[[n_ht]]@matrix) == 0) {
                    ht_gap[n_ht - 1] = unit(0, "mm")
                    if(verbose) qqcat("The last heatmap has zero row, set the last gap to unit(0, 'mm')\n")
                } else if(nrow(object@ht_list[[i]]@matrix) == 0) {
                    ht_gap[i] = unit(0, "mm")
                    if(verbose) qqcat("The @{i}th heatmap has zero row, set the @{i}th gap to unit(0, 'mm')\n")
                }
            }
        }
    }

    ### update some values for the main heatmap
    if(direction == "horizontal") {
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
    } else {
        if(!is.null(column_split)) {
            object@ht_list[[i_main]]@matrix_param$column_split = column_split
            if(verbose) qqcat("set column_split to main heatmap\n")
        }
        if(!is.null(column_km)) {
            object@ht_list[[i_main]]@matrix_param$column_km = column_km
            if(verbose) qqcat("set column_km to main heatmap\n")
        }

        if(!is.null(column_gap)) {
            object@ht_list[[i_main]]@matrix_param$column_gap = column_gap
            if(verbose) qqcat("set column_gap to main heatmap\n")
        }

        if(!is.null(cluster_columns)) {

            if(is.null(show_column_dend) && identical(cluster_columns, TRUE)) {
                show_column_dend = TRUE
            }

            if(inherits(cluster_columns, c("dendrogram", "hclust"))) {
                object@ht_list[[i_main]]@column_dend_param$obj = cluster_columns
                object@ht_list[[i_main]]@column_dend_param$cluster = TRUE
                if(verbose) qqcat("set cluster_columns to main heatmap\n")
            } else if(inherits(cluster_columns, "function")) {
                object@ht_list[[i_main]]@column_dend_param$fun = cluster_columns
                object@ht_list[[i_main]]@column_dend_param$cluster = TRUE
                if(verbose) qqcat("set cluster_columns to main heatmap\n")
            } else {
                object@ht_list[[i_main]]@column_dend_param$cluster = cluster_columns
                if(verbose) qqcat("set cluster_columns to main heatmap\n")
                if(!cluster_columns) {
                    column_dend_width = unit(0, "mm")
                    show_column_dend = FALSE
                } else {
                    column_dend_width = unit(10, "mm")
                }
            }
        }

        if(!is.null(show_column_dend)) {
            if(!show_column_dend) {
                column_dend_width = unit(0, "mm")
            }
        }
        if(!is.null(clustering_distance_columns)) {
            object@ht_list[[i_main]]@column_dend_param$distance = clustering_distance_columns
            if(verbose) qqcat("set clustering_distance_columns to main heatmap\n")
        }
        if(!is.null(clustering_method_columns)) {
            object@ht_list[[i_main]]@column_dend_param$method = clustering_method_columns
            if(verbose) qqcat("set clustering_method_columns to main heatmap\n")
        }
        if(!is.null(column_dend_width)) {
            if(column_dend_width[[1]] == 0) {
                object@ht_list[[i_main]]@column_dend_param$width = unit(0, "mm")
            } else {
                object@ht_list[[i_main]]@column_dend_param$width = column_dend_width + DENDROGRAM_PADDING  # append the gap
            }
        }
        if(!is.null(show_column_dend)) {
            object@ht_list[[i_main]]@column_dend_param$show = show_column_dend
        }
        if(!is.null(column_dend_gp)) {
            object@ht_list[[i_main]]@column_dend_param$gp = check_gp(column_dend_gp)
            if(verbose) qqcat("set column_dend_gp to main heatmap\n")
        }
        if(!is.null(column_dend_reorder)) {
            object@ht_list[[i_main]]@column_dend_param$reorder = column_dend_reorder
            if(verbose) qqcat("set column_dend_reorder to main heatmap\n")
        }
        if(!is.null(column_order)) {
            if(is.character(column_order)) {
                column_order = structure(seq_len(ncol(object@ht_list[[i_main]]@matrix)), names = colnames(object@ht_list[[i_main]]@matrix))[column_order]
            }
            object@ht_list[[i_main]]@column_order = column_order
            if(verbose) qqcat("set column_order to main heatmap\n")
        }
    }

    if(verbose) qqcat("auto adjust all heatmap/annotations by the main heatmap\n")


    ######## auto adjust ##########
    ht_main = object@ht_list[[i_main]]
    if(direction == "horizontal") {
        ht_main = make_row_cluster(ht_main)  # with pre-defined order
        if(verbose) qqcat("perform row clustering on the main heatmap\n")
    } else {
        ht_main = make_column_cluster(ht_main)  # with pre-defined order
        if(verbose) qqcat("perform column clustering on the main heatmap\n")
    }
    object@ht_list[[i_main]] = ht_main

    if(direction == "horizontal") {
        row_dend_side = match.arg(row_dend_side)[1]
        row_sub_title_side = match.arg(row_sub_title_side)[1]

        if(row_dend_side == "left" || row_sub_title_side == "left") {
            # if the first one is a HeatmapAnnotation object
            # add a heatmap with zero column so that we can put titles and dend on the most left
            if(inherits(object@ht_list[[1]], "HeatmapAnnotation")) {
                object = Heatmap(matrix(nrow = nr, ncol = 0)) + object
                ht_gap = unit.c(unit(0, "mm"), ht_gap)
                i_main = i_main + 1
                if(verbose) qqcat("add a zero-column heatmap for row dend/title on the very left\n")
            }
                
        }

        if(row_dend_side == "right" || row_sub_title_side == "right") {
            # if the last one is a HeatmapAnnotation object
            if(inherits(object@ht_list[[ length(object@ht_list) ]], "HeatmapAnnotation")) {
                object = object + Heatmap(matrix(nrow = nr, ncol = 0))
                ht_gap = unit.c(ht_gap, unit(0, "mm"))
                if(verbose) qqcat("add a zero-column heatmap for row dend/title on the very right\n")
            }
        }
    } else {
        column_dend_side = match.arg(column_dend_side)[1]
        column_sub_title_side = match.arg(column_sub_title_side)[1]

        if(column_dend_side == "top" || column_sub_title_side == "top") {
            if(inherits(object@ht_list[[1]], "HeatmapAnnotation")) {
                object = Heatmap(matrix(nrow = 0, ncol = nc)) %v% object
                ht_gap = unit.c(unit(0, "mm"), ht_gap)
                i_main = i_main + 1
                if(verbose) qqcat("add a zero-row heatmap for column dend/title on the very top\n")
            }   
        }

        if(column_dend_side == "bottom" || column_sub_title_side == "bottom") {
            if(inherits(object@ht_list[[ length(object@ht_list) ]], "HeatmapAnnotation")) {
                object = object %v% Heatmap(matrix(nrow = 0, ncol = nc))
                ht_gap = unit.c(ht_gap, unit(0, "mm"))
                if(verbose) qqcat("add a zero-column heatmap for row dend/title on the very bottom\n")
            }
        }
    }

    object@ht_list_param$main_heatmap = i_main
    object@ht_list_param$ht_gap = ht_gap
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
    if(direction == "horizontal") {
        for(i in seq_len(n_ht)) {
            if(inherits(object@ht_list[[i]], "Heatmap") & i != i_main) {
                object@ht_list[[i]]@row_order_list = ht_main@row_order_list
                object@ht_list[[i]]@row_order = ht_main@row_order
                object@ht_list[[i]]@row_dend_param$show = FALSE
                object@ht_list[[i]]@row_dend_param$cluster = FALSE  # don't do clustering because cluster was already done
            }
        }
        if(verbose) qqcat("adjust row order for all other heatmaps\n")
    } else {
        for(i in seq_len(n_ht)) {
            if(inherits(object@ht_list[[i]], "Heatmap") & i != i_main) {
                object@ht_list[[i]]@column_order_list = ht_main@column_order_list
                object@ht_list[[i]]@column_order = ht_main@column_order
                object@ht_list[[i]]@column_dend_param$show = FALSE
                object@ht_list[[i]]@column_dend_param$cluster = FALSE  # don't do clustering because cluster was already done
            }
        }
        if(verbose) qqcat("adjust column order for all other heatmaps\n")
    }
    
    if(direction == "horizontal") {
        # update other heatmaps' row titles
        for(i in seq_len(n_ht)) {
            if(inherits(object@ht_list[[i]], "Heatmap") && i != i_main) {
                object@ht_list[[i]]@row_title = character(0)
            }
        }
        if(verbose) qqcat("remove row titles for all other heatmaps\n")
    } else {
        for(i in seq_len(n_ht)) {
            if(inherits(object@ht_list[[i]], "Heatmap") && i != i_main) {
                object@ht_list[[i]]@column_title = character(0)
            }
        }
        if(verbose) qqcat("remove column titles for all other heatmaps\n")
    }

    if(direction == "horizontal") {
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
    } else {
        if(column_dend_side == "top") {
            object@ht_list[[i_main]]@column_dend_param$show = FALSE
            object@ht_list[[1]]@column_dend_list = ht_main@column_dend_list
            object@ht_list[[1]]@column_dend_param = ht_main@column_dend_param
            object@ht_list[[1]]@column_dend_param$side = "top"
            if(verbose) qqcat("add dendrogram of the main heatmap to the top of the first heatmap\n")
        } else if(column_dend_side == "bottom") {
            object@ht_list[[i_main]]@column_dend_param$show = FALSE
            object@ht_list[[n_ht]]@column_dend_list = ht_main@column_dend_list
            object@ht_list[[n_ht]]@column_dend_param = ht_main@column_dend_param
            object@ht_list[[n_ht]]@column_dend_param$side = "bottom"
            if(verbose) qqcat("add dendrogram of the main heatmap to the bottom of the last heatmap\n")
        }

        if(column_sub_title_side == "top") {
            object@ht_list[[i_main]]@column_title = character(0)
            object@ht_list[[1]]@column_title = ht_main@column_title
            object@ht_list[[1]]@column_title_param = ht_main@column_title_param
            object@ht_list[[1]]@column_title_param$side = "top"
            object@ht_list[[1]]@column_title_param$just = get_text_just(ht_main@column_title_param$rot, "top")
            if(verbose) qqcat("add column title of the main heatmap to the top of the first heatmap\n")
        } else if(column_sub_title_side == "bottom") {
            object@ht_list[[i_main]]@column_title = character(0)
            object@ht_list[[n_ht]]@column_title = ht_main@column_title
            object@ht_list[[n_ht]]@column_title_param = ht_main@column_title_param
            object@ht_list[[n_ht]]@column_title_param$side = "bottom"
            object@ht_list[[n_ht]]@column_title_param$just = get_text_just(ht_main@row_title_param$rot, "bottom")
            if(verbose) qqcat("add column title of the main heatmap to the bottom of the last heatmap\n")
        }
    }

    # gap 
    if(direction == "horizontal") {
        for(i in seq_len(n_ht)) {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                object@ht_list[[i]]@matrix_param$row_gap = ht_main@matrix_param$row_gap
            }
        }
        if(verbose) qqcat("adjust row_gap for all other heatmaps\n")
    } else {
        for(i in seq_len(n_ht)) {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                object@ht_list[[i]]@matrix_param$column_gap = ht_main@matrix_param$column_gap
            }
        }
        if(verbose) qqcat("adjust column_gap for all other heatmaps\n")
    }
    
    if(direction == "horizontal") {
        for(i in seq_len(n_ht)) {
            # supress row clustering because all rows in all heatmaps are adjusted
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(verbose) qqcat("prepare layout for heatmap: @{object@ht_list[[i]]@name}\n")
                object@ht_list[[i]] = prepare(object@ht_list[[i]], process_rows = FALSE)
            }
        }
    } else {
        for(i in seq_len(n_ht)) {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(verbose) qqcat("prepare layout for heatmap: @{object@ht_list[[i]]@name}\n")
                object@ht_list[[i]] = prepare(object@ht_list[[i]], process_columns = FALSE)
            }
        }
    }

    object = adjust_heatmap_list(object)
    object@layout$layout_index = rbind(heatmaplist = heatmap_list_layout_index("heatmap_list"))
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
            object@layout$layout_index = rbind(object@layout$layout_index, column_title_top = heatmap_list_layout_index("column_title_top"))
        } else {
            object@layout$layout_column_title_bottom_height = grobHeight(textGrob(column_title, gp = column_title_gp)) + TITLE_PADDING*2
            object@layout$layout_index = rbind(object@layout$layout_index, column_title_bottom = heatmap_list_layout_index("column_title_bottom"))
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
            object@layout$layout_index = rbind(object@layout$layout_index, row_title_left = heatmap_list_layout_index("row_title_left"))
        } else {
            object@layout$layout_row_title_right_width = grobHeight(textGrob(row_title, gp = row_title_gp)) + TITLE_PADDING*2
            object@layout$layout_index = rbind(object@layout$layout_index, row_title_right = heatmap_list_layout_index("row_title_right"))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_title(object, which = "row"))
    }

    #################################################
    ## heatmap legend to top, bottom, left and right
    # default values
    ColorMappingList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(direction == "horizontal") {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                }
                if(object@ht_list_param$merge_legends && !is.null(ht@top_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                }
                if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                    ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                }
                if(object@ht_list_param$merge_legends && !is.null(ht@bottom_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                }
                if(!is.null(ht@right_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                }
            } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
            }
        } else {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(object@ht_list_param$merge_legends && !is.null(ht@left_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                }
                if(!is.null(ht@top_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                }
                if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                    ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                }
                if(!is.null(ht@bottom_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                }
                if(object@ht_list_param$merge_legends && !is.null(ht@right_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                }
            } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
            }
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
            object@layout$layout_index = rbind(object@layout$layout_index, heatmap_legend_top = heatmap_list_layout_index("heatmap_legend_top"))
        } else if(heatmap_legend_side == "bottom") {
            object@heatmap_legend_param$padding = unit(c(0, 0, 2, 0), "mm")
            size = heatmap_legend_size(object, legend_list = heatmap_legend_list)
            object@heatmap_legend_param$size = size
            object@layout$layout_heatmap_legend_bottom_height = size[2]
            object@layout$layout_index = rbind(object@layout$layout_index, heatmap_legend_bottom = heatmap_list_layout_index("heatmap_legend_bottom"))
        } else if(heatmap_legend_side == "left") {
            object@heatmap_legend_param$padding = unit(c(0, 0, 0, 2), "mm")
            size = heatmap_legend_size(object, legend_list = heatmap_legend_list)
            object@heatmap_legend_param$size = size
            object@layout$layout_heatmap_legend_left_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, heatmap_legend_left = heatmap_list_layout_index("heatmap_legend_left"))
        } else if(heatmap_legend_side == "right") {
            object@heatmap_legend_param$padding = unit(c(0, 2, 0, 0), "mm")
            size = heatmap_legend_size(object, legend_list = heatmap_legend_list)
            object@heatmap_legend_param$size = size
            object@layout$layout_heatmap_legend_right_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, heamap_legend_right = heatmap_list_layout_index("heatmap_legend_right"))
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
            if(direction == "horizontal") {
                if(inherits(ht, "Heatmap")) {
                    if(!is.null(ht@top_annotation)) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                    }
                    if(!is.null(ht@bottom_annotation)) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                    }
                }
            } else {
                if(inherits(ht, "Heatmap")) {
                    if(!is.null(ht@left_annotation)) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                    }
                    if(!is.null(ht@right_annotation)) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                    }
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
            object@layout$layout_index = rbind(object@layout$layout_index, annotation_legend_top = heatmap_list_layout_index("annotation_legend_top"))
        } else if(annotation_legend_side == "bottom") {
            object@annotation_legend_param$padding = unit(c(0, 0, 2, 0), "mm")
            size = annotation_legend_size(object, legend_list = annotation_legend_list)
            object@annotation_legend_param$size = size
            object@layout$layout_annotation_legend_bottom_height = size[2]
            object@layout$layout_index = rbind(object@layout$layout_index, annotation_legend_bottom = heatmap_list_layout_index("annotation_legend_bottom"))
        } else if(annotation_legend_side == "left") {
            object@annotation_legend_param$padding = unit(c(0, 0, 0, 2), "mm")
            size = annotation_legend_size(object, legend_list = annotation_legend_list)
            object@annotation_legend_param$size = size
            object@layout$layout_annotation_legend_left_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, annotation_legend_left = heatmap_list_layout_index("annotation_legend_left"))
        } else if(annotation_legend_side == "right") {
            object@annotation_legend_param$padding = unit(c(0, 2, 0, 0), "mm")
            size = annotation_legend_size(object, legend_list = annotation_legend_list)
            object@annotation_legend_param$size = size
            object@layout$layout_annotation_legend_right_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, annotation_legend_right = heatmap_list_layout_index("annotation_legend_right"))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_annotation_legend(object, legend_list = annotation_legend_list))
    } else {
        object@annotation_legend_param$size = unit(c(0, 0), "null")
    }

    if(direction == "horizontal") {
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
    } else {
        main_matrix_cn = colnames(object@ht_list[[i_main]]@matrix)
        if(!is.null(main_matrix_cn)) {
            for(i in seq_len(n_ht)) {
                if(i == i_main) next
                if(inherits(object@ht_list[[i]], "Heatmap")) {
                    matrix_cn = colnames(object@ht_list[[i]]@matrix)
                    if(!is.null(matrix_cn)) {
                        # if same set but different order
                        if(setequal(main_matrix_cn, matrix_cn)) {
                            if(!identical(main_matrix_cn, matrix_cn)) {
                                warning("Column names of heatmap ", i, " is not consistent as the main heatmap (", i_main, ")", sep = "")
                            }
                        }
                    }
                }
            }
        }
    }

    object@layout$initialized = TRUE

    return(object)
})




HEATMAP_LIST_LAYOUT_COLUMN_COMPONENT = 1:7
names(HEATMAP_LIST_LAYOUT_COLUMN_COMPONENT) = c("annotation_legend_top", "heatmap_legend_top", "column_title_top",
    "heatmap_list", "column_title_bottom", "heatmap_legend_bottom", "annotation_legend_bottom")
HEATMAP_LIST_LAYOUT_ROW_COMPONENT = 1:7
names(HEATMAP_LIST_LAYOUT_ROW_COMPONENT) = c("annotation_legend_left", "heatmap_legend_left", "row_title_left", 
    "heatmap_list", "row_title_right", "heatmap_legend_right", "annotation_legend_right")

heatmap_list_layout_index = function(nm) {
    if(nm == "heatmap_list") { # heatmap_body
        ind = c(HEATMAP_LIST_LAYOUT_COLUMN_COMPONENT["heatmap_list"], HEATMAP_LIST_LAYOUT_ROW_COMPONENT["heatmap_list"])
    } else if(nm %in% names(HEATMAP_LIST_LAYOUT_COLUMN_COMPONENT)) {
        ind = c(HEATMAP_LIST_LAYOUT_COLUMN_COMPONENT[nm], HEATMAP_LIST_LAYOUT_ROW_COMPONENT["heatmap_list"])
    } else if(nm %in% names(HEATMAP_LIST_LAYOUT_ROW_COMPONENT)) {
        ind = c(HEATMAP_LIST_LAYOUT_COLUMN_COMPONENT["heatmap_list"], HEATMAP_LIST_LAYOUT_ROW_COMPONENT[nm])
    }
    names(ind) = c("layout.pos.row", "layout.pos.col")
    return(ind)
}

has_heatmap_list_component = function(object, component) {
    m = object@layout$layout_index
    ind = heatmap_list_layout_index(component)
    any(m[, 1] == ind[1] & m[, 2] == ind[2])
}


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
    definition = function(object, k = HEATMAP_LIST_LAYOUT_ROW_COMPONENT) {

    if(is.numeric(k)) {
        component_name = names(HEATMAP_LIST_LAYOUT_ROW_COMPONENT)[k]
    } else {
        component_name = k
    }
    # this function is used for grid.layout, so null unit is allowed
    .single_unit = function(nm) {
        if(nm == "heatmap_list") {
             width = sum(do.call("unit.c", lapply(object@ht_list, function(ht) {
                    if(inherits(ht, "Heatmap")) {
                        ht@heatmap_param$width
                    } else {
                        size(ht)
                    }
                })))
            if(is_abs_unit(width)) {
                width + sum(object@ht_list_param$ht_gap)
            } else {
                unit(1, "null") 
            }
        } else {
            object@layout[[paste0("layout_", nm, "_width")]]
        }
    }
    
    do.call("unit.c", lapply(component_name, .single_unit))
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
    definition = function(object, k = HEATMAP_LIST_LAYOUT_COLUMN_COMPONENT) {

    if(is.numeric(k)) {
        component_name = names(HEATMAP_LIST_LAYOUT_COLUMN_COMPONENT)[k]
    } else {
        component_name = k
    }
    # this function is used for grid.layout, so null unit is allowed
    .single_unit = function(nm) {
        if(nm == "heatmap_list") {
            height = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
                    if(inherits(ht, "Heatmap")) {
                        ht@heatmap_param$height
                    } else {
                        size(ht)
                    }
                })))
            if(is_abs_unit(height)) {
                height
            } else {
                unit(1, "null") 
            }
        } else {
            object@layout[[paste0("layout_", nm, "_height")]]
        }
    }

    do.call("unit.c", lapply(component_name, .single_unit))
})
