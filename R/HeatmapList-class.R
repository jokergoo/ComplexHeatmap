
# == title
# Class for a list of heatmaps
#
# == details
# A heatmap list is defined as a list of heatmaps and annotations.
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
        direction = "character",

        row_title = "ANY",
        row_title_param = "list",
        column_title = "ANY",
        column_title_param = "list",

        annotation_legend_param = "list",
        heatmap_legend_param = "list",

        layout = "list"
    ),
    prototype = list(
        direction = "horizontal",

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
# -direction direction of the concatenation.
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
    definition = function(object, x, direction = c("horizontal", "vertical")) {
    
    direction = match.arg(direction)[1]
    if(object@direction != direction) {
        stop_wrap("The heatmap list should only be all horizontal or vertical. Maybe you can move some of the row annotations to the 'left_annotation'/'right_annotation' of the heatmap if you want to make the heatmap list vertical. Or similarly, to move independent column annotations to 'top_annotation'/'bottom_annotation' if you want the heatmap list horizontal.")
    }

    if(object@layout$initialized) {
        stop_wrap("The heatmap object should not be processed by draw().")
    }

    # check settings of this new heatmap
    if(inherits(x, "Heatmap")) {
        ht_name = x@name
        x = list(x)
        names(x) = ht_name
        object@ht_list = c(object@ht_list, x)
    } else if(inherits(x, "HeatmapAnnotation")) {
        if(x@which == "row" && direction == "vertical") {
            stop_wrap("Row annotations should be added to the heatmap list in horizontal direction.")
        } else if(x@which == "column" && direction == "horizontal") {
            stop_wrap("Column annotations should be added to the heatmap list in vertical direction.")
        }
        ht_name = x@name
        x = list(x)
        names(x) = ht_name
        object@ht_list = c(object@ht_list, x)
        
    } else if(inherits(x, "HeatmapList")) {
        if(x@layout$initialized) {
            stop_wrap("The heatmap object should not be processed by draw().")
        }
        ht_name = names(x@ht_list)
        object@ht_list = c(object@ht_list, x@ht_list)
    }

    ht_name = names(object@ht_list)
    which_duplicated = duplicated(ht_name)
    if(any(which_duplicated)) {
        warning_wrap(paste0("Heatmap/annotation names are duplicated: ", paste(ht_name[which_duplicated], collapse = ", ")))
    }

    if(direction == "horizontal") {
        nr = sapply(object@ht_list, function(ht) {
            if(inherits(ht, "Heatmap")) {
                nrow(ht@matrix)
            } else if(inherits(ht, "HeatmapAnnotation")) {
                nobs(ht)
            } else {
                NA
            }
        })
        nr = nr[!is.na(nr)]

        if(length(unique(nr)) > 1) {
            msg = "`nrow` of all heatmaps and `nobs` of all annotations should be the same\nfor horizontal heatmap list."
            for(i in seq_along(object@ht_list)) {
                if(inherits(object@ht_list[[i]], "Heatmap")) {
                    msg = c(msg, paste("  heatmap '", object@ht_list[[i]]@name, "': ", nrow(object@ht_list[[i]]@matrix), sep = ""))
                } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                    ha = object@ht_list[[i]]
                    for(j in seq_along(ha@anno_list)) {
                        if(!is.na(nobs(ha@anno_list[[j]]))) {
                            msg = c(msg, paste("  annotation '", ha@anno_list[[j]]@name, "': ", nobs(ha@anno_list[[j]]), sep = ""))
                        }
                    }
                }
            }
            stop(paste(msg, collapse = "\n"), call. = FALSE)
        }
    } else {
        nc = sapply(object@ht_list, function(ht) {
            if(inherits(ht, "Heatmap")) {
                ncol(ht@matrix)
            } else if(inherits(ht, "HeatmapAnnotation")) {
                nobs(ht)
            } else {
                NA
            }
        })
        nc = nc[!is.na(nc)]

        if(length(unique(nc)) > 1) {
            msg = "`ncol` of all heatmaps and `nobs` of all annotations should be the same\nfor vertical heatmap list."
            for(i in seq_along(object@ht_list)) {
                if(inherits(object@ht_list[[i]], "Heatmap")) {
                    msg = c(msg, paste("  heatmap '", object@ht_list[[i]]@name, "': ", ncol(object@ht_list[[i]]@matrix), sep = ""))
                } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                    ha = object@ht_list[[i]]
                    for(j in seq_along(ha@anno_list)) {
                        if(!is.na(nobs(ha@anno_list[[j]]))) {
                            msg = c(msg, paste("  annotation '", ha@anno_list[[j]]@name, "': ", nobs(ha@anno_list[[j]]), sep = ""))
                        }
                    }
                }
            }
            stop(paste(msg, collapse = "\n"), call. = FALSE)
        }
    }

    return(object)
})


# == title
# Draw a list of heatmaps
#
# == param
# -object a `HeatmapList-class` object.
# -newpage whether create a new page for the graphics. If you want to arrange multiple plots in one page, I suggest to use `grid::grid.grabExpr`.
# -background Background color of the whole plot.
# -row_title title on the row.
# -row_title_side will the title be put on the left or right of the heatmap.
# -row_title_gp graphic parameters for drawing text.
# -column_title title on the column.
# -column_title_side will the title be put on the top or bottom of the heatmap.
# -column_title_gp graphic parameters for drawing text.
# -heatmap_legend_side side to put heatmap legend
# -merge_legends merge heatmap legends and annotation legends to put into one column.
# -show_heatmap_legend whether show all heatmap legends
# -heatmap_legend_list use-defined legends which are put after the heatmap legends
# -annotation_legend_side side of the annotation legends
# -show_annotation_legend whether show annotation legends
# -annotation_legend_list user-defined legends which are put after the annotation legends
# -align_heatmap_legend How to align the legends to heatmap. Possible values are "heatmap_center", "heatmap_top" and "global_center". If the value is ``NULL``,
#           it automatically picks the proper value from the three options.
# -align_annotation_legend  How to align the legends to heatmap. Possible values are "heatmap_center", "heatmap_top" and "global_center".
# -legend_grouping How the legends are grouped. Values should be "adjusted" or "original". If it is set as
#             "original", all annotation legends are grouped together.
# -gap gap between heatmaps/annotations
# -ht_gap same as ``gap``.
# -main_heatmap index of main heatmap. The value can be a numeric index or the heatmap name
# -padding padding of the whole plot. The value is a unit vector of length 4, which corresponds to bottom, left, top and right.
# -adjust_annotation_extension whether take annotation name into account when calculating positions of graphic elements.
# -auto_adjust whether apply automatic adjustment? The auto-adjustment includes turning off dendrograms, titles and row/columns for non-main heatmaps.
# -row_dend_side side of the dendrogram from the main heatmap
# -row_sub_title_side side of the row title from the main heatmap
# -column_dend_side side of the dendrogram from the main heatmap
# -column_sub_title_side side of the column title from the main heatmap
# -row_gap this modifies ``row_gap`` of the main heatmap
# -cluster_rows this modifies ``cluster_rows`` of the main heatmap
# -cluster_row_slices this modifies ``cluster_row_slices`` of the main heatmap
# -clustering_distance_rows this modifies ``clustering_distance_rows`` of the main heatmap
# -clustering_method_rows this modifies ``clustering_method_rows`` of the main heatmap
# -row_dend_width this modifies ``row_dend_width`` of the main heatmap
# -show_row_dend this modifies ``show_row_dend`` of the main heatmap
# -row_dend_reorder this modifies ``row_dend_reorder`` of the main heatmap
# -row_dend_gp this modifies ``row_dend_gp`` of the main heatmap
# -row_order this modifies ``row_order`` of the main heatmap
# -km = this modifies ``km`` of the main heatmap
# -split this modifies ``split`` of the main heatmap
# -row_km this modifies ``row_km`` of the main heatmap
# -row_km_repeats this modifies ``row_km_repeats`` of the main heatmap
# -row_split this modifies ``row_split`` of the main heatmap
# -height this modifies ``height`` of the main heatmap
# -heatmap_height this modifies ``heatmap_height`` of the main heatmap
# -column_gap this modifies ``column_gap`` of the main heatmap
# -cluster_columns this modifies ``cluster_columns`` of the main heatmap
# -cluster_column_slices this modifies ``cluster_column_slices`` of the main heatmap
# -clustering_distance_columns this modifies ``clustering_distance_columns`` of the main heatmap
# -clustering_method_columns this modifies ``clustering_method_columns`` of the main heatmap
# -column_dend_width this modifies ``column_dend_width`` of the main heatmap
# -show_column_dend this modifies ``show_column_dend`` of the main heatmap
# -column_dend_reorder this modifies ``column_dend_reorder`` of the main heatmap
# -column_dend_gp this modifies ``column_dend_gp`` of the main heatmap
# -column_order this modifies ``column_order`` of the main heatmap
# -column_km this modifies ``column_km`` of the main heatmap
# -column_km_repeats this modifies ``column_km_repeats`` of the main heatmap
# -column_split this modifies ``column_split`` of the main heatmap
# -width this modifies ``width`` of the main heatmap
# -heatmap_width this modifies ``heatmap_width`` of the main heatmap
# -use_raster this modifies ``use_raster`` of every heatmap.
# -raster_device this modifies ``raster_device`` of every heatmap.
# -raster_quality this modifies ``raster_quality`` of every heatmap.
# -raster_device_param this modifies ``raster_device_param`` of every heatmap.
# -raster_resize this modifies ``raster_resize`` of every heatmap.
# -post_fun A self-defined function will be executed after all the heatmaps are drawn.
# -save_last Whether to save the last plot?
# -heatmap_row_names_gp  this set the value in `ht_opt` and reset back after the plot is done
# -heatmap_column_names_gp this set the value in `ht_opt` and reset back after the plot is done
# -heatmap_row_title_gp this set the value in `ht_opt` and reset back after the plot is done
# -heatmap_column_title_gp this set the value in `ht_opt` and reset back after the plot is done
# -legend_title_gp this set the value in `ht_opt` and reset back after the plot is done
# -legend_title_position this set the value in `ht_opt` and reset back after the plot is done
# -legend_labels_gp this set the value in `ht_opt` and reset back after the plot is done
# -legend_grid_height this set the value in `ht_opt` and reset back after the plot is done
# -legend_grid_width  this set the value in `ht_opt` and reset back after the plot is done
# -legend_border this set the value in `ht_opt` and reset back after the plot is done
# -legend_gap Gap between legends. The value should be a vector of two units. One for gaps between
#         vertical legends and one for the horizontal legends. If only one single unit is specified,
#         the same gap set for the vertical and horizontal legends. 
# -heatmap_border this set the value in `ht_opt` and reset back after the plot is done
# -annotation_border  this set the value in `ht_opt` and reset back after the plot is done
# -fastcluster this set the value in `ht_opt` and reset back after the plot is done
# -simple_anno_size  this set the value in `ht_opt` and reset back after the plot is done
# -show_parent_dend_line this set the value in `ht_opt` and reset back after the plot is done
#
# == detail
# The function first calls `make_layout,HeatmapList-method` to calculate
# the layout of the heatmap list and the layout of every single heatmap,
# then makes the plot by re-calling the graphic functions which are already recorded
# in the layout.
#
# == seealso
# https://jokergoo.github.io/ComplexHeatmap-reference/book/a-list-of-heatmaps.html
#
# == value
# This function returns a `HeatmapList-class` object for which the layout has been created.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw",
    signature = "HeatmapList",
    definition = function(object, 
    newpage = TRUE,
    background = "white",

    row_title = character(0), 
    row_title_side = c("left", "right"), 
    row_title_gp = gpar(fontsize = 13),
    column_title = character(0), 
    column_title_side = c("top", "bottom"), 
    column_title_gp = gpar(fontsize = 13), 

    heatmap_legend_side = c("right", "left", "bottom", "top"), 
    merge_legends = ht_opt$merge_legends,
    show_heatmap_legend = TRUE, 
    heatmap_legend_list = list(),
    annotation_legend_side = c("right", "left", "bottom", "top"), 
    show_annotation_legend = TRUE, 
    annotation_legend_list = list(),
    align_heatmap_legend = NULL,
    align_annotation_legend = NULL,
    legend_grouping = c("adjusted", "original"),

    gap = unit(2, "mm"), 
    ht_gap = gap, 

    main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
    padding = GLOBAL_PADDING,
    adjust_annotation_extension = NULL,
    
    auto_adjust = TRUE,
    row_dend_side = c("original", "left", "right"),
    row_sub_title_side = c("original", "left", "right"),
    column_dend_side = c("original", "top", "bottom"),
    column_sub_title_side = c("original", "top", "bottom"),
    
    row_gap = NULL,
    cluster_rows = NULL,
    cluster_row_slices = NULL,
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
    row_km_repeats = NULL,
    row_split = split,
    height = NULL,
    heatmap_height = NULL,

    column_gap = NULL,
    cluster_columns = NULL,
    cluster_column_slices = NULL,
    clustering_distance_columns = NULL,
    clustering_method_columns = NULL,
    column_dend_width = NULL, 
    show_column_dend = NULL, 
    column_dend_reorder = NULL,
    column_dend_gp = NULL,
    column_order = NULL,
    column_km = NULL,
    column_km_repeats = NULL,
    column_split = NULL,
    width = NULL,
    heatmap_width = NULL,

    use_raster = NULL, 
    raster_device = NULL,
    raster_quality = NULL,
    raster_device_param = NULL,
    raster_resize = NULL,

    post_fun = NULL,
    save_last = ht_opt$save_last,

    ### global setting
    heatmap_row_names_gp = NULL,
    heatmap_column_names_gp = NULL,
    heatmap_row_title_gp = NULL,
    heatmap_column_title_gp = NULL,
    legend_title_gp = NULL,
    legend_title_position = NULL,
    legend_labels_gp = NULL,
    legend_grid_height = NULL,
    legend_grid_width = NULL,
    legend_border = NULL,
    legend_gap = NULL,
    heatmap_border = NULL,
    annotation_border = NULL,
    fastcluster = NULL,
    simple_anno_size = NULL,
    show_parent_dend_line = NULL
    ) {

    .ENV$IS_UNDER_JUPYTER = is_under_jupyter()

    if(.ENV$IS_UNDER_JUPYTER && !.ENV$IS_UNDER_JUPYTER_IGNORE) {
        .ENV$IS_UNDER_JUPYTER_IGNORE = TRUE
        on.exit(.ENV$IS_UNDER_JUPYTER_IGNORE <- FALSE)
        p = grid.grabExpr({
            object = draw(object,
                newpage = newpage,
                background = background,

                row_title = row_title, 
                row_title_side = row_title_side, 
                row_title_gp = row_title_gp,
                column_title = column_title, 
                column_title_side = column_title_side, 
                column_title_gp = column_title_gp, 

                heatmap_legend_side = heatmap_legend_side, 
                merge_legends = merge_legends,
                show_heatmap_legend = show_heatmap_legend, 
                heatmap_legend_list = heatmap_legend_list,
                annotation_legend_side = annotation_legend_side, 
                show_annotation_legend = show_annotation_legend, 
                annotation_legend_list = annotation_legend_list,
                align_heatmap_legend = align_heatmap_legend,
                align_annotation_legend = align_annotation_legend,
                legend_grouping = legend_grouping,

                gap = gap, 
                ht_gap = ht_gap, 

                main_heatmap = main_heatmap,
                padding = padding,
                adjust_annotation_extension = adjust_annotation_extension,
                
                auto_adjust = auto_adjust,
                row_dend_side = row_dend_side,
                row_sub_title_side = row_sub_title_side,
                column_dend_side = column_dend_side,
                column_sub_title_side = column_sub_title_side,
                
                row_gap = row_gap,
                cluster_rows = cluster_rows,
                cluster_row_slices = cluster_row_slices,
                clustering_distance_rows = clustering_distance_rows,
                clustering_method_rows = clustering_method_rows,
                row_dend_width = row_dend_width, 
                show_row_dend = show_row_dend, 
                row_dend_reorder = row_dend_reorder,
                row_dend_gp = row_dend_gp,
                row_order = row_order,
                km = km,
                split = split,
                row_km = row_km,
                row_km_repeats = row_km_repeats,
                row_split = row_split,
                height = height,
                heatmap_height = heatmap_height,

                column_gap = column_gap,
                cluster_columns = cluster_columns,
                cluster_column_slices = cluster_column_slices,
                clustering_distance_columns = clustering_distance_columns,
                clustering_method_columns = clustering_method_columns,
                column_dend_width = column_dend_width, 
                show_column_dend = show_column_dend, 
                column_dend_reorder = column_dend_reorder,
                column_dend_gp = column_dend_gp,
                column_order = column_order,
                column_km = column_km,
                column_km_repeats = column_km_repeats,
                column_split = column_split,
                width = width,
                heatmap_width = heatmap_width,

                use_raster = use_raster, 
                raster_device = raster_device,
                raster_quality = raster_quality,
                raster_device_param = raster_device_param,
                raster_resize = raster_resize,

                post_fun = post_fun,
                save_last = save_last,

                ### global setting
                heatmap_row_names_gp = heatmap_row_names_gp,
                heatmap_column_names_gp = heatmap_column_names_gp,
                heatmap_row_title_gp = heatmap_row_title_gp,
                heatmap_column_title_gp = heatmap_column_title_gp,
                legend_title_gp = legend_title_gp,
                legend_title_position = legend_title_position,
                legend_labels_gp = legend_labels_gp,
                legend_grid_height = legend_grid_height,
                legend_grid_width = legend_grid_width,
                legend_border = legend_border,
                legend_gap = legend_gap,
                heatmap_border = heatmap_border,
                annotation_border = annotation_border,
                fastcluster = fastcluster,
                simple_anno_size = simple_anno_size,
                show_parent_dend_line = show_parent_dend_line
            )
        }, width = getOption("repr.plot.width"), height = getOption("repr.plot.height"))
        grid.draw(p)

        return(invisible(object))
    }

    verbose = ht_opt$verbose

    if(missing(cluster_rows) && !missing(row_order)) {
        cluster_rows = FALSE
    }
    if(missing(cluster_columns) && !missing(column_order)) {
        cluster_columns = FALSE
    }

    ovl = list()
    for(opt_nm in c("heatmap_row_names_gp",
                    "heatmap_column_names_gp",
                    "heatmap_row_title_gp",
                    "heatmap_column_title_gp",
                    "legend_title_gp",
                    "legend_title_position",
                    "legend_labels_gp",
                    "legend_grid_height",
                    "legend_grid_width",
                    "legend_border",
                    "legend_gap",
                    "heatmap_border",
                    "annotation_border",
                    "fastcluster",
                    "simple_anno_size",
                    "show_parent_dend_line")) {
        v = get(opt_nm, inherits = FALSE)
        if(!is.null(v)) {
            ovl[[opt_nm]] = ht_opt[[opt_nm]]
            ht_opt[[opt_nm]] = v

            if(verbose) qqcat("temporarily set the global parameter @{opt_nm}\n")
        }
    }
    if(length(ovl)) {
        on.exit(ht_opt(ovl))
    }

    direction = object@direction

    if(!has_heatmap(object)) {
        ob = sapply(object@ht_list, nobs)
        ob = ob[!is.na(ob)]
        if(length(ob) == 0) {
            stop_wrap("There is no heatmap in the list and cannot infer the number of observations in the heatmap annotations, please add a zero row/column matrix by hand.")
        }
        if(direction == "horizontal") {
            nr = ob[1]
            max_height = max(do.call("unit.c", lapply(object@ht_list, function(ha) {
                h = height(ha)
                if(is_abs_unit(h)) {
                    convertHeight(h, "mm")
                } else {
                    unit(0, "mm")
                }
            })))
            max_height = convertHeight(max_height, "mm")
            if(unit_to_numeric(max_height[1]) == 0) {
                object = object + Heatmap(matrix(ncol = 0, nrow = nr), row_title = NULL, show_heatmap_legend = FALSE)
            } else {
                object = object + Heatmap(matrix(ncol = 0, nrow = nr), height = max_height, row_title = NULL, show_heatmap_legend = FALSE)
            }
            
        } else {
            nc = ob[1]
            max_width = max(do.call("unit.c", lapply(object@ht_list, function(ha) {
                w = width(ha)
                if(is_abs_unit(w)) {
                    convertWidth(w, "mm")
                } else {
                    unit(0, "mm")
                }
            })))
            max_width = convertWidth(max_width, "mm")
            if(unit_to_numeric(max_width[1]) == 0) {
                object = object %v% Heatmap(matrix(nrow = 0, ncol = nc), column_title = NULL, show_heatmap_legend = FALSE)
            } else {
                object = object %v% Heatmap(matrix(nrow = 0, ncol = nc), width = max_width, column_title = NULL, show_heatmap_legend = FALSE)
            }
        }
        row_sub_title_side = "original"
        row_dend_side = "original"
        column_sub_title_side = "original"
        column_dend_side = "original"
    }

    if(object@layout$initialized) {
 
        if(missing(padding)) padding = object@ht_list_param$padding
        if(missing(ht_gap) && missing(gap)) ht_gap = object@ht_list_param$ht_gap
        if(missing(main_heatmap)) main_heatmap = object@ht_list_param$main_heatmap
        if(missing(merge_legends)) merge_legends = object@ht_list_param$merge_legends
        if(missing(auto_adjust)) auto_adjust = object@ht_list_param$auto_adjust
        if(missing(legend_grouping)) legend_grouping = object@ht_list_param$legend_grouping
        if(missing(post_fun)) post_fun = object@ht_list_param$post_fun

        if(missing(heatmap_legend_side)) heatmap_legend_side = object@heatmap_legend_param$side
        if(missing(show_heatmap_legend)) show_heatmap_legend = object@heatmap_legend_param$show
        if(missing(annotation_legend_side)) annotation_legend_side = object@annotation_legend_param$side
        if(missing(show_annotation_legend)) show_annotation_legend = object@annotation_legend_param$show

        if(missing(row_title)) row_title = object@row_title
        if(missing(row_title_side)) row_title_side = object@row_title_param$side
        if(missing(row_title_gp)) row_title_gp = object@row_title_param$gp
        if(missing(column_title)) column_title = object@column_title
        if(missing(column_title_side)) column_title_side = object@column_title_param$side
        if(missing(column_title_gp)) column_title_gp = object@column_title_param$gp

        if(missing(background)) {
            if(!is.null(object@ht_list_param$background)) {
                background = object@ht_list_param$background
            }
        }
    }

    if(newpage) {
        grid.newpage()
    }

    heatmap_legend_side = match.arg(heatmap_legend_side)
    annotation_legend_side = match.arg(annotation_legend_side)
    if(heatmap_legend_side == annotation_legend_side) {
        if(!is.null(align_heatmap_legend) && is.null(align_annotation_legend)) {
            align_annotation_legend = align_heatmap_legend
        } else if(is.null(align_heatmap_legend) && !is.null(align_annotation_legend)) {
            align_heatmap_legend = align_heatmap_legend
        }
    }
    object@ht_list_param$adjust_annotation_extension = adjust_annotation_extension
    object@ht_list_param$post_fun = post_fun
    object@ht_list_param$background = background

    object = make_layout(
        object, 
        row_title = row_title, 
        row_title_side = row_title_side, 
        row_title_gp = row_title_gp,
        column_title = column_title, 
        column_title_side = column_title_side, 
        column_title_gp = column_title_gp, 

        heatmap_legend_side = heatmap_legend_side, 
        merge_legends = merge_legends,
        show_heatmap_legend = show_heatmap_legend, 
        heatmap_legend_list = heatmap_legend_list,
        annotation_legend_side = annotation_legend_side, 
        show_annotation_legend = show_annotation_legend, 
        annotation_legend_list = annotation_legend_list,
        align_heatmap_legend = align_heatmap_legend,
        align_annotation_legend = align_annotation_legend,
        legend_grouping = legend_grouping,

        ht_gap = ht_gap, 

        main_heatmap = main_heatmap,
        padding = padding,

        auto_adjust = auto_adjust,
        row_dend_side = row_dend_side,
        row_sub_title_side = row_sub_title_side,
        column_dend_side = column_dend_side,
        column_sub_title_side = column_sub_title_side,
        
        row_gap = row_gap,
        cluster_rows = cluster_rows,
        cluster_row_slices = cluster_row_slices,
        clustering_distance_rows = clustering_distance_rows,
        clustering_method_rows = clustering_method_rows,
        row_dend_width = row_dend_width, 
        show_row_dend = show_row_dend, 
        row_dend_reorder = row_dend_reorder,
        row_dend_gp = row_dend_gp,
        row_order = row_order,
        row_km = row_km,
        row_km_repeats = row_km_repeats,
        row_split = row_split,
        height = height,
        heatmap_height = heatmap_height,

        column_gap = column_gap,
        cluster_columns = cluster_columns,
        cluster_column_slices = cluster_column_slices,
        clustering_distance_columns = clustering_distance_columns,
        clustering_method_columns = clustering_method_columns,
        column_dend_width = column_dend_width, 
        show_column_dend = show_column_dend, 
        column_dend_reorder = column_dend_reorder,
        column_dend_gp = column_dend_gp,
        column_order = column_order,
        column_km = column_km,
        column_km_repeats = column_km_repeats,
        column_split = column_split,
        width = width,
        heatmap_width = heatmap_width,

        use_raster = use_raster, 
        raster_device = raster_device,
        raster_quality = raster_quality,
        raster_device_param = raster_device_param,
        raster_resize = raster_resize
    )

    padding = object@ht_list_param$padding

    code = expression({

        current_vp = current.viewport()$name
        if(current_vp == "ROOT") {
            page_size = unit(par("din"), "in")
        } else {
            grid::upViewport()
            page_size = unit.c(convertWidth(unit(1, "npc"), "mm"),
                                             convertHeight(unit(1, "npc"), "mm"))
            grid::downViewport(current_vp)
        }

        # adjust legend, the following have 99% duplication as in HeatmapList-layout.R
        # however, it runs fast, so I just make it ugly

        #################################################
        ## heatmap legend to top, bottom, left and right
        # default values
        legend_grouping = object@ht_list_param$legend_grouping
        ColorMappingList = list()
        for(i in seq_along(object@ht_list)) {
            ht = object@ht_list[[i]]
            if(object@direction == "horizontal") {
                if(inherits(object@ht_list[[i]], "Heatmap")) {
                    if(!is.null(ht@left_annotation)) {
                        if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                        }
                    }
                    if(!is.null(ht@top_annotation)) {
                        if(object@ht_list_param$merge_legends) {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                        }
                    }
                    if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                        ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                    }
                    if(!is.null(ht@bottom_annotation)) {
                        if(object@ht_list_param$merge_legends) {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                        }
                    }
                    if(!is.null(ht@right_annotation)) {
                        if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                        }
                    }
                } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                    if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
                    }
                }
            } else {
                if(inherits(object@ht_list[[i]], "Heatmap")) {
                    if(!is.null(ht@left_annotation)) {
                        if(object@ht_list_param$merge_legends) {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                        }
                    }
                    if(!is.null(ht@top_annotation)) {
                        if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                        }
                    }
                    if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                        ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                    }
                    if(!is.null(ht@bottom_annotation)) {
                        if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                        }
                    }
                    if(!is.null(ht@right_annotation)) {
                        if(object@ht_list_param$merge_legends) {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                        }
                    }
                } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                    if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
                    }
                }
            }
        }
        heatmap_legend_list = object@heatmap_legend_param$list
        annotation_legend_list = object@annotation_legend_param$list
        if(length(heatmap_legend_list) != 0) {
            if(inherits(heatmap_legend_list, c("Legends", "grob"))) {
                heatmap_legend_list = list(heatmap_legend_list)
            }
        }
        if(length(annotation_legend_list) != 0) {
            if(inherits(annotation_legend_list, c("Legends", "grob"))) {
                annotation_legend_list = list(annotation_legend_list)
            }
        }
        if(object@ht_list_param$merge_legends) {
            heatmap_legend_list = c(heatmap_legend_list, annotation_legend_list)
        }
        if(length(ColorMappingList) == 0 && length(heatmap_legend_list) == 0) {
            show_heatmap_legend = FALSE
        }

        object@heatmap_legend_param$show = show_heatmap_legend
        heatmap_legend_side = object@heatmap_legend_param$side
        if(show_heatmap_legend) {
            if(heatmap_legend_side == "top") {
                object@heatmap_legend_param$padding = unit.c(ht_opt$HEATMAP_LEGEND_PADDING, unit(c(0, 0, 0), "mm")) #unit(c(2, 0, 0, 0), "mm")
                size = heatmap_legend_size(object, legend_list = heatmap_legend_list, max_width = calc_legends_max_width(object, page_size))
                object@heatmap_legend_param$size = size
                object@layout$layout_heatmap_legend_top_height = size[2]
                layout_index = heatmap_list_layout_index("heatmap_legend_top")
            } else if(heatmap_legend_side == "bottom") {
                object@heatmap_legend_param$padding = unit.c(unit(c(0, 0), "mm"), ht_opt$HEATMAP_LEGEND_PADDING, unit(0, "mm")) # unit(c(0, 0, 2, 0), "mm")
                size = heatmap_legend_size(object, legend_list = heatmap_legend_list, max_width = calc_legends_max_width(object, page_size))
                object@heatmap_legend_param$size = size
                object@layout$layout_heatmap_legend_bottom_height = size[2]
                layout_index = heatmap_list_layout_index("heatmap_legend_bottom")
            } else if(heatmap_legend_side == "left") {
                object@heatmap_legend_param$padding = unit.c(unit(c(0, 0, 0), "mm"), ht_opt$HEATMAP_LEGEND_PADDING) # unit(c(0, 0, 0, 2), "mm")
                size = heatmap_legend_size(object, legend_list = heatmap_legend_list, max_height = calc_legends_max_height(object, page_size))
                object@heatmap_legend_param$size = size
                object@layout$layout_heatmap_legend_left_width = size[1]
                layout_index = heatmap_list_layout_index("heatmap_legend_left")
            } else if(heatmap_legend_side == "right") {
                object@heatmap_legend_param$padding = unit.c(unit(0, "mm"), ht_opt$HEATMAP_LEGEND_PADDING, unit(c(0, 0), "mm")) # unit(c(0, 2, 0, 0), "mm")
                size = heatmap_legend_size(object, legend_list = heatmap_legend_list, max_height = calc_legends_max_height(object, page_size))
                object@heatmap_legend_param$size = size
                object@layout$layout_heatmap_legend_right_width = size[1]
                layout_index = heatmap_list_layout_index("heatmap_legend_right")
            }
            if(heatmap_legend_side %in% c("top", "bottom")) {
                ind = which(object@layout$layout_index[, 1] == layout_index[1] & object@layout$layout_index[, 2] == layout_index[2])
                object@layout$graphic_fun_list[[ind]] = function(object) draw_heatmap_legend(object, legend_list = heatmap_legend_list, max_width = calc_legends_max_width(object, page_size))
            } else {
                ind = which(object@layout$layout_index[, 1] == layout_index[1] & object@layout$layout_index[, 2] == layout_index[2])
                object@layout$graphic_fun_list[[ind]] = function(object) draw_heatmap_legend(object, legend_list = heatmap_legend_list, max_height = calc_legends_max_height(object, page_size))
            }
        } else {
            object@heatmap_legend_param$size = unit(c(0, 0), "mm")
        }

        #################################################
        ## annotation legend to top, bottom, left and right
        # default values
        ColorMappingList = list()
        if(!object@ht_list_param$merge_legends) {
            for(i in seq_along(object@ht_list)) {
                ht = object@ht_list[[i]]
                if(object@direction == "horizontal") {
                    if(inherits(ht, "Heatmap")) {
                        if(!is.null(ht@left_annotation)) {
                            if(legend_grouping == "original") {
                                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                            }
                        }
                        if(!is.null(ht@top_annotation)) {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                        }
                        if(!is.null(ht@bottom_annotation)) {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                        }
                        if(!is.null(ht@right_annotation)) {
                            if(legend_grouping == "original") {
                                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                            }
                        }
                    } else if(inherits(ht, "HeatmapAnnotation")) {
                        if(legend_grouping == "original") {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht))
                        }
                    }
                } else {
                    if(inherits(ht, "Heatmap")) {
                        if(!is.null(ht@left_annotation)) {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                        }
                        if(!is.null(ht@top_annotation)) {
                            if(legend_grouping == "original") {
                                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                            }
                        }
                        if(!is.null(ht@bottom_annotation)) {
                            if(legend_grouping == "original") {
                                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                            }
                        }
                        if(!is.null(ht@right_annotation)) {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                        }
                    } else if(inherits(ht, "HeatmapAnnotation")) {
                        if(legend_grouping == "original") {
                            ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht))
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
        annotation_legend_side = object@annotation_legend_param$side
        if(show_annotation_legend) {
            if(annotation_legend_side == "top") {
                object@annotation_legend_param$padding = unit.c(ht_opt$ANNOTATION_LEGEND_PADDING, unit(c(0, 0, 0), "mm")) # unit(c(2, 0, 0, 0), "mm")
                size = annotation_legend_size(object, legend_list = annotation_legend_list, max_width = calc_legends_max_width(object, page_size))
                object@annotation_legend_param$size = size
                object@layout$layout_annotation_legend_top_height = size[2]
                layout_index = heatmap_list_layout_index("annotation_legend_top")
            } else if(annotation_legend_side == "bottom") {
                object@annotation_legend_param$padding = unit.c(unit(c(0, 0), "mm"), ht_opt$ANNOTATION_LEGEND_PADDING, unit(0, "mm")) # unit(c(0, 0, 2, 0), "mm")
                size = annotation_legend_size(object, legend_list = annotation_legend_list, max_width = calc_legends_max_width(object, page_size))
                object@annotation_legend_param$size = size
                object@layout$layout_annotation_legend_bottom_height = size[2]
                layout_index = heatmap_list_layout_index("annotation_legend_bottom")
            } else if(annotation_legend_side == "left") {
                object@annotation_legend_param$padding = unit.c(unit(c(0, 0, 0), "mm"), ht_opt$ANNOTATION_LEGEND_PADDING) # unit(c(0, 0, 0, 2), "mm")
                size = annotation_legend_size(object, legend_list = annotation_legend_list, max_height = calc_legends_max_height(object, page_size))
                object@annotation_legend_param$size = size
                object@layout$layout_annotation_legend_left_width = size[1]
                layout_index = heatmap_list_layout_index("annotation_legend_left")
            } else if(annotation_legend_side == "right") {
                object@annotation_legend_param$padding = unit.c(unit(0, "mm"), ht_opt$ANNOTATION_LEGEND_PADDING, unit(c(0, 0), "mm")) # unit(c(0, 2, 0, 0), "mm")
                size = annotation_legend_size(object, legend_list = annotation_legend_list, max_height = calc_legends_max_height(object, page_size))
                object@annotation_legend_param$size = size
                object@layout$layout_annotation_legend_right_width = size[1]
                layout_index = heatmap_list_layout_index("annotation_legend_right")
            }
            if(annotation_legend_side %in% c("top", "bottom")) {
                ind = which(object@layout$layout_index[, 1] == layout_index[1] & object@layout$layout_index[, 2] == layout_index[2])
                object@layout$graphic_fun_list[[ind]] = function(object) draw_annotation_legend(object, legend_list = annotation_legend_list, max_width = calc_legends_max_width(object, page_size))
            } else {
                ind = which(object@layout$layout_index[, 1] == layout_index[1] & object@layout$layout_index[, 2] == layout_index[2])
                object@layout$graphic_fun_list[[ind]] = function(object) draw_annotation_legend(object, legend_list = annotation_legend_list, max_height = calc_legends_max_height(object, page_size))
            }
        } else {
            object@annotation_legend_param$size = unit(c(0, 0), "null")
        }

        ht_list_width = sum(component_width(object)) + padding[2] + padding[4]
        ht_list_height = sum(component_height(object)) + padding[1] + padding[3]

        if(is_abs_unit(ht_list_width)) {
            ht_list_width = unit(convertWidth(ht_list_width, "mm", valueOnly = TRUE), "mm")
            # qqcat("Since all heatmaps/annotations have absolute units, the total width of the plot is @{ht_list_width}\n")
            w = ht_list_width
        } else {
            w = unit(1, "npc")
        }
        if(is_abs_unit(ht_list_height)) {
            ht_list_height = unit(convertHeight(ht_list_height, "mm", valueOnly = TRUE), "mm")
            # qqcat("Since all heatmaps/annotations have absolute units, the total height of the plot is @{ht_list_height}\n")
            h = ht_list_height
        } else {
            h = unit(1, "npc")
        }

        layout = grid.layout(nrow = length(HEATMAP_LIST_LAYOUT_COLUMN_COMPONENT), 
            ncol = length(HEATMAP_LIST_LAYOUT_ROW_COMPONENT), 
            widths = component_width(object), 
            heights = component_height(object))

        pushViewport(viewport(name = "global", width = w, height = h, gp = gpar(lineheight = 0.9)))
        grid.rect(gp = gpar(fill = object@ht_list_param$background, col = object@ht_list_param$background))
        pushViewport(viewport(layout = layout, name = "global_layout", x = padding[2], y = padding[1], width = unit(1, "npc") - padding[2] - padding[4],
            height = unit(1, "npc") - padding[1] - padding[3], just = c("left", "bottom")))
        ht_layout_index = object@layout$layout_index
        ht_graphic_fun_list = object@layout$graphic_fun_list

        for(j in seq_len(nrow(ht_layout_index))) {
            pushViewport(viewport(name = paste0("global-", rownames(ht_layout_index)[j]), layout.pos.row = ht_layout_index[j, 1], layout.pos.col = ht_layout_index[j, 2]))
            ht_graphic_fun_list[[j]](object)
            upViewport()
        }

        upViewport()
        upViewport()

        object@ht_list_param$width = w
        object@ht_list_param$height = h

        for(i in seq_along(object@ht_list)) {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                ht = object@ht_list[[i]]
                if(!is.null(ht@heatmap_param$post_fun)) {
                    ht@heatmap_param$post_fun(ht)
                }
            }
        }

        if(!is.null(object@ht_list_param$post_fun)) {
            object@ht_list_param$post_fun(object)
        }

        if(run_recordGraphics) {
            object <<- object
        }
    })

    run_recordGraphics = FALSE
    if(is_RStudio_current_dev() || (!dev.interactive())) {
        eval(code)
    } else {
        run_recordGraphics = TRUE
        grDevices::recordGraphics(eval(code), list(),  as.environment(-1))
    }

    object@ht_list_param$called_arguments = list(
        newpage = newpage,
        
        row_title = row_title, 
        row_title_side = row_title_side, 
        row_title_gp = row_title_gp,
        column_title = column_title, 
        column_title_side = column_title_side, 
        column_title_gp = column_title_gp, 

        heatmap_legend_side = heatmap_legend_side, 
        merge_legends = merge_legends,
        show_heatmap_legend = show_heatmap_legend, 
        heatmap_legend_list = heatmap_legend_list,
        annotation_legend_side = annotation_legend_side, 
        show_annotation_legend = show_annotation_legend, 
        annotation_legend_list = annotation_legend_list,
        align_heatmap_legend = align_heatmap_legend,
        align_annotation_legend = align_annotation_legend,
        legend_grouping = legend_grouping,

        gap = gap, 
        ht_gap = ht_gap, 

        main_heatmap = main_heatmap,
        padding = padding,
        adjust_annotation_extension = adjust_annotation_extension,
        
        auto_adjust = auto_adjust,
        row_dend_side = row_dend_side,
        row_sub_title_side = row_sub_title_side,
        column_dend_side = column_dend_side,
        column_sub_title_side = column_sub_title_side,
        
        row_gap = row_gap,
        cluster_rows = cluster_rows,
        cluster_row_slices = cluster_row_slices,
        clustering_distance_rows = clustering_distance_rows,
        clustering_method_rows = clustering_method_rows,
        row_dend_width = row_dend_width, 
        show_row_dend = show_row_dend, 
        row_dend_reorder = row_dend_reorder,
        row_dend_gp = row_dend_gp,
        row_order = row_order,
        km = km,
        split = split,
        row_km = row_km,
        row_km_repeats = row_km_repeats,
        row_split = row_split,
        height = height,
        heatmap_height = heatmap_height,

        column_gap = column_gap,
        cluster_columns = cluster_columns,
        cluster_column_slices = cluster_column_slices,
        clustering_distance_columns = clustering_distance_columns,
        clustering_method_columns = clustering_method_columns,
        column_dend_width = column_dend_width, 
        show_column_dend = show_column_dend, 
        column_dend_reorder = column_dend_reorder,
        column_dend_gp = column_dend_gp,
        column_order = column_order,
        column_km = column_km,
        column_km_repeats = column_km_repeats,
        column_split = column_split,
        width = width,
        heatmap_width = heatmap_width,

        use_raster = use_raster, 
        raster_device = raster_device,
        raster_quality = raster_quality,
        raster_device_param = raster_device_param,
        raster_resize = raster_resize,

        post_fun = post_fun,

        ### global setting
        heatmap_row_names_gp = heatmap_row_names_gp,
        heatmap_column_names_gp = heatmap_column_names_gp,
        heatmap_row_title_gp = heatmap_row_title_gp,
        heatmap_column_title_gp = heatmap_column_title_gp,
        legend_title_gp = legend_title_gp,
        legend_title_position = legend_title_position,
        legend_labels_gp = legend_labels_gp,
        legend_grid_height = legend_grid_height,
        legend_grid_width = legend_grid_width,
        legend_border = legend_border,
        legend_gap = legend_gap,
        heatmap_border = heatmap_border,
        annotation_border = annotation_border,
        fastcluster = fastcluster,
        simple_anno_size = simple_anno_size,
        show_parent_dend_line = show_parent_dend_line
    )

    if(save_last) {
        .ENV$last = object
    }

    return(invisible(object))
})

has_heatmap = function(ht_list) {
    any(sapply(ht_list@ht_list, inherits, "Heatmap"))
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


# == title
# Subset a HeatmapList object
#
# == param
# -x A `HeatmapList-class` object
# -i row indices
# -j column indices
#
# == details
# If the heatmap list is horizontal, ``i`` is the row indices and ``j`` corresponds to heatmap names and single annotation names.
# and if the heatlist is vertical, ``i`` corresponds to heatmap/annotation names and ``j`` is the column indices.
#
# == example
# ht_list = Heatmap(matrix(rnorm(100), 10), name = "rnorm") +
#   rowAnnotation(foo = 1:10, bar = anno_points(10:1)) + 
#   Heatmap(matrix(runif(100), 10), name = "runif")
# summary(ht_list[1:5, ])
# summary(ht_list[1:5, 1])
# summary(ht_list[1:5, "rnorm"])
# summary(ht_list[1:5, c("rnorm", "foo")])
#
# ht_list = Heatmap(matrix(rnorm(100), 10), name = "rnorm") \%v\%
#   columnAnnotation(foo = 1:10, bar = anno_points(10:1)) \%v\%
#   Heatmap(matrix(runif(100), 10), name = "runif")
# summary(ht_list[, 1:5])
# summary(ht_list[1, 1:5])
# summary(ht_list["rnorm", 1:5])
# summary(ht_list[c("rnorm", "foo"), 1:5])
"[.HeatmapList" = function(x, i, j) {

    direction = x@direction

    if(!is.null(x@layout$initialized)) {
        if(x@layout$initialized) {
            stop_wrap("subsetting on HeatmapList can only be applied before `draw()`.")
        }
    }

    if(direction == "horizontal") {
        if(nargs() == 2) {
            subset_heatmap_list_by_row(x, i, direction)
        } else {
            if(missing(i)) {
                subset_heatmap_list_by_column(x, j, direction)
            } else if(missing(j)) {
                subset_heatmap_list_by_row(x, i, direction)
            } else {
                x = subset_heatmap_list_by_row(x, i, direction)
                subset_heatmap_list_by_column(x, j, direction)
            }
        }
    } else {
        if(missing(i)) {
            subset_heatmap_list_by_column(x, j, direction)
        } else if(missing(j)) {
            subset_heatmap_list_by_row(x, i, direction)
        } else {
            x = subset_heatmap_list_by_row(x, i, direction)
            subset_heatmap_list_by_column(x, j, direction)
        }
    }
}

# there is no main heatmap yet
subset_heatmap_list_by_row = function(ht_list, ind, direction) {
    
    if(direction == "horizontal") {
        for(i in seq_along(ht_list@ht_list)) {
            if(inherits(ht_list@ht_list[[i]], "Heatmap")) {
                ht_list@ht_list[[i]] = ht_list@ht_list[[i]][ind, ]
            } else {
                ht_list@ht_list[[i]] = ht_list@ht_list[[i]][ind]
            }
        }
    } else {
        # if it is vertical heatmap list, `ind` corresponds to heatmap names and annotation names
        if(is.numeric(ind)) {
            ht_list@ht_list = ht_list@ht_list[ind]
        } else {
            hl = list()
            # also check annotation names
            if(!all(ind %in% names(ht_list))) {
                stop_wrap("Cannot find all name indices in the heatmap list.")
            }
            for(nm in names(ht_list@ht_list)) {
                if(inherits(ht_list@ht_list[[nm]], "Heatmap")) {
                    if(nm %in% ind) {
                        hl[[nm]] = ht_list@ht_list[[nm]]
                    }
                } else {
                    anno_nm = names(ht_list@ht_list[[nm]]@anno_list)
                    if(any(anno_nm %in% ind)) {
                        hl[[nm]] = ht_list@ht_list[[nm]][, intersect(ind, anno_nm)]
                    }
                }
            }
            ht_list@ht_list = hl
        }
    }
    return(ht_list)
}

subset_heatmap_list_by_column = function(ht_list, ind, direction) {
    if(direction == "horizontal") {
        # if it is horizontal heatmap list, `ind` corresponds to heatmap names and annotation names
        if(is.numeric(ind)) {
            ht_list@ht_list = ht_list@ht_list[ind]
        } else {
            hl = list()
            # also check annotation names
            if(!all(ind %in% names(ht_list))) {
                stop_wrap("Cannot find all name indices in the heatmap list.")
            }
            for(nm in names(ht_list@ht_list)) {
                if(inherits(ht_list@ht_list[[nm]], "Heatmap")) {
                    if(nm %in% ind) {
                        hl[[nm]] = ht_list@ht_list[[nm]]
                    }
                } else {
                    anno_nm = names(ht_list@ht_list[[nm]]@anno_list)
                    if(any(anno_nm %in% ind)) {
                        hl[[nm]] = ht_list@ht_list[[nm]][, intersect(ind, anno_nm)]
                    }
                }
            }
            ht_list@ht_list = hl
        }
    } else {
        for(i in seq_along(ht_list@ht_list)) {
            if(inherits(ht_list@ht_list[[i]], "Heatmap")) {
                ht_list@ht_list[[i]] = ht_list@ht_list[[i]][, ind]
            } else {
                ht_list@ht_list[[i]] = ht_list@ht_list[[i]][ind]
            }
        }
    }
    return(ht_list)
}

# == title
# Names of the heatmaps/annotations
#
# == param
# -x A `HeatmapList-class` object
#
names.HeatmapList = function(x) {
    nm = NULL
    for(i in seq_along(x@ht_list)) {
        if(inherits(x@ht_list[[i]], "Heatmap")) {
            nm = c(nm, x@ht_list[[i]]@name)
        } else {
            nm = c(nm, names(x@ht_list[[i]]))
        }
    }
    return(nm)
}

# == title
# Length of the HeatmapList object
#
# == param
# -x A `HeatmapList-class` object
#
length.HeatmapList = function(x) {
    length(x@ht_list)
}


# == title
# Summary of a Heatmap List
#
# == param
# -object A `HeatmapList-class` object.
# -... Other arguments.
#
summary.HeatmapList = function(object, ...) {
    n_ht = length(object@ht_list)

    direction = object@direction

    qqcat("A @{direction} heamtap list with @{n_ht} heatmap/annotations.\n")

    ht_name = names(object@ht_list)
    for(i in seq_len(n_ht)) {
        if(inherits(object@ht_list[[i]], "Heatmap")) {
            qqcat("  @{ht_name[i]}: a matrix with @{nrow(object@ht_list[[i]]@matrix)} rows and @{ncol(object@ht_list[[i]]@matrix)} columns\n")
        } else {
            qqcat("  @{ht_name[i]}: a list of @{length(object@ht_list[[i]]@anno_list)} annotations\n")
            for(j in seq_along(object@ht_list[[i]]@anno_list)) {
                qqcat("    @{object@ht_list[[i]]@anno_list[[j]]@name}:")
                if(is_simple_annotation(object@ht_list[[i]]@anno_list[[j]])) {
                    qqcat("   a simple annotation.\n")
                } else {
                    qqcat("   a complex annotation.\n")
                }
            }
        }
    }
}

# == title
# Calculate the width and height of the heatmaps
#
# == param
# -ht A `Heatmap-class` or `HeatmapList-class` object.
#
# == value
# A list of two elements: width and height.
#
ht_size = function(ht) {
    dev.null()
    ht = draw(ht)
    w = width(ht)
    h = height(ht)
    dev.off2()

    list(width = w, height = h)
}


which_first_ht = function(ht_list) {
    which(sapply(ht_list@ht_list, inherits, "Heatmap"))[1]
}


which_last_ht = function(ht_list) {
    max(which(sapply(ht_list@ht_list, inherits, "Heatmap")))
}

which_main_ht = function(ht_list) {
    ht_list@ht_list_param$main_heatmap
}


# remove the effect from make_layout
# return obj@ht_list
resetHeatmapList = function(ht_list) {
    ht_list@ht_list = lapply(ht_list@ht_list, function(x) {
        if(inherits(x, "Heatmap")) {
            resetHeatmap(x)
        } else {
            x
        }
    })
    ht_list
}


resetHeatmap = function(ht) {
    ht@layout = list(layout_size = list(column_title_top_height = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), column_dend_top_height = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), column_anno_top_height = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), column_names_top_height = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), column_title_bottom_height = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), column_dend_bottom_height = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), column_anno_bottom_height = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), column_names_bottom_height = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), row_title_left_width = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), row_dend_left_width = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), row_names_left_width = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), row_dend_right_width = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), row_names_right_width = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), row_title_right_width = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), row_anno_left_width = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2")), row_anno_right_width = structure(0, unit = 7L, class = c("simpleUnit",  "unit", "unit_v2"))), layout_index = structure(logical(0), .Dim = c(0L,  2L)), graphic_fun_list = list(), initialized = FALSE)
    ht
}



# == title
# Draw heatmap
#
# == param
# -x A `HeatmapList-class` object.
# -... All pass to `draw,HeatmapList-method`.
#
plot.HeatmapList = function(x, ...) {
    draw(x, ...)
}

# == title
# Draw heatmap
#
# == param
# -x A `Heatmap-class` object.
# -... All pass to `draw,Heatmap-method`.
#
plot.Heatmap = function(x, ...) {
    draw(x, ...)
}

# == title
# Draw heatmap annotations
#
# == param
# -x A `HeatmapAnnotation-class` object.
# -... All pass to `draw,HeatmapList-method`.
#
plot.HeatmapAnnotation = function(x, ...) {
    if(x@which == "column") {
        draw(x %v% NULL, ...)
    } else {
        draw(x + NULL, ...)
    }
}
