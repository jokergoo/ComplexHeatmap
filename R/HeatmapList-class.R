
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
# -direction direction
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
        stop("The heatmap list should only be all horizontal or vertical.")
    }

    # check settings of this new heatmap
    if(inherits(x, "Heatmap")) {
        ht_name = x@name
        x = list(x)
        names(x) = ht_name
        object@ht_list = c(object@ht_list, x)
    } else if(inherits(x, "HeatmapAnnotation")) {
        if(x@which == "row" && direction == "vertical") {
            stop("Row annotations should be added to the heatmap list in horizontal direction.")
        } else if(x@which == "column" && direction == "horizontal") {
            stop("Column annotations should be added to the heatmap list in vertical direction.")
        }
        ht_name = x@name
        x = list(x)
        names(x) = ht_name
        object@ht_list = c(object@ht_list, x)
        
    } else if(inherits(x, "HeatmapList")) {
        ht_name = names(x@ht_list)
        object@ht_list = c(object@ht_list, x@ht_list)
    }

    ht_name = names(object@ht_list)
    which_duplicated = duplicated(ht_name)
    if(any(which_duplicated)) {
        warning(paste0("Heatmap/annotation names are duplicated: ", paste(ht_name[which_duplicated], collapse = ", ")))
    }

    l = which(sapply(object@ht_list, inherits, "Heatmap"))
    if(direction == "horizontal") {
        nr = sapply(object@ht_list[l], function(ht) nrow(ht@matrix))

        if(length(unique(nr)) > 1) {
            cat("`nrow` of all heatmaps:\n")
            print(nr)
            stop("`nrow` of all heatmaps should be the same for horizontal heatmap list.")
            for(i in l) {
                cat(object@ht_list[[i]]@name, ":", nrow(object@ht_list[[i]]@matrix), "\n")
            }
            cat("\n")
        }
    } else {
        nc = sapply(object@ht_list[l], function(ht) ncol(ht@matrix))

        if(length(unique(nc)) > 1) {
            cat("`ncol` of all heatmaps:\n")
            print(nc)
            stop("`ncol` of all heatmaps should be the same for vertical heatmap list.")
            for(i in l) {
                cat(object@ht_list[[i]]@name, ":", ncol(object@ht_list[[i]]@matrix), "\n")
            }
            cat("\n")
        }
    }

    return(object)
})


# == title
# Draw a list of heatmaps
#
# == param
# -object a `HeatmapList-class` object
# -newpage whether create a new page for the graphics.
# -row_title title on the row.
# -row_title_side will the title be put on the left or right of the heatmap.
# -row_title_gp graphic parameters for drawing text.
# -column_title title on the column.
# -column_title_side will the title be put on the top or bottom of the heatmap.
# -column_title_gp graphic parameters for drawing text.
# -heatmap_legend_side = c("right", "left", "bottom", "top"), 
# -heatmap_legend_offset = unit(0, "mm"),
# -merge_legends = FALSE,
# -show_heatmap_legend = TRUE, 
# -heatmap_legend_list = list(),
# -annotation_legend_side = c("right", "left", "bottom", "top"), 
# -annotation_legend_offset = unit(0, "mm"),
# -show_annotation_legend = TRUE, 
# -annotation_legend_list = list(),
# -gap = unit(2, "mm"), 
# -ht_gap = gap, 
# -main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
# -padding = NULL,
# -adjust_annotation_name adjust_annotation_name
# -row_dend_side = c("original", "left", "right"),
# -row_sub_title_side = c("original", "left", "right"),
# -column_dend_side = c("original", "top", "bottom"),
# -column_sub_title_side = c("original", "top", "bottom"), 
# -row_gap = NULL,
# -cluster_rows = NULL,
# -clustering_distance_rows = NULL,
# -clustering_method_rows = NULL,
# -row_dend_width = NULL, 
# -show_row_dend = NULL, 
# -row_dend_reorder = NULL,
# -row_dend_gp = NULL,
# -row_order = NULL,
# -km = NULL,
# -split = NULL,
# -row_km = km,
# -row_split = split,
# -heatmap_body_height = NULL,
# -column_gap = NULL,
# -cluster_columns = NULL,
# -clustering_distance_columns = NULL,
# -clustering_method_columns = NULL,
# -column_dend_width = NULL, 
# -show_column_dend = NULL, 
# -column_dend_reorder = NULL,
# -column_dend_gp = NULL,
# -column_order = NULL,
# -column_km = NULL,
# -column_split = NULL,
# -heatmap_body_width = NULL,
# -heatmap_row_names_gp = NULL,
# -heatmap_column_names_gp = NULL,
# -heatmap_row_title_gp = NULL,
# -heatmap_column_title_gp = NULL,
# -legend_title_gp = NULL,
# -legend_title_position = NULL,
# -legend_labels_gp = NULL,
# -legend_grid_height = NULL,
# -legend_grid_width = NULL,
# -legend_grid_border = NULL,
# -fastcluster = NULL,
# -show_vp_border = NULL,
# -anno_simple_row_size = NULL
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
    newpage = TRUE,

    row_title = character(0), 
    row_title_side = c("left", "right"), 
    row_title_gp = gpar(fontsize = 14),
    column_title = character(0), 
    column_title_side = c("top", "bottom"), 
    column_title_gp = gpar(fontsize = 14), 

    heatmap_legend_side = c("right", "left", "bottom", "top"), 
    heatmap_legend_offset = unit(0, "mm"),
    merge_legends = FALSE,
    show_heatmap_legend = TRUE, 
    heatmap_legend_list = list(),
    annotation_legend_side = c("right", "left", "bottom", "top"), 
    annotation_legend_offset = unit(0, "mm"),
    show_annotation_legend = TRUE, 
    annotation_legend_list = list(),

    gap = unit(2, "mm"), 
    ht_gap = gap, 

    main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
    padding = GLOBAL_PADDING,
    adjust_annotation_extension = TRUE,
    
    auto_adjust = TRUE,
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
    km = NULL,
    split = NULL,
    row_km = km,
    row_split = split,
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
    heatmap_body_width = NULL,

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
    heatmap_border = NULL,
    annotation_border = NULL,
    fastcluster = NULL,
    anno_simple_size = NULL
    ) {

    verbose = ht_opt$verbose

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
                    "heatmap_border",
                    "annotation_border",
                    "fastcluster",
                    "anno_simple_size")) {
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

    l = sapply(object@ht_list, inherits, "Heatmap")
    if(! any(l)) {
        ob = sapply(object@ht_list, nobs)
        ob = ob[!is.na(ob)]
        if(length(ob) == 0) {
            stop("There is no heatmap in the list and cannot infer the number of observations in the heatmap annotations, please add a zero row/column matrix by hand.")
        }
        if(direction == "horizontal") {
            nr = ob[1]
            object = object + Heatmap(matrix(ncol = 0, nrow = nr))
        } else {
            nc = ob[1]
            object = object %v% Heatmap(matrix(nrow = 0, ncol = nc))
        }
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

    object@ht_list_param$adjust_annotation_extension = adjust_annotation_extension

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
        clustering_distance_rows = clustering_distance_rows,
        clustering_method_rows = clustering_method_rows,
        row_dend_width = row_dend_width, 
        show_row_dend = show_row_dend, 
        row_dend_reorder = row_dend_reorder,
        row_dend_gp = row_dend_gp,
        row_order = row_order,
        row_km = row_km,
        row_split = row_split,
        heatmap_body_height = heatmap_body_height,

        column_gap = column_gap,
        cluster_columns = cluster_columns,
        clustering_distance_columns = clustering_distance_columns,
        clustering_method_columns = clustering_method_columns,
        column_dend_width = column_dend_width, 
        show_column_dend = show_column_dend, 
        column_dend_reorder = column_dend_reorder,
        column_dend_gp = column_dend_gp,
        column_order = column_order,
        column_km = column_km,
        column_split = column_split,
        heatmap_body_width = heatmap_body_width
    )

    layout = grid.layout(nrow = length(HEATMAP_LIST_LAYOUT_COLUMN_COMPONENT), 
        ncol = length(HEATMAP_LAYOUT_ROW_COMPONENT), 
        widths = component_width(object), 
        heights = component_height(object))
    ht_list_width = sum(component_width(object)) + padding[2] + padding[4]
    ht_list_height = sum(component_height(object)) + padding[1] + padding[3]

    if(is_abs_unit(ht_list_width)) {
        ht_list_width = unit(round(convertWidth(ht_list_width, "mm", valueOnly = TRUE)), "mm")
        qqcat("Since all heatmaps/annotations have absolute units, the total width of the plot is @{ht_list_width}\n")
        w = ht_list_width
    } else {
        w = unit(1, "npc")
    }
    if(is_abs_unit(ht_list_height)) {
        ht_list_height = unit(round(convertHeight(ht_list_height, "mm", valueOnly = TRUE)), "mm")
        qqcat("Since all heatmaps/annotations have absolute units, the total height of the plot is @{ht_list_height}\n")
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

    for(i in seq_along(object@ht_list)) {
        if(inherits(object@ht_list[[i]], "Heatmap")) {
            ht = object@ht_list[[i]]
            if(!is.null(ht@heatmap_param$post_fun)) {
                ht@heatmap_param$post_fun(ht)
            }
        }
    }

    return(invisible(object))
})

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

    if(length(x@ht_list) == 1) {
        if(inherits(x@ht_list[[1]], "Heatmap")) {
           return(x@ht_list[[1]][i, j])
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
