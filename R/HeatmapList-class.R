
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
# - `add_heatmap,HeatmapList-method` add heatmaps to the list of heatmaps.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
HeatmapList = setClass("HeatmapList",
    slots = list(
        ht_list = "list",
        ht_list_param = "list",

        row_title = "character",
        row_title_param = "list",
        column_title = "character",
        column_title_param = "list",

        annotation_legend_param = "list",
        heatmap_legend_param = "list",

        layout = "list"
    ),
    prototype = list(
        layout = list(
            layout_annotation_legend_left_width = unit(0, "null"),
            layout_heatmap_legend_left_width = unit(0, "null"),
            layout_row_title_left_width = unit(0, "null"),
            layout_row_title_right_width = unit(0, "null"),
            layout_heatmap_legend_right_width = unit(0, "null"),
            layout_annotation_legend_right_width = unit(0, "null"),

            layout_annotation_legend_top_height = unit(0, "null"),
            layout_heatmap_legend_top_height = unit(0, "null"),
            layout_column_title_top_height = unit(0, "null"),
            layout_column_title_bottom_height = unit(0, "null"),
            layout_heatmap_legend_bottom_height = unit(0, "null"),
            layout_annotation_legend_bottom_height = unit(0, "null"),
            
            layout_index = matrix(nrow = 0, ncol = 2),
            graphic_fun_list = list()
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

    l = which(sapply(object@ht_list, inherits, "Heatmap"))
    nr = sapply(object@ht_list[l], function(ht) nrow(ht@matrix))

    if(length(unique(nr)) > 1) {
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
# -show_heatmap_legend whether show heatmap legend.
# -annotation_legend_side side of annotation legend.
# -show_annotation_legend whether show annotation legend.
# -annotation_legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -gap gap between heatmaps, should be a `grid::unit` object.
# -auto_adjust auto adjust if the number of heatmap is larger than one.
# -main_heatmap name or index for the main heatmap
# -row_hclust_side if auto adjust, where to put the row dendrograms for the main heatmap
# -row_sub_title_side if auto adjust, where to put sub row titles for the main heatmap
# -... graphic parameters pass to `color_mapping_legend,ColorMapping-method`.
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
    definition = function(object, row_title = character(0), 
    row_title_side = c("left", "right"), row_title_gp = gpar(fontsize = 14),
    column_title = character(0), column_title_side = c("top", "bottom"), 
    column_title_gp = gpar(fontsize = 14), 
    heatmap_legend_side = c("right", "left", "bottom", "top"), 
    show_heatmap_legend = TRUE,
    annotation_legend_side = c("right", "left", "bottom", "top"), 
    show_annotation_legend = TRUE, annotation_legend_list = list(),
    gap = unit(3, "mm"), auto_adjust = TRUE, 
    main_heatmap = which(sapply(object@ht_list, inherits, "Heatmap"))[1],
    row_hclust_side = c("original", "left", "right"),
    row_sub_title_side = c("original", "left", "right"), ...) {

    n = length(object@ht_list)
    i_main = main_heatmap[1]

    # i_main is aleays numeric index
    if(is.character(i_main)) {
        i_main = which(names(object@ht_list) == i_main)[1]
    }


    if(inherits(object@ht_list[[i_main]], "HeatmapAnnotation")) {
        stop("the main heatmap can only be a heatmap.")
    }

    nr = nrow(object@ht_list[[i_main]]@matrix)

    if(n > 1) {
        if(length(gap) == 1) {
            if(inherits(gap, "unit")) {
                gap = do.call("unit.c", rep(list(gap), n))
            }
        } else if(length(gap) == n - 1) {
            gap = unit.c(gap, unit(1, "null"))
        } else if(length(gap) != n) {
            stop(paste0("length of `gap` can only be 1 or ", n-1, " or ", n, "."))
        }
    } else {
        if(!is.unit(gap)) {
            gap = unit(rep(0, n), "null")    
        }
    }
    object@ht_list_param$gap = gap

    if(auto_adjust) {
        ht_main = object@ht_list[[i_main]]
        ht_main = make_row_cluster(ht_main)
        
        row_order = unlist(ht_main@row_order_list)
        split = ht_main@matrix_param$split

        row_hclust_side = match.arg(row_hclust_side)[1]
        row_sub_title_side = match.arg(row_sub_title_side)[1]

        if(row_hclust_side == "left" || row_sub_title_side == "left") {
            # if the first one is a HeatmapAnnotation object
            if(inherits(object@ht_list[[1]], "HeatmapAnnotation")) {
                object = Heatmap(matrix(nrow = nr, ncol = 0)) + object
                gap = unit.c(unit(0, "null"), gap)
                i_main = i_main + 1
            }
            
        }

        if(row_hclust_side == "right" || row_sub_title_side == "right") {
            # if the last one is a HeatmapAnnotation object
            if(inherits(object@ht_list[[ length(object@ht_list) ]], "HeatmapAnnotation")) {
                object = object + Heatmap(matrix(nrow = nr, ncol = 0))
                gap = unit.c(gap, unit(0, "null"))
            }
        }
        object@ht_list_param$gap = gap

        n = length(object@ht_list)

        if(row_sub_title_side == "left") {
            for(i in seq_len(n)) {
                if(i == 1) {
                    object@ht_list[[i]]@row_title = ht_main@row_title
                    object@ht_list[[i]]@row_title_param = ht_main@row_title_param
                    object@ht_list[[i]]@row_title_param$side = "left"
                } else {
                    if(inherits(object@ht_list[[i]], "Heatmap")) {
                        object@ht_list[[i]]@row_title = character(0)
                    }
                }
            }
        } else if(row_sub_title_side == "right") {
            for(i in seq_len(n)) {
                if(i == n) {
                    object@ht_list[[n]]@row_title = ht_main@row_title
                    object@ht_list[[n]]@row_title_param = ht_main@row_title_param
                    object@ht_list[[n]]@row_title_param$side = "right"
                } else {
                    if(inherits(object@ht_list[[i]], "Heatmap")) {
                        object@ht_list[[i]]@row_title = character(0)
                    }
                }
            }
        } else {
            for(i in seq_len(n)) {
                if(i == i_main) {
                    object@ht_list[[i]]@row_title = ht_main@row_title
                    object@ht_list[[i]]@row_title_param = ht_main@row_title_param      
                } else {
                    if(inherits(object@ht_list[[i]], "Heatmap")) {
                        object@ht_list[[i]]@row_title = character(0)
                    }
                }
            }
        }


        if(row_hclust_side == "left") {
            for(i in seq_len(n)) {
                if(i == 1) {
                    object@ht_list[[1]]@row_hclust_list = ht_main@row_hclust_list
                    object@ht_list[[1]]@row_hclust_param = ht_main@row_hclust_param
                    object@ht_list[[1]]@row_hclust_param$side = "left"
                } else {
                    if(inherits(object@ht_list[[i]], "Heatmap")) {
                        object@ht_list[[i]]@row_hclust_param$show = FALSE
                    }
                }
                if(inherits(object@ht_list[[i]], "Heatmap")) {
                    object@ht_list[[i]]@row_order_list = ht_main@row_order_list
                    object@ht_list[[i]]@row_hclust_param$cluster = FALSE
                }
            }
        } else if(row_hclust_side == "right") {
            for(i in seq_len(n)) {
                if(i == n) {
                    object@ht_list[[n]]@row_hclust_list = ht_main@row_hclust_list
                    object@ht_list[[n]]@row_hclust_param = ht_main@row_hclust_param
                    object@ht_list[[n]]@row_hclust_param$side = "right"
                } else {
                    if(inherits(object@ht_list[[i]], "Heatmap")) {
                        object@ht_list[[i]]@row_hclust_param$show = FALSE
                    }
                }
                if(inherits(object@ht_list[[i]], "Heatmap")) {
                    object@ht_list[[i]]@row_order_list = ht_main@row_order_list
                    object@ht_list[[i]]@row_hclust_param$cluster = FALSE
                }
            }
        } else {
            for(i in seq_len(n)) {
                if(i == i_main) {
                    object@ht_list[[i]]@row_hclust_list = ht_main@row_hclust_list
                    object@ht_list[[i]]@row_hclust_param = ht_main@row_hclust_param
                } else {
                    if(inherits(object@ht_list[[i]], "Heatmap")) {
                        object@ht_list[[i]]@row_hclust_param$show = FALSE
                    }
                }
                if(inherits(object@ht_list[[i]], "Heatmap")) {
                    object@ht_list[[i]]@row_order_list = ht_main@row_order_list
                    object@ht_list[[i]]@row_hclust_param$cluster = FALSE
                }
            }
        }

        for(i in seq_len(n)) {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                object@ht_list[[i]]@matrix_param$km = 1
                object@ht_list[[i]]@row_title_param$combined_name_fun = NULL
                object@ht_list[[i]] = prepare(object@ht_list[[i]], row_order = NULL, split = NULL)
            }
        }
    }

    object@layout$layout_index = rbind(c(4, 4))
    object@layout$graphic_fun_list = list(function(object) draw_heatmap_list(object))

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
    object@column_title = column_title
    object@column_title_param$gp = check_gp(column_title_gp)
    object@column_title_param$side = column_title_side
    if(length(column_title) > 0) {
        if(column_title_side == "top") {
            object@layout$layout_column_title_top_height = grobHeight(textGrob(column_title, gp = column_title_gp))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(3, 4))
        } else {
            object@layout$layout_column_title_bottom_height = grobHeight(textGrob(column_title, gp = column_title_gp))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(5, 4))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_title(object, which = "column"))
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
    object@row_title = row_title
    object@row_title_param$gp = check_gp(row_title_gp)
    object@row_title_param$side = row_title_side
    if(length(row_title) > 0) {
        if(row_title_side == "left") {
            object@layout$layout_row_title_left_width = grobHeight(textGrob(row_title, gp = row_title_gp))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 3))
        } else {
            object@layout$layout_row_title_right_width = grobHeight(textGrob(row_title, gp = row_title_gp))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 5))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_title(object, which = "row"))
    }

    #################################################
    ## heatmap legend to top, bottom, left and right
    # default values
    object@heatmap_legend_param$show = show_heatmap_legend
    heatmap_legend_side = match.arg(heatmap_legend_side)[1]
    object@heatmap_legend_param$side = heatmap_legend_side   
    if(show_heatmap_legend) {
        if(heatmap_legend_side == "top") {
            object@heatmap_legend_param$padding = unit(c(2, 0, 2, 0), "mm")
            size = heatmap_legend_size(object, ...)
            object@heatmap_legend_param$size = size
            object@layout$layout_heatmap_legend_top_height = size[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(2, 4))
        } else if(heatmap_legend_side == "bottom") {
            object@heatmap_legend_param$padding = unit(c(2, 0, 2, 0), "mm")
            size = heatmap_legend_size(object, ...)
            object@heatmap_legend_param$size = size
            object@layout$layout_heatmap_legend_bottom_height = size[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(6, 4))
        } else if(heatmap_legend_side == "left") {
            object@heatmap_legend_param$padding = unit(c(0, 2, 0, 2), "mm")
            size = heatmap_legend_size(object, ...)
            object@heatmap_legend_param$size = size
            object@layout$layout_heatmap_legend_left_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 2))
        } else if(heatmap_legend_side == "right") {
            object@heatmap_legend_param$padding = unit(c(0, 2, 0, 2), "mm")
            size = heatmap_legend_size(object, ...)
            object@heatmap_legend_param$size = size
            object@layout$layout_heatmap_legend_right_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 6))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_heatmap_legend(object, ...))
    } else {
        object@heatmap_legend_param$size = unit(c(0, 0), "null")
    }

    #################################################
    ## annotation legend to top, bottom, left and right
    # default values
    ColorMappingList = list()
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
    if(length(ColorMappingList) == 0 && length(annotation_legend_list) == 0) {
        show_annotation_legend = FALSE
    }
    object@annotation_legend_param$show = show_annotation_legend
    annotation_legend_side = match.arg(annotation_legend_side)[1]
    object@annotation_legend_param$side = annotation_legend_side
    if(show_annotation_legend) {
        if(annotation_legend_side == "top") {
            object@annotation_legend_param$padding = unit(c(2, 0, 2, 0), "mm")
            size = annotation_legend_size(object, annotation_legend_list = annotation_legend_list, ...)
            object@annotation_legend_param$size = size
            object@layout$layout_annotation_legend_top_height = size[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(1, 4))
        } else if(annotation_legend_side == "bottom") {
            object@annotation_legend_param$padding = unit(c(2, 0, 2, 0), "mm")
            size = annotation_legend_size(object, annotation_legend_list = annotation_legend_list, ...)
            object@annotation_legend_param$size = size
            object@layout$layout_annotation_legend_bottom_height = size[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(7, 4))
        } else if(annotation_legend_side == "left") {
            object@annotation_legend_param$padding = unit(c(0, 2, 0, 2), "mm")
            size = annotation_legend_size(object, annotation_legend_list = annotation_legend_list, ...)
            object@annotation_legend_param$size = size
            object@layout$layout_annotation_legend_left_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 1))
        } else if(annotation_legend_side == "right") {
            object@annotation_legend_param$padding = unit(c(0, 2, 0, 2), "mm")
            size = annotation_legend_size(object, annotation_legend_list = annotation_legend_list, ...)
            object@annotation_legend_param$size = size
            object@layout$layout_annotation_legend_right_width = size[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 7))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_annotation_legend(object, annotation_legend_list = annotation_legend_list, ...))
    } else {
        object@annotation_legend_param$size = unit(c(0, 0), "null")
    }

    return(object)
})

# == title
# Draw a list of heatmaps
#
# == param
# -object a `HeatmapList-class` object
# -... pass to `make_layout,HeatmapList-method`
# -newpage whether to create a new page
#
# == detail
# The function first calls `make_layout,HeatmapList-method` to calculate
# the layout of the heatmap list and the layout of every single heatmap,
# then makes the plot by re-calling the graphic functions which are already recorded
# in the layout.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw",
    signature = "HeatmapList",
    definition = function(object, ..., newpage= TRUE) {

    if(! any(sapply(object@ht_list, inherits, "Heatmap"))) {
        stop("There should be at least one Heatmap in the heatmap list.")
    }
    
    if(newpage) {
        grid.newpage()
    }
    
    object = make_layout(object, ...)

    layout = grid.layout(nrow = 7, ncol = 7, widths = component_width(object, 1:7), heights = component_height(object, 1:7))
    pushViewport(viewport(layout = layout, name = "global"))
    ht_layout_index = object@layout$layout_index
    ht_graphic_fun_list = object@layout$graphic_fun_list
    
    for(j in seq_len(nrow(ht_layout_index))) {
        pushViewport(viewport(layout.pos.row = ht_layout_index[j, 1], layout.pos.col = ht_layout_index[j, 2]))
        ht_graphic_fun_list[[j]](object)
        upViewport()
    }

    upViewport()
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
            unit(1, "null")
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

    .single_unit = function(k) {
        if(k == 1) {
            object@layout$layout_annotation_legend_top_height
        } else if(k == 2) {
            object@layout$layout_heatmap_legend_top_height
        } else if(k == 3) {
            object@layout$layout_column_title_top_height
        } else if(k == 4) {
            unit(1, "null")
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

    gap = object@ht_list_param$gap
    ht_index = which(sapply(object@ht_list, inherits, "Heatmap"))

    n = length(object@ht_list)
    # since each heatmap actually has nine rows, calculate the maximum height of corresponding rows in all heatmap 
    max_component_height = unit.c(
        max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 1)))),
        max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 2)))),
        max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 3)))),
        max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 4)))),
        max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 5)))),
        max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 6)))),
        max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 7)))),
        max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 8)))),
        max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, k = 9))))
    )

    # set back to each heatmap
    for(i in ht_index) {
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 1, max_component_height[1])
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 2, max_component_height[2])
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 3, max_component_height[3])
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 4, max_component_height[4])
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 6, max_component_height[6])
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 7, max_component_height[7])
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 8, max_component_height[8])
        object@ht_list[[i]] = set_component_height(object@ht_list[[i]], k = 9, max_component_height[9])
    }

    width_without_heatmap_body = do.call("unit.c", lapply(object@ht_list, function(ht) {
        if(inherits(ht, "Heatmap")) {
            component_width(ht, c(1:3, 5:7))
        } else {
            unit(rep(0, 6), "null")  # to be consistent with heatmap non-body columns
        }
    }))
    
    # number of columns in heatmap whic are not fixed width
    heatmap_ncol = sapply(object@ht_list, function(ht) {
        if(inherits(ht, "Heatmap")) {
            if(is.null(ht@heatmap_param$width)) {
                return(ncol(ht@matrix))
            }
        }
        return(0)
    })

    heatmap_fixed_width = lapply(object@ht_list, function(ht) {
        if(inherits(ht, "Heatmap")) {
            if(!is.null(ht@heatmap_param$width)) {
                return(ht@heatmap_param$width)
            } else {
                return(unit(0, "null"))
            }
        } else if(inherits(ht, "HeatmapAnnotation")) {
            if(is.null(ht@size)) {
                return(unit(0, "null"))
            } else {
                return(ht@size)
            }
        }
    })
    heatmap_fixed_width = do.call("unit.c", heatmap_fixed_width)
    # width for body for each heatmap
    heatmap_body_width = (unit(1, "npc") - sum(width_without_heatmap_body) - sum(heatmap_fixed_width) - sum(gap) + gap[length(gap)]) * (1/sum(heatmap_ncol)) * heatmap_ncol

    # width of heatmap including body, and other components
    # width without fixed width
    heatmap_width = sum(width_without_heatmap_body[1:3]) + heatmap_body_width[1] + sum(width_without_heatmap_body[5:7-1])
    for(i in seq_len(n - 1) + 1) {
        heatmap_width = unit.c(heatmap_width, sum(width_without_heatmap_body[6*(i-1) + 1:3]) + heatmap_body_width[i] + sum(width_without_heatmap_body[6*(i-1) + 5:7-1]))
    }
    # width with fixed width
    heatmap_width = heatmap_width + heatmap_fixed_width

    pushViewport(viewport(name = "main_heatmap_list"))
    
    x = unit(0, "npc")
    kk = which(sapply(object@ht_list, inherits, "Heatmap"))[1]
    htkk = object@ht_list[[kk]]
    slice_gap = htkk@matrix_param$gap
    n_slice = length(htkk@row_order_list)
    snr = sapply(htkk@row_order_list, length)
    slice_height = (unit(1, "npc") - sum(max_component_height[c(1:4,6:9)]) - slice_gap*(n_slice-1))*(snr/sum(snr))
    for(i in seq_len(n_slice)) {
        if(i == 1) {
            slice_y = unit(1, "npc") - sum(max_component_height[c(1:4)])
        } else {
            slice_y = unit.c(slice_y, unit(1, "npc") - sum(max_component_height[c(1:4)]) - sum(slice_height[seq_len(i-1)]) - slice_gap*(i-1))
        }
    }

    for(i in seq_len(n)) {
        ht = object@ht_list[[i]]

        if(i > 1) {
            x = sum(heatmap_width[seq_len(i-1)]) + sum(gap[seq_len(i-1)])
        }
        
        pushViewport(viewport(x = x, y = unit(0, "npc"), width = heatmap_width[i], just = c("left", "bottom"), name = paste0("heatmap_", object@ht_list[[i]]@name)))
        if(inherits(ht, "Heatmap")) {
            draw(ht, internal = TRUE)
        } else if(inherits(ht, "HeatmapAnnotation")) {
            # calcualte the position of the heatmap body
            for(j in seq_len(n_slice)) {
                draw(ht, index = rev(htkk@row_order_list[[j]]), y = slice_y[j], height = slice_height[j], just = c("center", "top"))
            }
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

        pushViewport(viewport(name = "global_row_title", clip = FALSE))
        grid.text(title, rot = rot, gp = gp)
        upViewport()
    } else {
        pushViewport(viewport(name = "global_column_title", clip = FALSE))
        grid.text(title, gp = gp)
        upViewport()
    }
})

# == title
# Draw legends for all heatmaps
#
# == param
# -object a `HeatmapList-class` object
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
    definition = function(object, ...) {

    side = object@heatmap_legend_param$side
    size = object@heatmap_legend_param$size
    padding = object@heatmap_legend_param$padding

    ColorMappingList = list()
    for(i in seq_along(object@ht_list)) {
        if(inherits(object@ht_list[[i]], "Heatmap")) {
            if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                ColorMappingList = c(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
            }
        } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
            ColorMappingList = c(ColorMappingList, get_color_mapping_list(object@ht_list[[i]]))
        }
    }

    annotation_legend_side = object@annotation_legend_param$side
    annotation_legend_size = object@annotation_legend_param$size
    if(side != annotation_legend_side) {
        pushViewport(viewport(name = "heatmap_legend", x = unit(0.5, "npc"), y = unit(0.5, "npc"), width = size[1], height = size[2], just = c("center", "center")))
    } else {
        if(side %in% c("left", "right")) {
            y1 = unit(0.5, "npc") + size[2]*0.5  # top of heatmap legend
            y2 = unit(0.5, "npc") + annotation_legend_size[2]*0.5
            y = max(y1, y2)
            pushViewport(viewport(name = "heatmap_legend", x = unit(0.5, "npc"), y = y, width = size[1], height = size[2], just = c("center", "top")))           
        } else {
            x1 = unit(0.5, "npc") - size[1]*0.5  # top of heatmap legend
            x2 = unit(0.5, "npc") - annotation_legend_size[1]*0.5
            x = min(x1, x2)
            pushViewport(viewport(name = "heatmap_legend", x = x, y = unit(0.5, "npc"), width = size[1], height = size[2], just = c("left", "center")))           
        }
    }
    draw_legend(ColorMappingList, side = side, padding = padding, ...)
    upViewport()
})

# == title
# Draw legends for all column annotations
#
# == param
# -object a `HeatmapList-class` object
# -annotation_legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
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
    definition = function(object, annotation_legend_list = list(), ...) {

    side = object@annotation_legend_param$side
    size = object@annotation_legend_param$size
    padding = object@annotation_legend_param$padding

    ColorMappingList = list()
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

    heatmap_legend_side = object@heatmap_legend_param$side
    heatmap_legend_size = object@heatmap_legend_param$size
    if(side != heatmap_legend_side) {
        pushViewport(viewport(name = "annotation_legend", x = unit(0.5, "npc"), y = unit(0.5, "npc"), width = size[1], height = size[2], just = c("center", "center")))
    } else {
        if(side %in% c("left", "right")) {
            y1 = unit(0.5, "npc") + size[2]*0.5  # top of heatmap legend
            y2 = unit(0.5, "npc") + heatmap_legend_size[2]*0.5
            y = max(y1, y2)
            pushViewport(viewport(name = "annotation_legend", x = unit(0.5, "npc"), y = y, width = size[1], height = size[2], just = c("center", "top")))           
        } else {
            x1 = unit(0.5, "npc") - size[1]*0.5  # top of heatmap legend
            x2 = unit(0.5, "npc") - heatmap_legend_size[1]*0.5
            x = min(x1, x2)
            pushViewport(viewport(name = "annotation_legend", x = x, y = unit(0.5, "npc"), width = size[1], height = size[2], just = c("left", "center")))           
        }
    }
    draw_legend(ColorMappingList, side = side, annotation_legend_list = annotation_legend_list, padding = padding, ...)
    upViewport()
})

# == title
# Size of the heatmap legend viewport
#
# == param
# -object a `HeatmapList-class` object
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
    definition = function(object, ...) {

    side = object@heatmap_legend_param$side
    padding = object@heatmap_legend_param$padding

    ColorMappingList = list()
    for(i in seq_along(object@ht_list)) {
        if(inherits(object@ht_list[[i]], "Heatmap")) {
            if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                ColorMappingList = c(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
            }
        } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
            ColorMappingList = c(ColorMappingList, get_color_mapping_list(object@ht_list[[i]]))
        }
    }

    size = draw_legend(ColorMappingList, side = side, plot = FALSE, padding = padding, ...)

    return(size)
})

# == title
# Size of the annotation legend viewport
#
# == param
# -object a `HeatmapList-class` object.
# -annotation_legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
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
    definition = function(object, annotation_legend_list = list(), ...) {

    side = object@annotation_legend_param$side
    padding = object@annotation_legend_param$padding

    ColorMappingList = list()
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

    size = draw_legend(ColorMappingList, side = side, plot = FALSE, annotation_legend_list = annotation_legend_list, padding = padding, ...)
    
    return(size)
})

# create a viewport which contains legend
# currently, one-row or one-column legend is supported
draw_legend = function(ColorMappingList, side = c("right", "left", "top", "bottom"), plot = TRUE, 
    gap = unit(2, "mm"), annotation_legend_list = list(), padding = unit(c(0, 0, 0, 0), "null"), ...) {

    side = match.arg(side)[1]

    n = length(ColorMappingList)

    if(n == 0 && length(annotation_legend_list) == 0) {
        return(unit(c(0, 0), "null"))
    }

    if(n > 0 && length(annotation_legend_list) > 0) {
        cm_size = lapply(ColorMappingList, function(cm) color_mapping_legend(cm, plot = FALSE, ...))
        cm_width = do.call("unit.c", lapply(cm_size, function(x) x[1]))
        cm_height = do.call("unit.c", lapply(cm_size, function(x) x[2]))

        for(i in seq_along(annotation_legend_list)) {
            cm_width = unit.c(cm_width, grobWidth(annotation_legend_list[[i]]))
            cm_height = unit.c(cm_height, grobHeight(annotation_legend_list[[i]]))
        }
    } else if(n > 0) {
        cm_size = lapply(ColorMappingList, function(cm) color_mapping_legend(cm, plot = FALSE, ...))
        cm_width = do.call("unit.c", lapply(cm_size, function(x) x[1]))
        cm_height = do.call("unit.c", lapply(cm_size, function(x) x[2]))
    } else if(length(annotation_legend_list) > 0) {
        cm_width = do.call("unit.c", lapply(annotation_legend_list, grobWidth))
        cm_height = do.call("unit.c", lapply(annotation_legend_list, grobHeight))
    }

    if(side %in% c("left", "right")) {

        width = max(cm_width)
        height = sum(cm_height) + gap*(n + length(annotation_legend_list) -1)

        if(plot) {
            cm_height = unit.c(unit(0, "null"), cm_height)
        	for(i in seq_len(n)) {
                cm = ColorMappingList[[i]]
                if(side == "left") {
                    color_mapping_legend(cm, x = unit(0, "npc"), y = unit(1, "npc") - (sum(cm_height[seq_len(i)]) + gap*(i-1)), just = c("left", "top"), plot = TRUE, ...)                   
                } else {
                    color_mapping_legend(cm, x = unit(0, "npc") + padding[2], y = unit(1, "npc") - (sum(cm_height[seq_len(i)]) + gap*(i-1)), just = c("left", "top"), plot = TRUE, ...)
                }
            }
            
            if(length(annotation_legend_list)) {
                for(i in seq_along(annotation_legend_list)) {
                    if(side == "left") {
                        pushViewport(viewport(x = unit(0, "npc"), y = unit(1, "npc") - (sum(cm_height[seq_len(i+n)]) + gap*(n+i-1)), width = grobWidth(annotation_legend_list[[i]]), height = grobHeight(annotation_legend_list[[i]]), just = c("left", "top")))
                        grid.draw(annotation_legend_list[[i]])
                        upViewport()
                    } else {
                        pushViewport(viewport(x = unit(0, "npc") + padding[2], y = unit(1, "npc") - (sum(cm_height[seq_len(i+n)]) + gap*(n+i-1)), width = grobWidth(annotation_legend_list[[i]]), height = grobHeight(annotation_legend_list[[i]]), just = c("left", "top")))
                        grid.draw(annotation_legend_list[[i]])
                        upViewport()
                    }
                }
            }
        }

        size = unit.c(width, height)

    } else if(side %in% c("top", "bottom")) {

        width = sum(cm_width) + gap*(n + length(annotation_legend_list) - 1)
        height = max(cm_height)

        if(plot) {
            cm_width = unit.c(unit(0, "null"), cm_width)
            for(i in seq_len(n)) {
                cm = ColorMappingList[[i]]
                if(side == "top") {
                    color_mapping_legend(cm, x = sum(cm_width[seq_len(i)]) + gap*(i-1), y = unit(1, "npc"), just = c("left", "top"), plot = TRUE, ...)
                } else {
                    color_mapping_legend(cm, x = sum(cm_width[seq_len(i)]) + gap*(i-1), y = unit(1, "npc") - padding[3], just = c("left", "top"), plot = TRUE, ...)
                }
            }

            if(length(annotation_legend_list)) {
                for(i in seq_along(annotation_legend_list)) {
                    if(side == "top") {
                        pushViewport(viewport(x = sum(cm_width[seq_len(n+i)] + gap*(n+i-1)), y = unit(1, "npc"), width = grobWidth(annotation_legend_list[[i]]), height = grobHeight(annotation_legend_list[[i]]), just = c("left", "top")))
                        grid.draw(annotation_legend_list[[i]])
                    } else {
                        pushViewport(viewport(x = sum(cm_width[seq_len(n+i)] + gap*(n+i-1)), y = unit(1, "npc") - padding[3], width = grobWidth(annotation_legend_list[[i]]), height = grobHeight(annotation_legend_list[[i]]), just = c("left", "top")))
                        grid.draw(annotation_legend_list[[i]])
                    }
                }
            }
        }
        
        size = unit.c(width, height)
    }

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
# == author
# Zuguang Gu <z.gu@dkfz.de>
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

compare_unit = function(u1, u2) {
    u1 = convertUnit(u1, "cm", valueOnly = TRUE)
    u2 = convertUnit(u2, "cm", valueOnly = TRUE)
    ifelse(u1 > u2, 1, ifelse(u1 < u2, -1, 0))
}
